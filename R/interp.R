#' Interpolate a single bezier curve
#'
#' Thin wrapper around `BezierGrob` / `BezierPoints`, but ensures result comes
#' out in npc units.  `bezier_interp_even` returns "evenly" spaced points.  It
#' works by subdividing the bezier according to the `t` parameter in `mult` as
#' many segments as `steps` provided, and then interpolates along the resulting
#' piecewise linear (yes, a nasty hack).
#'
#' @export
#' @param coords a list with x and y coords only in that order.
#' @param steps integer(1) how many equal size steps to interpolate along.
#' @param mult how many more times to subivide per `steps` when looking for an
#'   even interpolation.
#' @param dev whether to spawn a new device (an open device is needed for
#'   conversions to/from NPC to work.
#' @return a list of coordinates in NPC (though the units are stripped) of the
#'   points along the input curve.

bezier_interp <- function(coords, steps=10, dev=TRUE) {
  vetr(list(x=numeric(), y=numeric()), INT.1.POS.STR, LGL.1)
  if(dev) {
    dev.new()
    on.exit(dev.off())
  }
  bzs <- BezierGrob(coords[[1]], coords[[2]], stepFn=nSteps(steps))

  # Can't figure out how to get BezierPoints to not return in inches, so
  # converting back to NPC manually, which is a real hack.

  bzsp <- BezierPoints(bzs)
  bzspi <- lapply(bzsp, unit, 'inches')
  list(c(convertX(bzspi[[1]], 'npc')), c(convertY(bzspi[[2]], 'npc')))
}
#' @rdname bezier_interp
#' @export

bezier_interp_even <- function(coords, steps=10, mult=10, dev=TRUE) {
  vetr(mult=INT.1.POS.STR)
  res <- bezier_interp(coords, steps=steps * mult, dev=dev)
  xd <- diff(res[[1]])
  yd <- diff(res[[2]])
  lens <- c(0, cumsum(sqrt(xd^2+yd^2)))
  target <- seq(0, lens[length(lens)], length.out=steps)
  interval <- findInterval(target, lens, rightmost.closed=TRUE)
  low <- lens[interval]
  high <- lens[interval + 1]
  interp <- (target - low) / (high - low)
  list(
    res[[1]][interval] + xd[interval] * interp,
    res[[2]][interval] + yd[interval] * interp
  )
}
#' Interpolate Path Curves
#'
#' Converts an SVG paths in the format produced by [parse_svg)] into pure x-y
#' coordinates by interpolating the Bezier curves, if any.  Due to how
#' `gridBezier` works this requires spawning a new device.
#'
#' @export
#' @importFrom gridBezier BezierGrob BezierPoints nSteps
#' @importFrom grid unit convertX convertY
#' @seealso [parse_paths()]
#' @param x a "svg_paths" S3 object as produced by [parse_paths()]
#' @param box integer(4) or NULL, the x and y offset of the SVG display box, and
#'   the width and height of that box, in that order.  If NULL these values will
#'   be those that define the "bounding" box of the coordinates in `x`, although
#'   The bounding box is defined simply to contain the supplied points,
#'   including control points.
#' @param normalize whether coordinates should be returned in 0-1 range.  If the
#'   input range is not square the longest dimension will span 0-1 and the
#'   shorter one whatever range preserves the aspect ratio, centered at 0.5.
#' @return a "svg_paths_xy" S3 object, which is like a "svg_paths" object, but
#'   the coordinates are expressed purely as x-y values and should be
#'   interpreted as the vertices of a polygon or connected straight line
#'   segments.

interp_paths <- function(x, steps=10, box=NULL, normalize=FALSE) {
  vetr(
    structure(list(), class='svg_paths'),
    INT.1.POS,
    NUM.POS && length(.) == 4 || NULL,
    LGL.1
  )
  if(is.null(box) && is.null(attr(x, 'box'))) {
    coords.all <- do.call(
      rbind, unlist(lapply(x, '[[', 'coords'), recursive=FALSE)
    )
    x.rng <- range(coords.all[['x']])
    y.rng <- range(coords.all[['y']])
    box <- c(x.rng[1], y.rng[1], diff(x.rng), diff(y.rng))
  } else if(is.null(box)) {
    box <- attr(x, 'box')
  }
  # Apply interpolation

  dev <- dev.new()
  on.exit(dev.off())
  interp <- lapply(
    x,
    function(y) {
      y[['coords']] <- lapply(y[['coords']], interp_path, steps, box, normalize)
      y
  } )
  # Collapse sub-paths into one DF, tracking the start points of each

  paths_xy <- lapply(
    interp,
    function(y) {
      starts <- cumsum(
        vapply(y[['coords']], nrow, numeric(1L))
      )[-length(y[['coords']])] + 1L
      coords <- do.call(rbind, y[['coords']])
      attr(coords, 'starts') <- starts
      y[['coords']] <- coords
      y
    }
  )
  structure(paths_xy, class='svg_paths_xy', box=box)
}

interp_path <- function(x, steps, box, normalize) {
  d <- x
  scale  <- max(box[3:4])

  # Normalize for npc use for GridBezier

  d[['x']] <- (
    d[['x']] - box[1] + (scale -  box[3]) / 2
  ) / scale
  d[['y']] <- (
    d[['y']] - box[2] + (scale -  box[4]) / 2
  ) / scale

  # C commands start one before the C command.

  brle <- with(d, rle(cmd == "C"))
  bends <- with(brle, cumsum(lengths)[values])
  bstarts <- with(brle, cumsum(lengths)[!values])[seq_along(bends)]

  # BezierGrob seems to open a display device...

  bzsp <- Map(
    function(start, end)
      bezier_interp(lapply(d[c('x', 'y')], '[', start:end), steps, dev=FALSE),
    bstarts,
    bends
  )
  # connect with the line segments

  lrle <- with(d, rle(cmd == "L"))
  lends <- with(lrle, cumsum(lengths)[values])
  lstarts <- with(lrle, cumsum(lengths)[!values][seq_along(lends)])

  lnsp <- Map(
    function(start, end) lapply(d[c('x', 'y')], '[', start:end),
    lstarts,
    lends
  )
  # Recombine

  els <- c(bzsp, lnsp)[order(c(bstarts, lstarts))]
  xvals <- unlist(lapply(els, '[[', 1))
  yvals <- unlist(lapply(els, '[[', 2))

  # Rescale

  if(!normalize) {
    xvals <- (xvals * scale) + box[1] - ((scale -  box[3]) / 2)
    yvals <- (yvals * scale) + box[2] - ((scale -  box[4]) / 2)
  }
  data.frame(x=xvals, y=yvals)
}


