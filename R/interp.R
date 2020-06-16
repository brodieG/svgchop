#' Interpolate Path Curves
#'
#' Converts an SVG paths in the format produced by [parse_paths()] into pure x-y
#' coordinates by interpolating the Bezier curves.
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
    coords.all <- do.call(rbind, unlist(lapply(x, '[[', 'd'), recursive=FALSE))
    x.rng <- range(coords.all[['x']])
    y.rng <- range(coords.all[['y']])
    box <- c(x.rng[1], y.rng[1], diff(x.rng), diff(y.rng))
  } else if(is.null(box)) {
    box <- attr(x, 'box')
  }
  # Apply interpolation

  interp <- lapply(
    x,
    function(y) {
      y[['d']] <- lapply(y[['d']], interp_path, steps, box, normalize)
      y
  } )
  # Collapse sub-paths into one DF, tracking the start points of each

  paths_xy <- lapply(
    interp,
    function(y) {
      starts <-
        cumsum(vapply(y[['d']], nrow, numeric(1L)))[-length(y[['d']])] + 1L
      coords <- do.call(rbind, y[['d']])
      attr(coords, 'starts') <- starts
      y[['d']] <- coords
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

  bzs <- Map(
    function(start, end) {
      points <- lapply(d[c('x', 'y')], '[', start:end)
      BezierGrob(points[[1]], points[[2]], stepFn=nSteps(steps))
    },
    bstarts,
    bends
  )
  # Can't figure out how to get BezierPoints to not return in inches, so
  # converting back to NPC manually, which is a real hack.

  bzsp <- lapply(bzs, BezierPoints)
  bzspi <- lapply(bzsp, lapply, unit, 'inches')
  bzsp <- lapply(
    bzspi, function(x) list(convertX(x[[1]], 'npc'), convertY(x[[2]], 'npc'))
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


