#' Interpolate Path Curves
#'
#' Converts an SVG path in the format produced by [parse_path()] into pure x-y
#' coordinates by interpolating the Bezier curves.
#'
#' @export
#' @importFrom gridBezier BezierGrob BezierPoints nSteps
#' @importFrom grid unit convertX convertY
#' @seealso [parse_path()]
#' @param x a "subpath" S3 objects as produced by [parse_path()]
#' @param width numeric(1) or numeric(2) or NULL, the width of the device, used
#'   to normalize x coordinates into 0-1 range.  If two values are provided then
#'   they represent the x range.  If a single value is provided then the other
#'   end of the range is assumed to be zero.  If NULL is provided, the range is
#'   taken to be the range covered by the supplied coordinates.  No effort is
#'   made to compute the actual bounding box for any included Bezier curves.
#'   The bounding box is defined simply to contain the supplied points,
#'   including control points.
#' @param height like `width`, except for the y range.
#' @param normalize whether coordinates should be returned in 0-1 range.
#' @return a "subpath_xy" S3 object, which is just like a "subpath" object, but
#'   the coordinates are expressed purely as x-y values and should be
#'   interpreted as the vertices of a polygon or connected straight line
#'   segments.

interp_path <- function(
  x, steps=10, width=NULL, height=NULL, normalize=FALSE
) {
  vetr(
    structure(list(), class='subpath'),
    INT.1.POS,
    NULL || integer(1L) || integer(2L),
    NULL || integer(1L) || integer(2L),
    LGL.1
  )
  d <- x[['d']]
  if(is.null(width)) width <- range(d[['x']])
  if(is.null(height)) height <- range(d[['y']])
  if(length(width) == 1) width <- c(0, width)
  if(length(height) == 1) height <- c(0, height)

  # Normalize for npc use for GridBezier

  d[['x']] <- (d[['x']] - width[1]) / diff(width)
  d[['y']] <- (d[['y']] - height[1]) / diff(height)

  # C commands start one before the C command.

  brle <- with(d, rle(cmd == "C"))
  bends <- with(brle, cumsum(lengths)[values])
  bstarts <- with(brle, cumsum(lengths)[!values])[seq_along(bends)]

  # BezierGrob seems to open a display device...

  bzs <- Map(
    function(start, end) {
      points <- d[start:end, c('x', 'y')]
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
    function(start, end) d[start:end, c('x', 'y')],
    lstarts,
    lends
  )
  # Recombine

  els <- c(bzsp, lnsp)[order(c(bstarts, lstarts))]
  xvals <- unlist(lapply(els, '[[', 1))
  yvals <- unlist(lapply(els, '[[', 2))

  # Rescale

  if(!normalize) {
    xvals <- (xvals * diff(width)) + width[1]
    yvals <- (yvals * diff(height)) + height[1]
  }
  class(x) <- "subpath_xy"
  x[['d']] <- data.frame(x=xvals, y=yvals)
  x
}


