flatten <- function(x) {
  res <- list()
  frec <- function(y) {
    if(is.list(y)) {
      lapply(y, frec)
    } else {
      res <<- c(res, list(y))
    }
    invisible(NULL)
  }
  frec(x)
  res
}
#' Plot Method for "svg_chopped" Objects
#'
#' Display an SVG plot object to the graphical device.  Renders closed polygons
#' with [graphics::polypath()] and open ones with [graphics::lines()].  Will set
#' the viewport to the SVG viewBox coordinates if they exist, and to the SVG
#' extent if they don't.  Aspect ratio is fixed so that user coordinates are 1:1
#' in x and y directions.
#'
#' Nested SVGs viewports will not be respected and their contents will be drawn
#' in the outer viewport coordinates.
#'
#' Stroke widths are computed under the assumption that 1 "lwd" == 1/96th of an
#' inch, and may look bad on devices that don't support "lwd" values less than
#' one.
#'
#' @export
#' @param x an "svg_chopped" object
#' @param ... unused
#' @return `x`, invisibly

plot.svg_chopped <- function(x, ...) {
  old.par <- par(xaxs='i', yaxs='i')
  on.exit(par(old.par))

  plot.new()

  vb <- attr(x, 'viewBox')
  extents <- attr(x, 'extents')
  if(
    !is.null(vb) && is.numeric(vb) && !anyNA(vb) && length(vb) == 4 &&
    all(vb[3:4] > 0)
  ) {
    x0 <- vb[1]
    y0 <- vb[2]
    width <- vb[3]
    height <- vb[4]
  } else if(
    !is.null(extents) &&
    isTRUE(vet(list(numeric(2), numeric(2)), extents))
  ) {
    width <- diff(extents[[1]])
    height <- diff(extents[[2]])
    x0 <- extents[[1]][1]
    y0 <- extents[[2]][1]
  } else stop("Dimensions corrupted.")

  asp <- height / width
  plot.new()
  plot.window(c(x0, x0 + width), c(y0 + height, y0), asp=asp)

  # lwd = 1 taken to be 1/96th of an inch

  pin <- par('pin')
  if(width > height) {
    if(pin[1] > pin[2]) {
      ppi <- width / pin[1]
    } else {
      ppi <- height / pin[2]
    }
  } else{
    if(pin[1] < pin[2]) {
      ppi <- height / pin[2]
    } else {
      ppi <- width / pin[1]
    }
  }
  ptolwd <- ppi / 96

  mats <- flatten(x)
  for(i in seq_along(mats)) {
    mat <- mats[[i]]
    style <- attr(mat, 'style-computed')
    fill <- stroke <- NA
    stroke.width <- 1
    if(!is.null(style)) {
      fill <- if(!is.na(style[['fill']])) style[['fill']] else NA
      stroke <- if(!is.na(style[['stroke']])) style[['stroke']] else NA
      if(
        !is.na(fill) && !is.null(style[['fill-opacity']]) &&
        !is.na(style[['fill-opacity']])
      ) {
        fill <- paste0(fill, as.hexmode(round(style[['fill-opacity']] * 255)))
      }
      if(
        !is.na(stroke) && !is.null(style[['stroke-opacity']]) &&
        !is.na(style[['stroke-opacity']])
      ) {
        stroke <- paste0(stroke, as.hexmode(style[['stroke-opacity']] * 255))
      }
      if(!is.na(style[['stroke-width']]))
        stroke.width <- as.numeric(sub("\\D+$", "", style[['stroke-width']]))
    }
    stroke.width <- stroke.width / ptolwd

    # Are all paths closed (note lose attributes after starts business)
    closed <- all(attr(mat, 'closed'))

    # Handle holes / sub-paths by adding NAs
    if(is.numeric(attr(mat, 'starts'))) {
      idx <- rep(
        seq_len(ncol(mat)),
        seq_len(ncol(mat)) %in% (attr(mat, 'starts') - 1) + 1
      )
      idx[duplicated(idx)] <- NA
      mat <- mat[,idx]
    }
    # plot

    if(closed) polypath(t(mat), col=fill, border=stroke, lwd=stroke.width)
    else lines(t(mat), col=stroke, lwd=stroke.width)
  }
  invisible(x)
}
