# Copyright (C) 2020 Brodie Gaslam
#
# This file is part of "svgchop - Approximate SVG Elements With Line Segments"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Plot Method for "svg_chopped" Objects
#'
#' Display an SVG plot object to the graphical device.  Renders closed polygons
#' with [graphics::polypath()] and open ones with [graphics::lines()].  Will set
#' the viewport to the SVG viewBox coordinates if they exist, and to the SVG
#' extent if they don't.  Aspect ratio is fixed so that user coordinates are 1:1
#' in x and y directions.  The "width", "height", "x", and "y" attributes of the
#' SVG element proper are ignored.
#'
#' Nested SVGs viewports will not be respected and their contents will be drawn
#' in the outer viewport coordinates.  Maybe, this is untested.
#'
#' Stroke widths are computed under the assumption that 1 "lwd" == 1/96th of an
#' inch, and may look bad on devices that don't support "lwd" values less than
#' one.
#'
#' The "svg_chopped_list" method will call the "svg_chopped" method for each
#' item, passing along the "url-data" object that is attached as the "url"
#' attribute of the "svg_chopped_list" object.  If you set `par(mfrow=...)` or
#' similar each element of the list will be plotted in its own grid spot.
#'
#' Future versions may switch to `grid` functions and take advantage of
#' viewports and the implementation of gradients, etc.
#'
#' @export
#' @seealso [process_svg()]
#' @inheritParams stats::plot.lm
#' @param x an "svg_chopped" or "svg_chopped_list" object
#' @param ... passed on to [polypath()] and/or [lines()].
#' @return `x`, invisibly

plot.svg_chopped <- function(x, ...) plot_one(x, ...)

#' @export
#' @rdname plot.svg_chopped

plot.svg_chopped_flat <- function(x, ...) plot_one(x, ...)

#' @export
#' @rdname plot.svg_chopped

plot.svg_chopped_list <- function(
  x,
  ask = prod(par("mfcol")) < length(x) && dev.interactive(),
  ...
) {
  vetr(structure(list(), class='svg_chopped_list'), LGL.1)
  plot_list(x, ask, ...)
}
#' @export
#' @rdname plot.svg_chopped

plot.svg_chopped_list_flat <- function(
  x,
  ask = prod(par("mfcol")) < length(x) && dev.interactive(),
  ...
) {
  vetr(structure(list(), class='svg_chopped_list_flat'), LGL.1)
  plot_list(x, ask, ...)
}

#' Viewport Data
#'
#' The width and height of the viewbox
#' The width and height of the SVG
#' The width and height of the device window
#' 
#' Device window does not affect anything unless in scale mode.  We could infer
#' this from width and height specified as %.  Or just let the user specify.
#'
#' Width and height of the viewBox relative to the SVG will affect the aspect
#' ratio.  We should probably use the largest user pixels per lwd ratio of the
#' width and height when ASP is not 1.
#' 
#' Cases:
#'
#' * Adjust for scaling
#' * SVG width / height 
#'     * in pixels
#'     * in percent
#'     * in units (assume pixels)
#'     * mixed
#'     * unspecified means user pixels are display pixels
#'     * if in percent, convert to pixels
#'     * if both width/height unspecified, use user pixels
#'     * if one is specified fit using user aspect ratio
#'
#'     * whatever it is, assuming 96ppi, 
#'
#' * SVG width / height in percent
#' * 
#' 
#'
#' @export
#' @param x an "svg_chopped" object
#' @param pin numeric length 2 width and height of the plot area in inches.
#' @param scale TRUE or FALSE (default) whether units should be scaled to fit
#'   the viewbox in the display.  Aspect ratio will be preserved.
#' @param integer how many "pixels" per display inch we should assume.
#' @return numeric vector containing:
#' * aspect ratio
#' * user points per inch
#' * displayable user point ranges

compute_display_params <- function(x, pin=par('pin'), scale=FALSE, ppi=96) {
  vetr(
    structure(list(), class='svg_chopped'),
    pin=numeric(2) && all(. > 0), scale=LGL.1, ppi=INT.1.POS.STR
  )
  # Start with viewBox width and height
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

  # Compute viewport width and height in pixels
  vp.width <- attr(x, 'width')
  vp.height <- attr(x, 'height')

  if(is.na(vp.width) && is.na(vp.height)) {
    vp.width <- width
    vp.height <- height
  } else if (is.na(vp.width) && !is.na(vp.height)) {
    if(isTRUE(attr(x, 'wh.pct')['height'])) {
      vp.height <- vp.height / 100 * ppi * pin[2]
    }
    vp.width <- vp.height / height * width
  } else if (is.na(vp.height) && !is.na(vp.width)) {
    if(isTRUE(attr(x, 'wh.pct')['width'])) {
      vp.width <- vp.width / 100 * ppi * pin[1]
    }
    vp.height <- vp.width / width * height
  } else {
    if(isTRUE(attr(x, 'wh.pct')['height']))
      vp.height <- vp.height / 100 * ppi * pin[2]
    if(isTRUE(attr(x, 'wh.pct')['width']))
      vp.width <- vp.width / 100 * ppi * pin[1]
  }
  # Based on most constrained dimension, compute disply pixels to user pixels
  vpp.to.usrp <- 1   # ratio of viewport pixels to viewbox pixels
  asp <- 1
  if(is.finite(vp.width) && is.finite(vp.height)) {
    # this could be min too, or averge, need to pick something
    vpp.to.usrp <- max(c(vp.width / width, vp.height / height))
    asp <- vp.height / vp.width
  } else if (is.finite(vp.width)) {
    vpp.to.usrp <- vp.width / width
  } else if (is.finite(vp.height)) {
    vpp.to.usrp <- vp.height / height
  } else stop("Internal Error: both dimensions infinite for viewport.")

  # Figure out the actul plottable area as the viewport may not fit in the
  # display window, unless scale is TRUE (do we need to use ASP here?)

  width.p <- width * min(c(1, pin[1] * ppi / vp.width))
  height.p <- height * min(c(1, pin[2] * ppi / vp.height))

  list(
    plot.lim=list(x=c(x0, x0 + width.p), y=c(y0, y0 + height.p)),
    asp=asp, pixel.ratio=vpp.to.usrp
  )
}
## Internal: plot either normal or flat chopped_list

plot_list <- function(x, ask, ...) {
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  lapply(x, plot, ...)
  invisible(x)
}
## Internal: plot either normal or flat chopped

plot_one <- function(x, ...) {
  url <- attr(x, 'url')   # gradients, patterns, etc., stored here
  old.par <- par(xaxs='i', yaxs='i')
  on.exit(par(old.par))

  # Compute plot dimensions in user units using viewBox info if availble, and if
  # not from the pre-computed extents attributes

  plot.new()
  d.params <- compute_display_params(x)
  lim <- d.params[['plot.lim']]
  plot.window(lim[['x']], rev(lim[['y']]), asp=d.params[['asp']])

  # Flatten makes it easier to iterate through structure, but also removes all
  # "hidden elements" so they are not plotted
  mats <- if(!inherits(x, 'svg_chopped_flat')) flatten(x) else x
  for(i in seq_along(mats)) {
    mat <- mats[[i]]
    style <- attr(mat, 'style-computed')
    fill <- stroke <- stroke.width <- NA
    fill.rule <- 'winding'
    if(!is.null(style)) {
      # Fill could be specified via `url(#id)` so we need to translate that if
      # possible into something the device can recognize.  More complex logic
      # would be required to specify patterns, or the actual gradient, etc.
      fill <- approximate_fill(style[['fill']], url)
      stroke <- style[['stroke']]

      # Here we apply alpha by generating 8 char hex codes (e.g. #FFFFFFCC)
      fill <- append_alpha(fill, style[['fill-opacity']])
      stroke <- append_alpha(stroke, style[['stroke-opacity']])

      stroke.width <- style[['stroke-width']]
      fill.rule <- c(evenodd='evenodd', nonzero='winding')[style[['fill-rule']]]
      if(is.na(fill.rule)) fill.rule <- 'winding'
    }
    stroke.width <- stroke.width * d.params[['pixel.ratio']]

    # Retrieve `closed` attribute which designates which sub-paths are closed.
    # We don't use it here but in the SVG spec it changes how the line butts are
    # displayed when the start and end-point overlap.
    closed <- all(attr(mat, 'closed'))

    # Split sub-paths using the "starts" attribute.  Sub-paths may create holes
    # in the polygons.
    if(is.numeric(attr(mat, 'starts'))) {
      idx <- rep(
        seq_len(ncol(mat)),
        seq_len(ncol(mat)) %in% (attr(mat, 'starts') - 1) + 1
      )
      idx[duplicated(idx)] <- NA
      mat <- mat[,idx]
    }
    # We use polypath for the fill, and lines for the stroke; whether a polygon
    # looks closed or not will be a matter of the lines forming a closed
    # outline.

    if(ncol(mat) > 1) {
      m <- t(mat)
      polypath(m, col=fill, border=NA, rule=fill.rule, ...)
      lines(m, col=stroke, lwd=stroke.width, ...)
    }
  }
  invisible(x)
}

