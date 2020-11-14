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
#' @param x an "svg_chopped" or "svg_chopped_list" object.
#' @param ask TRUE or FALSE whether to prompt to display more plots when
#'   plotting an "svg_chopped_list" with multiple elements that don't fit in the
#'   current plotting grid.
#' @param ppi numeric pixels per inch to assume for calculations.
#' @param ... passed on to [polypath()] and/or [lines()].
#' @return `x`, invisibly

plot.svg_chopped <- function(x, ppi=96, ...) plot_one(x, ppi, ...)

#' @export
#' @rdname plot.svg_chopped

plot.svg_chopped_flat <- function(x, ppi=96, ...) plot_one(x, ppi, ...)

#' @export
#' @rdname plot.svg_chopped

plot.svg_chopped_list <- function(
  x, ppi=96,
  ask = prod(par("mfcol")) < length(x) && dev.interactive(),
  ...
) {
  vetr(structure(list(), class='svg_chopped_list'), INT.1.POS.STR, LGL.1)
  plot_list(x, ppi=ppi, ask=ask, ...)
}
#' @export
#' @rdname plot.svg_chopped

plot.svg_chopped_list_flat <- function(
  x, ppi=96,
  ask = prod(par("mfcol")) < length(x) && dev.interactive(),
  ...
) {
  vetr(structure(list(), class='svg_chopped_list_flat'), INT.1.POS.STR, LGL.1)
  plot_list(x, ppi=ppi, ask=ask, ...)
}

#' Compute Display Parameters for Device
#'
#' Computes how the dimensions of the display device in user coordinates, the
#' aspect ratio of user coordinates relative to device coordinates, and the user
#' coordinates "ppi" (this is taken to be the higher of the X or Y ppi, which
#' may not be the same if the aspect ratio is not 1).
#'
#' This information can be used by a rendering function such as
#' [plot.svg_chopped()] to scale and position the output.
#'
#' @export
#' @param x an "svg_chopped" object
#' @param pin numeric length 2 width and height of the plot area in inches.
#' @param ppi numeric device resolution in pixels per inch; if rendered SVGs
#'   look larger or smaller than in your browser you may need to adjust this
#'   setting.
#' @param scale TRUE (currently unsupported) or FALSE (default) whether units
#'   should be scaled to fit the viewbox in the display.  Aspect ratio will be
#'   preserved.
#' @return a list containing elements:
#' * "plot.lim": a list with the x and y plot limits in user coordinates.
#' * "asp": numeric(1) the height/width ratio of each pixel in the plot (not the
#'   actual aspect ratio of the plot).
#' * "uppi": numeric(1) the user-pixels per inch.

compute_display_params <- function(x, pin=par('pin'), ppi=96, scale=FALSE) {
  vetr(
    structure(list(), class='svg_chopped') ||
    structure(list(), class='svg_chopped_flat'),
    pin=numeric(2) && all(. > 0), scale=LGL.1, ppi=INT.1.POS.STR
  )
  # Compute viewport width and height in pixels
  vp.width <- attr(x, 'width')
  vp.height <- attr(x, 'height')
  vp.both <- FALSE
  vp.pct <- attr(x, 'wh.pct')
  if(is.null(vp.pct)) vp.pct <- c(width=FALSE, height=FALSE)
  if(is.na(vp.width)) {
    vp.width <- 100
    vp.pct['width'] <- TRUE
  }
  if(is.na(vp.height)) {
    vp.height <- 100
    vp.pct['height'] <- TRUE
  }
  if(vp.pct['width']) {
    vp.width <- vp.width / 100 * ppi * pin[1]
  }
  if(vp.pct['height']) {
    vp.height <- vp.height / 100 * ppi * pin[2]
  }
  # Based on most constrained dimension, compute display pixels to user pixels
  vpp.to.usrp <- 1   # ratio of viewport pixels to viewbox pixels

  # Aspect ratio is only 1, unless preserveAspectRatio is not 'meet', and we
  # don't currenlty support anything other than meet.
  asp <- 1
  uppi <- ppi
  dev.width <- lim.width <- pin[1] * ppi
  dev.height <- lim.height <- pin[2] * ppi

  # viewbox info
  vb <- compute_vb_dim(x)

  if(vb$has.vb) {
    if(vp.height / vp.width > vb$height / vb$width) {
      uppi <- ppi * vb$width / vp.width
      lim.width <- vb$width
      lim.height <- vb$height * (vp.height / vp.width) / (vb$height / vb$width)
    } else {
      uppi <- ppi * vb$height / vp.height
      lim.height <- vb$height
      lim.width <- vb$width * (vb$height / vb$width) / (vp.height / vp.width)
    }
  }
  # Figure out the actul plottable area as the viewport may not fit in the
  # display window, unless scale is TRUE (do we need to use ASP here?)

  list(
    plot.lim=list(x=c(vb$x, vb$x + lim.width), y=c(vb$y, vb$y + lim.height)),
    asp=asp,
    uppi=uppi  # for stroke width calcs
  )
}
compute_vb_dim <- function(x) {
  # Start with viewBox width and height
  vb <- attr(x, 'viewBox')
  extents <- attr(x, 'extents')
  has.vb <- FALSE

  if(
    !is.null(vb) && is.numeric(vb) && !anyNA(vb) && length(vb) == 4 &&
    all(vb[3:4] > 0)
  ) {
    x0 <- vb[1]
    y0 <- vb[2]
    vb.width <- vb[3]
    vb.height <- vb[4]
    has.vb <- TRUE
  } else if(
    !is.null(extents) &&
    isTRUE(vet(list(numeric(2), numeric(2)), extents))
  ) {
    vb.width <- diff(extents[[1]])
    vb.height <- diff(extents[[2]])
    x0 <- extents[[1]][1]
    y0 <- extents[[2]][1]
  } else stop("Dimensions corrupted.")

  list(width=vb.width, height=vb.height, x=x0, y=y0, has.vb=has.vb)
}

## Internal: plot either normal or flat chopped_list

plot_list <- function(x, ppi, ask, ...) {
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  lapply(x, plot, ppi=ppi, ...)
  invisible(x)
}
## Internal: plot either normal or flat chopped

plot_one <- function(x, ppi, ...) {
  url <- attr(x, 'url')   # gradients, patterns, etc., stored here
  old.par <- par(xaxs='i', yaxs='i')
  on.exit(par(old.par))

  # Compute plot dimensions in user units using viewBox info if availble, and if
  # not from the pre-computed extents attributes

  plot.new()
  d.params <- compute_display_params(x, ppi=ppi)
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
    # Scale stroke width to account for user coordinates and transforms
    stroke.width <-
      stroke.width *
      # user - display conversion
      ppi / d.params[['uppi']] * 96 / ppi *
      # approximate scaling from transformation
      mean(abs(attr(mat, 'transform-computed')[['mx']][c(1,5)]))

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

