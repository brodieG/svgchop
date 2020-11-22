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
#' Stroke widths are computed under the assumption that 1 "lwd" == 1/96th of an
#' inch, and may look bad on devices that don't support "lwd" values less than
#' one.  Future versions may switch to `grid` functions and take advantage of
#' viewports, gradients, patterns, etc.
#'
#' @export
#' @inheritParams graphics::par
#' @seealso [chop()] in particular the "Unsupported Features" section,
#'   [graphics::par()] for other general graphical settings,
#'   [graphics::polypath()], [graphics::lines()], for how the polygons and
#'   their line strokes are drawn, [compute_display_params()] for how
#'   the SVG is sized, [as.svg_chopped_list()] to convert a list of
#'   "svg_chopped" objects into a plottable version of it, [svg_samples()] for
#'   sample SVG files that ship with this package.
#' @param x an "svg_chopped" or "svg_chopped_list" object.
#' @param ask NULL (default) TRUE or FALSE whether to prompt to display more
#'   plots when plotting an "svg_chopped_list" with multiple elements that don't
#'   fit in the current plotting grid.  If NULL, auto-detects based on how many
#'   elements there are to draw and whether they fit in the grid implied by
#'   `prod(par('mfrow'))`.
#' @param ppi numeric pixels per inch of the display device to assume for
#'   calculations, defaults to 125 which is a common display density for retina
#'   style displays (note these are "display" pixels, not screen pixels which
#'   are ~2x denser).
#' @param scale TRUE or FALSE (default), whether to force the SVG to scale to
#'   the display device.  If true, the image is adjusted to fill as much of the
#'   device as possible while respecting the original aspect ratio.  This causes
#'   the "viewBox", "width", and "height" attributes of the original SVG to be
#'   ignored.
#' @param center TRUE (default)  or FALSE, whether to center the image on the
#'   device.
#' @param ... used to set graphical parameters with [graphics::par()].
#' @return `x`, invisibly
#' @examples
#' \donttest{
#' svg <- chop(R_logo())
#' plot(svg)
#' plot(svg, ppi=75)       # fat pixels
#' plot(svg, scale=TRUE)   # fit to device
#'
#' ## Plot multiple svgs at once (chop_all is for all SVGS in
#' ## a single XML doc, not multiple SVG docs)
#' svgs <- as.svg_chopped_list(lapply(svg_samples()[1:4], chop))
#' plot(svgs, mfrow=c(2,2), mai=rep(.1, 4), scale=TRUE)
#' }

plot.svg_chopped <- function(
  x, ppi=getOption('svgchop.ppi', 125), scale=FALSE, center=TRUE,
  xaxs='i', yaxs='i', mai=numeric(4), ...
)
  plot_list(
    list(x), ppi=ppi, scale=scale, center=center, ask=FALSE,
    xaxs=xaxs, yaxs=yaxs, mai=mai, ...
  )

#' @export

plot.svg_chopped_flat <- function(
  x, ppi=getOption('svgchop.ppi', 125), scale=FALSE, center=TRUE,
  xaxs='i', yaxs='i', mai=numeric(4), ...
)
  plot_list(
    list(x), ppi=ppi, scale=scale, center=center, ask=FALSE,
    xaxs=xaxs, yaxs=yaxs, mai=mai, ...
  )

#' @export

plot.svg_chopped_list <- function(
  x, ppi=getOption('svgchop.ppi', 125), scale=FALSE, center=TRUE,
  ask = NULL, xaxs='i', yaxs='i', mai=numeric(4), ...
) {
  vetr(structure(list(), class='svg_chopped_list'), INT.1.POS.STR, LGL.1)
  plot_list(
    x, ppi=ppi, scale=scale, center=center, ask=ask,
    xaxs=xaxs, yaxs=yaxs, mai=mai, ...
  )
}
#' @export

plot.svg_chopped_list_flat <- function(
  x, ppi=getOption('svgchop.ppi', 125), scale=FALSE, center=TRUE,
  ask = NULL, xaxs='i', yaxs='i', mai=numeric(4), ...
) {
  vetr(structure(list(), class='svg_chopped_list_flat'), INT.1.POS.STR, LGL.1)
  plot_list(
    x, ppi=ppi, scale=scale, center=center, ask=ask,
    xaxs=xaxs, yaxs=yaxs, mai=mai, ...
  )
}

#' Compute SVG Display Parameters given a Device
#'
#' Relates SVG size to the device by accounting for device physical size given
#' dimensions in inches and PPI per resolution.  SVGs that do not specify
#' "width" or "height" will be scaled such that the "viewBox" fits the device
#' while preserving aspect ratio.  Otherwise the "viewBox" is scaled to fit the
#' "width" and "height", themselves measured in device units.  If there is no
#' "viewBox" the extents are scaled directly against device dimensions.
#'
#' This information can be used by a rendering function such as
#' [plot.svg_chopped()] to scale and position the output.  The key values are
#' "plot.lim" which represent the user-space coordinate range of the device
#' (i.e. what you would use as the `xlim` and `ylim` parameters to
#' [plot.window()]), and "uppi" which is the user pixels per display device inch
#' resolution (i.e. there are uppi/ppi user pixels per display pixel).
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
#' * "vb": list of parsed "viewBox" parameters which may be computed from other
#'   values if it is missing; "has.vb" sub-element denotes whether the values
#'   were auto-computed (FALSE) or parsed from the "viewBox" element (TRUE).

compute_display_params <- function(
  x, pin=par('pin'), ppi=getOption('svgchop.ppi', 125), center=TRUE
) {
  vetr(
    structure(list(), class='svg_chopped') ||
    structure(list(), class='svg_chopped_flat'),
    pin=numeric(2) && all(. > 0), ppi=INT.1.POS.STR,
    center=LGL.1
  )
  dev.width <- lim.width <- pin[1] * ppi
  dev.height <- lim.height <- pin[2] * ppi

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
    vp.width <- vp.width / 100 * dev.width
  }
  if(vp.pct['height']) {
    vp.height <- vp.height / 100 * dev.height
  }
  # Aspect ratio is only 1, unless preserveAspectRatio is not 'meet', and we
  # don't currently support anything other than meet.
  asp <- 1
  uppi <- ppi

  # There is the device, the viewport, and the viewbox.
  #
  # If there is a viewbox, it will be mapped to fit the viewport.  Then, those
  # coordinates are related to the device window.

  vb <- compute_vb_dim(x)
  if(vb$has.vb) {
    if(vp.height / vp.width > vb$height / vb$width) {
      uppi <- ppi * vb$width / vp.width
    } else {
      uppi <- ppi * vb$height / vp.height
    }
  }
  w.off <- (dev.width - vb$width * ppi / uppi) / 2 * center
  h.off <- (dev.height - vb$height * ppi / uppi) / 2 * center
  x0 <- vb$x - w.off
  y0 <- vb$y - h.off

  list(
    plot.lim=list(
      x=c(x0, x0 + dev.width),
      y=c(y0, y0 + dev.height)
    ),
    asp=asp,
    uppi=c(uppi),  # ppi/uppi is userspace to device scaling ratio
    vb=vb
  )
}
vb_from_extents <- function(x) {
  ext <- attr(x, 'extents')
  if(!is.null(ext)) {
    c(ext[['x']][1], ext[['y']][1], diff(ext[['x']]), diff(ext[['y']]))
  } else rep(NA_real_, 4)
}
# Parse viewBox falling back to extents

compute_vb_dim <- function(x) {
  # Start with viewBox width and height
  vb <- attr(x, 'viewBox')
  extents <- attr(x, 'extents')
  has.vb <- FALSE

  if(
    !is.null(vb) && is.numeric(vb) && !anyNA(vb) && length(vb) == 4 &&
    all(vb[3:4] > 0)
  ) {
    has.vb <- TRUE
  } else if(
    !is.null(extents) &&
    isTRUE(vet(list(numeric(2), numeric(2)), extents))
  ) {
    vb <- vb_from_extents(x)
  } else stop("Dimensions corrupted.")

  c(as.list(setNames(vb, c('x', 'y', 'width', 'height'))), list(has.vb=has.vb))
}

## Internal: plot either normal or flat chopped_list

plot_list <- function(
  x, ppi, scale=FALSE, center=TRUE, ask, xaxs='i', yaxs='i', mai=numeric(4),
  ...
) {
  dots <- list(...)
  old.par <- par(c(list(xaxs=xaxs, yaxs=yaxs, mai=mai), dots))
  on.exit(par(old.par))
  if(is.null(ask)) {
    ask <- prod(par("mfrow")) < length(x) && dev.interactive()
  }
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  lapply(x, plot_one, ppi=ppi, scale=scale, center=center)
  invisible(x)
}
## Internal: plot either normal or flat chopped

plot_one <- function(
  x, ppi, scale=FALSE, center=TRUE
) {
  url <- attr(x, 'url')   # gradients, patterns, etc., stored here
  extents <- attr(x, 'extents')

  # Compute plot dimensions in user units using viewBox info if availble, and if
  # not from the pre-computed extents attributes
  plot.new()
  if(scale) {
    attr(x, 'viewBox') <- vb_from_extents(x)
    attr(x, 'width') <- NA_real_
    attr(x, 'height') <- NA_real_
  }
  d.params <- compute_display_params(x, ppi=ppi, center=center)
  lim <- d.params[['plot.lim']]
  plot.window(lim[['x']], rev(lim[['y']]), asp=d.params[['asp']])

  # Flatten makes it easier to iterate through structure, but also removes all
  # "hidden elements" so they are not plotted
  mats <- if(!inherits(x, 'svg_chopped_flat')) flatten(x) else x
  for(i in seq_along(mats)) {
    cat(sprintf("Plotting %d/%d\r", i, length(mats)))
    mat <- mats[[i]]
    style <- attr(mat, 'style-computed')
    fill <- stroke <- stroke.width <- NA
    fill.rule <- 'winding'
    if(!is.null(style)) {
      # Fill could be specified via `url(#id)` so we need to translate that if
      # possible into something the device can recognize.  More complex logic
      # would be required to specify patterns, or the actual gradient, etc.
      fill <- approximate_color(style[['fill']], url)
      stroke <- approximate_color(style[['stroke']], url)

      #It's possible to get opacity back from approximation
      fill.op <- attr(fill, 'opacity')
      fill.op <- if(is.null(fill.op)) style[['fill-opacity']]
      else style[['fill-opacity']] * fill.op
      stroke.op <- attr(stroke, 'opacity')
      stroke.op <- if(is.null(stroke.op)) style[['stroke-opacity']]
      else style[['stroke-opacity']] * stroke.op

      # Here we apply alpha by generating 8 char hex codes (e.g. #FFFFFFCC)
      fill <- append_alpha(fill, fill.op)
      stroke <- append_alpha(stroke, stroke.op)

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

    vb <- d.params[['vb']]
    offset <- c(vb[['x']], vb[['y']])
    if(ncol(mat) > 1) {
      m <- t((mat - offset) * ppi / d.params[['uppi']] + offset)
      if(!is.na(fill)) polypath(m, col=fill, border=NA, rule=fill.rule)
      if(!is.na(stroke)) lines(m, col=stroke, lwd=stroke.width)
    }
  }
  cat(
    paste0(
      c(rep(" ", 10 + ceiling(log(length(mats), 10)) * 2), "\r"),
      collapse=""
  ) )
  invisible(x)
}

