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

#' Tools to Compare SVG Renderings
#'
#' `compare_svg` juxtaposes an SVG file to the corresponding [chop()]ed and
#' rasterized (via [plot.svg_chopped()]) version.  `compare_rsvg` does the same,
#' except the reference SVG is rasterized with [rsvg::rsvg_png()].  For the
#' latter we also show the mean absolute difference for each pixel across
#' channels as a monochrome image.  Reference renderings are on the left.  Both
#' these functions default to the built-in svgs listable via `svg_samples`.
#'
#' @export
#' @importFrom xml2 `xml_attr<-` write_xml
#' @importFrom grDevices png dev.off
#' @importFrom utils browseURL
#' @inheritParams plot.svg_chopped
#' @param files character vector of paths or URLs for SVGs to compare.
#' @param target a file to use as a directory to contain all the elements
#'   required for the output webpage, or NULL.  If NULL (default) a temporary
#'   file will be used.  If specified directly, it should not already exist on
#'   the file system.
#' @param width numeric(1L) if NA will use `height` and the aspect ratio from
#'   the "viewBox" if present, or from the element extents if not.
#' @param height numeric(1L) if NA will use `height` and the aspect ratio from
#'   the "viewBox" if present, or from the element extents if not.
#' @param display numeric in 0:2, where 0 does not display, 1 opens the
#'   generated HTML in a browser, and 2 (default) opens the generated HTML in a
#'   browser, and after `timeout` seconds (enough time for browser to open)
#'   deletes the files, but only if target was NULL (to avoid cluttering drive
#'   during testing).
#' @param ncol integer(1L) how many columns to arrange the diptychs in (only for
#'   `compare_svg`).
#' @param quietly TRUE or FALSE (default) passed on to the [plot.svg_chopped()].
#' @param rsvg TRUE or FALSE (default) whether to compare against `rsvg`
#'   rasterizations instead of browser output.
#' @param pattern character(1L) a regular expression to sub-select svgs from the
#'   built-in samples.
#' @param timeout numeric(1L) for the case when `display > `, how many seconds
#'   to wait before unlinking the `target` folder.
#' @param ... additional arguments passed on to [compare_svg()] and [chop()].
#' @return character(1L) the name of the file written to.
#' @examples
#' \dontrun{
#' ## These open browser instances
#' compare_svg()
#' compare_rsvg()
#' }
#' if(interactive()) {
#'   samples <- svg_samples()
#'   scol <- ceiling(sqrt(length(samples)))
#'   srow <- ceiling(length(samples) / scol)
#'   svgs <- as.svg_chopped_list(lapply(samples, chop))
#'   plot(svgs, mfcol=c(srow, scol), scale=TRUE, mai=rep(.1, 4))
#' }

compare_svg <- function(
  files=svg_samples(),
  target=NULL,
  ppi=getOption('svgchop.ppi', 125),
  width=400,
  height=NA_real_,
  ncol=1,
  quietly=FALSE,
  display=2,
  timeout=2,
  rsvg=FALSE,
  ...
) {
  vetr(
    display=INT.1 && . %in% 0:2,
    width=(NUM.1 && . > 0) || (isTRUE(is.na(.)) && !is.na(.(height))),
    height=(NUM.1 && . > 0) || (isTRUE(is.na(.)) && !is.na(.(width))),
    ncol=INT.1.POS.STR, timeout=NUM.1.POS, quietly=LGL.1,
    target=character(1L) || NULL
  )
  unlink.ok <- FALSE
  if(is.null(target)) {
    unlink.ok <- TRUE
    target <- tempfile()
  }
  if(file.exists(target))
    stop("`target` (", target, ") already exists; cannot proceed.")
  dir.create(target)
  out <- file.path(target, "index.html")
  col.str <- sprintf(
    "<col style='width: %spx;'><col style='width: %spx;'>",
    if(!is.na(width)) width + 4 else "auto",
    if(!is.na(width)) width + 4 else "auto"
  )
  writeLines(
    c("<!DOCTYPE html>
      <html>
        <head>
          <style>
          table {border-collapse: collapse;}
          td    {border: 1px solid black; padding: 2px;}
          </style>
        </head>
        <body>
          <table style='border: 1px solid black;'>
      ",
      rep(col.str, ncol)
    ),
    out
  )
  imgs <- character(length(files))
  if(rsvg) {
    if(!requireNamespace('rsvg', quietly=TRUE))
      stop("`rsvg` not available, set `rsvg=FALSE`.")
  }
  for(i in seq_along(files)) {
    if(!(i - 1) %% ncol) cat("<tr>", file=out, append=TRUE)

    bname <- sub("\\..*$", "", basename(files[i]))
    f <- file.path(target, sprintf("img-%s.png", bname))
    imgs[i] <- f
    svg <- chop(files[i], ...)

    xml <- read_xml(files[i])
    svg.node <- xml_find_first(xml, "//svg:svg[not(ancestor::svg:svg)]", NSMAP)

    # set viewbox to extents
    vbe <- vb_from_extents(svg)
    vbec <- as.character(vbe)

    xml_attr(svg.node, 'viewBox') <-
      if(anyNA(vbec)) NA_character_
      else paste0(vbec, collapse=" ")

    w <- width
    h <- height
    # not great for images that are taller than wide
    if(is.na(h) && !is.na(w)) {
      h <- vbe[4] / vbe[3] * w
      xml_attr(svg.node, "height") <- NULL
      xml_attr(svg.node, "width") <- "100%"
    } else if (is.na(w) && !is.na(h)) {
      w <- vbe[3] / vbe[4] * h
      xml_attr(svg.node, "width") <- NULL
      xml_attr(svg.node, "height") <- "100%"
    } else if (is.na(w) && is.na(h))
      stop("Internal Error: contact maintainer.")

    # xml_attr(svg.node, 'preserveAspectRatio') <- 'meet'
    svg.tmp <- file.path(target, sprintf("tmp-%s.svg", bname))
    write_xml(xml, svg.tmp)
    if(rsvg) {
      # rsvg force fits if both width and height are specified, so there appears
      # to be no way to have it respect aspect ratio if we specify both, and
      # thus no way to match something exactly when the viewbox does not match
      # the device aspect ratio.
      #
      # https://stackoverflow.com/questions/35966182/why-is-svg-preserveaspectratio-not-working-rsvg-convert
      #
      # specify only the most constraining of the two
      svg.png <- file.path(target, sprintf("rsvg-%s.png", bname))
      rsvg::rsvg_png(svg.tmp, file=svg.png, width=w, height=h)
      svg.tmp <- svg.png
    }
    cat(
      sprintf("<td><img src='%s' style='width: %spx;'/>", svg.tmp, w),
      file=out, append=TRUE
    )
    # generate chopped svg again so that all dims are done correctly.  This is
    # rather lazy and will take additional time.  Maybe can resolve by adding a
    # "fit" parameter to plot.

    png(f, width=w, height=h, res=ppi)
    par(mai=numeric(4))
    plot(svg, ppi=ppi, scale=TRUE, quietly=quietly)
    dev.off()    # this resets old parameters

    cat(sprintf("<td><img src='%s' />", f), file=out, append=TRUE)
  }
  cat("</table></body></html>\n", file=out, append=TRUE)
  res <- file.path(target, 'index.html')
  if(display) {
    # not allowed to test this
    # nocov start
    browseURL(res)
    if(display > 1 && unlink.ok) {
      Sys.sleep(timeout)
      unlink(dirname(res), recursive=TRUE)
    }
    # nocov end
  }
  res
}
## Collapse RGBA Into RGB
##
## Blend Assuming White Background.  Assumes values in 0-1

collapse_alpha <- function(x) {
  if(dim(x)[3] == 4) {
    x[, , -4] * c(x[, , 4, drop=FALSE]) + c(1 - x[, , 4])
  } else x
}

#' @rdname compare_svg
#' @export

compare_rsvg <- function(..., target=NULL, width=400, display=2, timeout=2) {
  if(!requireNamespace('png', quietly=TRUE))
    stop("'png' package required for this function.")
  out <- compare_svg(display=0, timeout=timeout, rsvg=TRUE, width=width, ...)
  unlink.ok <- FALSE
  if(is.null(target)) unlink.ok <- TRUE

  dir <- dirname(out)
  svgs <- list.files(dir, pattern="^img-.*\\.png$", full.names=TRUE)
  rsvgs <- list.files(dir, pattern="^rsvg-.*\\.png$", full.names=TRUE)
  res <- mapply(
    function(svg, rsvg) {
      a <- collapse_alpha(png::readPNG(svg))
      b <- collapse_alpha(png::readPNG(rsvg))
      if(!identical(dim(a), dim(b))) stop("Unequal diff dimensions.")
      out <- file.path(dir, paste0('diff-', basename(svg)))
      png::writePNG(rowMeans(1 - abs(a - b), dims=2), out)
      out
    },
    svgs,
    rsvgs
  )
  col.str <- sprintf(
    "<col style='width: %spx;'><col style='width: %spx;'><col style='width: %spx;'>",
    if(!is.na(width)) width + 4 else "auto",
    if(!is.na(width)) width + 4 else "auto",
    if(!is.na(width)) width + 4 else "auto"
  )
  writeLines(
    c("<!DOCTYPE html>
      <html>
        <head>
          <style>
          table {border-collapse: collapse;}
          td    {border: 1px solid black; padding: 2px;}
          </style>
        </head>
        <body>
          <table style='border: 1px solid black;'>
      ",
      col.str
    ),
    out
  )
  cat(
    sprintf(
      "<tr><td><img src='%s'/><td><img src='%s'/><td><img src='%s'/>",
      rsvgs, svgs, res
    ),
    file=out, append=TRUE
  )
  cat("</table></body></html>\n", file=out, append=TRUE)
  if(display) {
    # not allowed to test this
    # nocov start
    browseURL(out)
    if(display > 1 && unlink.ok) {
      Sys.sleep(timeout)
      unlink(dirname(out), recursive=TRUE)
    }
    # nocov end
  }
  out
}
#' @rdname compare_svg
#' @export

svg_samples <- function(pattern="\\.svg$")
  list.files(
    system.file(package='svgchop', 'svg'), pattern=pattern, ignore.case=TRUE,
    full.names=TRUE
  )


#' Return a Path to the SVG R Logo
#'
#' For demo and examples.
#'
#' @export
#' @param internal TRUE (default) whether to use the logo bundled with this
#'   package, or FALSE to use the one in R's installation directory.
#' @return character(1L) a path to R Logo SVG file.

R_logo <- function(internal=TRUE) {
  vetr(LGL.1)
  if(internal) {
    system.file(package='svgchop', file.path("svg", "07-R-logo.svg"))
  } else file.path(R.home(), 'doc', 'html', 'Rlogo.svg')
}

## Generate an HTML Page With All Samples
##
## Used to test that parsing of multiple SVGs in a single HTML page works.

svg_to_html <- function(
  files, target=paste0(tempfile(), ".html")
) {
  writeLines("<!DOCTYPE html><html><body>", target)
  lapply(files, file.append, file1=target)
  cat( "</body></html>\n", file=target, append=TRUE)
  target
}


