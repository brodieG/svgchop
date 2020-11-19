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

#' Compare SVGs to Their "Chopped" Renderings
#'
#' Create an HTML page of SVG and their corresponding [chop()]ed and rasterized
#' (png) counterparts juxtaposed into diptychs for comparison.  Viewport and
#' viewbox parameters may be manipulated to ensure the images fit into boxes of
#' width controlled by the `width` parameter.  Original SVG will be on the left,
#' with the rasterized counterpart on the right.
#'
#' @export
#' @importFrom xml2 `xml_attr<-` write_xml
#' @param source character vector of paths or URLs for SVGs to compile into
#'   single HTML file.
#' @param target a file to write the html to.
#' @param width numeric(1L) if NA will use `height` and the aspect ratio from
#'   the "viewBox" if present, or from the element extents if not.
#' @param height numeric(1L) if NA will use `height` and the aspect ratio from
#'   the "viewBox" if present, or from the element extents if not.
#' @param display numeric in 0:2, where does not display, 1 opens the generated
#'   HTML in a browser, and 2 (default) is opens the generated URL in a browser,
#'   and after `timeout` seconds (enough time for browser to open) deletes the
#'   files (to avoid cluttering drive during testing).
#' @param cols integer(1L) how many columns to arrange the diptychs in.
#' @param ... additional arguments passed on to [chop()]
#' @return character(1L) the name of the file written to

svg_gallery <- function(
  source=svg_samples(),
  target=tempfile(),
  ppi=96,
  width=400,
  height=NA_real_,
  cols=1,
  display=2,
  timeout=2,
  ...
) {
  vetr(
    display=INT.1 && . %in% 0:2,
    width=(NUM.1 && . > 0) || (numeric(1) && !is.na(.(height))),
    height=(NUM.1 && . > 0) || (numeric(1) && !is.na(.(width))),
    cols=INT.1.POS.STR, timeout=NUM.1.POS
  )
  dir.create(target)
  out <- file.path(target, "index.html")
  col.str <- sprintf(
    "<col style='width: %spx;'><col style='width: %spx;'>",
    if(!is.na(width)) width else "auto",
    if(!is.na(width)) width else "auto"
  )
  writeLines(
    c("<!DOCTYPE html>
      <html>
        <head>
          <style>
          table {border-collapse: collapse;}
          td    {border: 1px solid black;}
          </style>
        </head>
        <body>
          <table style='border: 1px solid black;'>
      ",
      rep(col.str, cols)
    ),
    out
  )
  imgs <- character(length(source))
  for(i in seq_along(source)) {
    if(!(i - 1) %% cols) cat("<tr>", file=out, append=TRUE)

    f <- file.path(target, sprintf("img-%03d.png",i))
    imgs[i] <- f
    # Compute dimensions for device, as well as for SVG.  This is means we do
    # the chopping twice.
    svg <- chop(source[i], ...)
    vb <- compute_vb_dim(svg[[1]])
    xml <- read_xml(source[i])
    svg.node <- xml_find_first(xml, "//svg:svg[not(ancestor::svg:svg)]", NSMAP)

    w <- width
    h <- height
    # not great for images that are taller than wide
    if(is.na(h) && !is.na(w)) {
      h <- vb$height / vb$width * w
      xml_attr(svg.node, "height") <- NULL
      xml_attr(svg.node, "width") <- "100%"
    } else if (is.na(w) && !is.na(h)) {
      w <- vb$width / vb$height * h
      xml_attr(svg.node, "width") <- NULL
      xml_attr(svg.node, "height") <- "100%"
    } else if (is.na(w) && is.na(h))
      stop("Internal Error: contact maintainer.")

    # set viewbox to extents if not set in the original SVG, and write back the
    # modified SVG to a tempfile for eventual inclusion into the gallery
    if(is.na(xml_attr(svg.node, 'viewBox'))) {
      ext <- attr(svg[[1]], 'extents')
      xml_attr(svg.node, 'viewBox') <- paste(
        ext$x[1], ext$y[1], ext$x[2] - ext$x[1], ext$y[2] - ext$y[1]
      )
    }
    svg.tmp <- file.path(target, sprintf("tmp-%04d.svg", i))
    write_xml(xml, svg.tmp)
    cat(sprintf("<td><img src='%s' />", svg.tmp), file=out, append=TRUE)

    # generate chopped svg again so that all dims are done correctly.  This is
    # rather lazy and will take additional time.  Maybe can resolve by adding a
    # "fit" parameter to plot.
    svg <- chop(svg.tmp, ...)
    png(f, width=w, height=h, res=ppi, type="cairo-png")
    par(mai=numeric(4))
    plot(svg, ppi=ppi)
    dev.off()    # this resets old parameters

    cat(sprintf("<td><img src='%s' />", f), file=out, append=TRUE)
  }
  cat("</table></body></html>\n", file=out, append=TRUE)
  res <- file.path(target, 'index.html')
  if(display) {
    browseURL(res)
    if(display > 1) {
      Sys.sleep(timeout)
      unlink(dirname(res), recursive=TRUE)
    }
  }
  res
}
#' @rdname svg_gallery
#' @export

svg_samples <- function()
  list.files(
    system.file(package='svgchop', 'svg'), pattern="\\.svg$", ignore.case=TRUE,
    full.names=TRUE
  )

## Generate an HTML Page With All Samples
##
## Used to test that parsing of multiple SVGs in a single HTML page works.

samples_to_html <- function(
  source=svg_samples(),
  target=paste0(tempfile(), ".html")
) {
  writeLines("<!DOCTYPE html><html><body>", target)
  lapply(source, file.append, file1=target)
  cat( "</body></html>\n", file=target, append=TRUE)
  target
}


