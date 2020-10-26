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

## Extract "url" Elements, Process Them, and Attach Them as Attributes
##
## "url" objects are gradient, pattern, clip, and mask elements that are
## referenced by other elements with the `url(#id)` expression in certain
## attributes.

process_url <- function(node) {
  # Looking for things with xml_name in the known elements

  name <- attr(node, 'xml_name')
  if(is.null(name)) name <- ""
  url.old <- attr(node, 'url')
  if(is.null(url.old)) url.old <- structure(list(), class='url-data')
  id <- attr(node, 'xml_attrs')[['id']]
  if(is.null(id)) id <- ""

  # If it is something to extract extract it and make it the attribute,
  # else recurse.  Another option would be to do this via environment, which
  # would be cleaner to implement but would mean by ref modifications.

  if(
    name %in%
    c('linearGradient', 'radialGradient', 'pattern', 'clipPath', 'mask')
  ) {
    node.new <- structure(list(), class=c('terminal', name))
    url.old[[id]] <- switch(
      name,
      linearGradient=process_gradient_linear(node),
      radialGradient=process_gradient_radial(node),
      node
    )
  } else if (is.list(node) && length(node) && !inherits(node, 'terminal')) {
    node.new <- lapply(node, process_url)
    urls.new <- lapply(node.new, attr, 'url')
    node.new <- lapply(node.new,
      function(x) {
        attr(x, 'url') <- NULL
        x
      }
    )
    # reduce them into one url object
    urls.new <- Reduce(
      function(x, y) {
        x[names(y)] <- y
        x
      },
      urls.new
    )
    url.old[names(urls.new)] <- urls.new
  } else node.new <- node

  attributes(node.new) <- attributes(node)
  attr(node.new, 'url') <- url.old
  node.new
}
#' Approximate Fill
#'
#' The "fill" attribute to SVG elements may be specified in the form "url(#id)"
#' where "id" is the DOM id of another SVG element.  This is used to implement
#' complex fills such as gradients and patterns.  This function will attempt to
#' represent the complex fills with a single color if it can based on data from
#' the url-referenced object.
#'
#' Currently only gradients are approximated.  They are approximated by taking
#' the arithmetic mean of the stop color RGB values.
#'
#' @export
#' @seealso [process_svg()]
#' @param fill character(1L) a value used as the "fill" attribute of an SVG
#'   element.
#' @param url "url-data" object, typically kept as the "url" attribute of
#'   "svg_chopped_list" objects.
#' @return character(1L), a hex color, or NA_character_ if the fill could not
#'   be approximated by a color.
#' @examples
#' svg <- process_svg(file.path(R.home(), 'doc', 'html', 'Rlogo.svg'))
#' fill.1 <- attr(svg[[1]][[2]], 'style-computed')[['fill']]
#' fill.1 # A gradient fill
#' approximate_fill(fill.1, attr(svg, 'url'))

approximate_fill <- function(fill, url) {
  vetr(character(1L), structure(list(), class="url-data"))
  if(grepl("^\\s*url\\(#[^\\)]+\\)\\s*$", fill)) {
      url.id <- sub(".*#([^\\)]+)\\).*", "\\1", fill)
    obj <- url[[url.id]]
    if(inherits(obj, 'gradient')) {
      stops <- obj[['stops']][['color']]
      rgb(t(round(rowMeans(col2rgb(stops)))), maxColorValue=255)
    } else NA_character_
  } else fill
}
