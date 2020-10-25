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
  if(is.null(url.old)) url.old <- list()
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
