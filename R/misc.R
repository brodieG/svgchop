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

#' Simplify "svg_chopped" Structure
#'
#' `flatten` collapses "svg_chopped" recursive structure into a one level list.
#' When flattening terminal leaves are retrieved via depth-first recursion into
#' a single level list for each "svg_chopped" object.  "svg_chopped_list"
#' objects will retain distinct elements for each "svg_chopped" contained
#' therein.  Attributes for the "svg_chopped", "svg_chopped_list", and terminal
#' nodes are retained.
#'
#' For convenience the flat list is named with the numeric index of the element
#' and the svg element name.  The underlying recursive list is unnamed so we 
#' use the name to succinctly display key information about the object when
#' examined with [utils::str()] (see example).
#'
#' @export
#' @param x an object to flatten
#' @return the object flattened
#' @examples
#' svg <- process_svg(file.path(R.home(), 'doc', 'html', 'Rlogo.svg'))
#' str(flatten(svg), give.attr=FALSE)   # attributes may be overwhelming

flatten <- function(x, ...) UseMethod('flatten')

#' @rdname flatten
#' @export

flatten.default <- function(x, ...)
  stop("Default flatten method not implemented")

flatten_rec <- function(x) {
  if(inherits(x, 'terminal')) {
    setNames(list(x), attr(x, 'xml_name'))
  } else {
    unlist(lapply(x, flatten_rec), recursive=FALSE)
  }
}
#' @rdname flatten
#' @export

flatten.svg_chopped <- function(x, ...) {
  res <- flatten_rec(x)
  names <- names(res)
  attributes(res) <- attributes(x)
  names(res) <- sprintf("[%s] %s", format(seq_along(names)), names)
  class(res) <- "svg_chopped_flat"
  res
}
#' @rdname flatten
#' @export

flatten.svg_chopped_list <- function(x, ...) {
  res <- lapply(x, flatten, ...)
  attributes(res) <- attributes(x)
  class(res) <- "svg_chopped_list_flat"
  res
}


