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
#' `flatten` collapses "svg_chopped" recursive structure by placing all
#' non-"hidden" terminal nodes into a one level list.  Flattened
#' "svg_chopped_list" objects will retain distinct elements for each
#' "svg_chopped" contained therein, so an "svg_chopped_list_flat" object will
#' have two levels: one for the "svg_chopped_flat" objects, and one for the
#' visible terminal leaves of those.
#'
#' When flattening terminal leaves are retrieved via depth-first recursion into
#' a single level list for each "svg_chopped" object.  Terminal elements
#' defined inside "defs" will be hidden.  Attributes for the "svg_chopped",
#' "svg_chopped_list", and terminal nodes are retained.
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
#' svg <- chop(file.path(R.home(), 'doc', 'html', 'Rlogo.svg'))
#' str(flatten(svg), give.attr=FALSE)   # attributes may be overwhelming

flatten <- function(x, ...) UseMethod('flatten')

#' @rdname flatten
#' @export

flatten.default <- function(x, ...)
  stop("Default flatten method not implemented")

flatten_rec <- function(x) {
  if(inherits(x, 'hidden')) list()
  else if(!is.list(x)) setNames(list(x), attr(x, 'xml_name'))
  else  unlist(unname(lapply(x, flatten_rec)), recursive=FALSE)
}
## Simplified version for clipping

flatten_rec2 <- function(x) {
  if(is.matrix(x)) list(x)
  else unlist(unname(lapply(x, flatten_rec2)), recursive=FALSE)
}
#' @rdname flatten
#' @export

flatten.svg_chopped <- function(x, ...) {
  res <- flatten_rec(x)
  names <- names(res)
  attrs <- attributes(x)
  attributes(res) <- attrs[names(attrs) != 'names']
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

#' Subset "svg_chopped_flat" Objects
#'
#' Versions of the base subsetting functions that do not drop attributes.  Among
#' other things, this allows us to subset complex objects and still benefit from
#' the inspection methods in the package.  Recall that the return value from
#' [chop()] is an "svg_chopped_list", often containing just one "svg_chopped"
#' object.
#'
#' @rdname subset.svg_chopped
#' @param x an "svg_chopped" or related object
#' @param i mixed subsetting indices
#' @export

`[.svg_chopped` <- function(x, i, ...) get_extents( subset_chop(x, i, ...))

#' @rdname subset.svg_chopped
#' @export

`[.svg_chopped_list` <- function(x, i, ...) subset_chop(x, i, ...)

#' @rdname subset.svg_chopped
#' @export

`[.svg_chopped_list_flat` <- function(x, i, ...) subset_chop(x, i, ...)

#' @rdname subset.svg_chopped
#' @export

`[.svg_chopped_flat` <- function(x, i, ...) get_extents(subset_chop(x, i, ...))

## Should probably have a common class for all the chopped objects instead of
## this hack.

subset_chop <- function(x, i, ...) {
  res <- .subset(x, i, ...)
  nm <- .subset(names(x), i, ...)
  attrs <- attributes(x)
  attributes(res) <- attrs[!names(attrs) == "names"]
  names(res) <- nm
  res
}

#' str Method "svg_chopped*" Objects
#'
#' Exactly like [utils::str()], except the `give.attr` parameter is set to FALSE
#' by default instead of TRUE as otherwise the output is overwhelming.  You can
#' set it explicitly to TRUE to show attributes.
#'
#' @seealso [flatten()] to generate a flat version of the tree that may be
#'   easier to subset.
#' @export
#' @inheritParams utils::str
#' @importFrom utils str
#' @return NULL, invisibly.

str.svg_chopped <- function(object, give.attr=FALSE, ...) {
  res <- NextMethod("str", object=object, give.attr=give.attr, ...)
  message('Attributes suppresssed; set "give.attr = TRUE" to display them')
  invisible(res)
}
#' @rdname str.svg_chopped
#' @export

str.svg_chopped_list <- function(object, give.attr=FALSE, ...)
  NextMethod("str", object=object, give.attr=give.attr, ...)

#' @rdname str.svg_chopped
#' @export

str.svg_chopped_flat <- function(object, give.attr=FALSE, ...) {
  res <- NextMethod("str", object=object, give.attr=give.attr, ...)
  message('Attributes suppresssed; set "give.attr = TRUE" to display them')
  invisible(res)
}
#' @rdname str.svg_chopped
#' @export

str.svg_chopped_list_flat <- function(object, give.attr=FALSE, ...)
  NextMethod("str", object=object, give.attr=give.attr, ...)

#' Create Names Based on XML Data
#'
#' Will recursively traverse lists such as "svg_chopped" object and use the
#' "xml_name" and "xml_attrs" to create names for the list.  These names are not
#' intended to be unique, but may be so if elements contain unique "id" XML
#' attributes.  The purpose of the names is to make the structure of the object
#' more recognizable when viewed via with e.g. [str.svg_chopped()].
#'
#' @noRd
#' @param x a list
#' @return x, but with new names

give_names <- function(x) {
  if(is.list(x) && length(x)) {
    x[] <- lapply(x, give_names)
    names(x) <- vapply(x, make_name, "")
  }
  x
}
make_name <- function(x) {
  name <- class <- id <- ""
  if(!is.null(attr(x, 'xml_name'))) name <- attr(x, 'xml_name')
  if(!is.null(attr(x, 'xml_attrs'))) {
    tmp <- attr(x, 'xml_attrs')[['class']]
    if(is.character(tmp) && length(tmp))
      class <- paste0(c("", tmp), collapse=".")
    tmp <- attr(x, 'xml_attrs')[['id']]
    if(is.character(tmp) && length(tmp))
      id <- paste0(c("", tmp), collapse="#")
  }
  sprintf("%s%s%s", name, id, class)
}


## Twisted Pyramid
##
## To generate the twisting overlapped squares for the tests
##
## @export

# nocov start
twisted_pyramid <- function(tiers=10, angle=10, colors=rainbow(tiers)) {
  outer <- sprintf('
    <svg viewBox="-55 -55 110 110">
      <defs>
        <rect 
        id="base-rect" width="100" height="100" x="-50" y="-50" 
        transform="rotate(-%f)"
        />
      </defs>
      %%s
    </svg>', 
  angle)
  templates <- sprintf('
    <g transform="rotate(%%f) scale(%%f)">
      <use href="#base-rect" fill="%s"/>
      %%s
    </g>',
    colors
  )
  scale <- 1 / (cos((45 - angle) / 180 * pi) * sqrt(2))
  inside <- Reduce(
    function(x, y) sprintf(x, angle, scale, y),
    c(templates, "")
  )
  sprintf(outer, inside)
}
# nocov end








