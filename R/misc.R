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
#' defined inside "&lt;defs&gt;" blocks are considered hidden and omitted from
#' the flat list.  Attributes for the "svg_chopped", "svg_chopped_list", and
#' terminal nodes are retained.
#'
#' The raw names of the flat structure may not be unique, so for convenience we
#' prepend the numeric index of each element so you may subset with numeric
#' indices.
#'
#' "svg_chopped_flat" (and "svg_chopped") objects are best inspected with
#' [utils::str()].
#'
#' @export
#' @seealso [chop()]
#' @param x an object to flatten
#' @return the object flattened
#' @examples
#' ## Normal "svg_chopped" objects are tree-like
#' svg <- chop(svg_samples('shapes'))
#' str(svg, max.level=4, list.len=3)
#'
#' ## Flattened ones are linearized and lose hidden elements
#' svgf <- flatten(svg)
#' str(svgf, list.len=8)
#' length(svgf)          # number of distinct SVG elements
#'
#' ## We can use this to plot only parts of the SVG
#' \donttest{
#' old.par <- par(mfrow=c(2,2), mai=rep(.1, 4))
#' plot(svgf, scale=TRUE)             # full plot
#' plot(svgf[4], scale=TRUE)     # one item
#' plot(svgf[4:6], scale=TRUE)   # more
#' plot(svgf[10:12], scale=TRUE) # more
#' par(old.par)
#' }

flatten <- function(x, ...) UseMethod('flatten')

#' @rdname flatten
#' @export

flatten.default <- function(x, ...)
  stop("Default flatten method not implemented")

flatten_rec <- function(x, names=character(length(x))) {
  if(inherits(x, 'hidden')) list()
  else if(!is.list(x)) setNames(list(x), names)
  else  unlist(unname(Map(flatten_rec, x, names(x))), recursive=FALSE)
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
  names(res) <- flat_names(names)
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
#' the inspection methods in the package.
#'
#' @rdname subset.svg_chopped
#' @param x an "svg_chopped" or related object
#' @param i mixed subsetting indices
#' @export

`[.svg_chopped` <- function(x, i, ...) update_extents(subset_chop(x, i, ...))

#' @rdname subset.svg_chopped
#' @export

`[.svg_chopped_flat` <- function(x, i, ...) {
  res <- update_extents(subset_chop(x, i, ...))
  names(res) <- flat_names(names(res))
  res
}
#' @rdname subset.svg_chopped
#' @export

`[.svg_chopped_list` <- function(x, i, ...) subset_chop(x, i, ...)

#' @rdname subset.svg_chopped
#' @export

`[.svg_chopped_list_flat` <- function(x, i, ...) subset_chop(x, i, ...)

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
flat_names <- function(names) {
  names <- sub("^\\s*\\[[0-9]+\\] ", "", names)
  sprintf(
    "%s %s",
    format(sprintf("[%d]", seq_along(names)), justify='right'), names
  )
}

## For Unsupported Features


sig_u <- function(msg) {
  cond <- simpleCondition(msg)
  class(cond) <- c('svgchop_unsupported', 'svgchop', class(cond))
  signalCondition(cond)
  invisible(NULL)
}
sig_e <- function(msg) {
  cond <- simpleCondition(msg)
  class(cond) <- c('svgchop_error', 'svgchop', class(cond))
  signalCondition(cond)
  invisible(NULL)
}

