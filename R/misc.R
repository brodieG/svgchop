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
#' @param ... unused for additional method parameters.
#' @return the object flattened.
#' @examples
#' ## Normal "svg_chopped" objects are tree-like
#' svg <- chop(svg_samples('shapes'))
#' str(svg, max.level=3, list.len=3)
#'
#' ## Flattened ones are linearized and lose hidden elements
#' svgf <- flatten(svg)
#' str(svgf, list.len=8)
#' length(svgf)          # number of distinct SVG elements
#'
#' ## We can use this to plot only parts of the SVG
#' if (interactive()) {
#'   old.par <- par(mfrow=c(2,2), mai=rep(.1, 4))
#'   plot(svgf, scale=TRUE)             # full plot
#'   plot(svgf[4], scale=TRUE)     # one item
#'   plot(svgf[4:6], scale=TRUE)   # more
#'   plot(svgf[10:12], scale=TRUE) # more
#'   par(old.par)
#' }

flatten <- function(x, ...) UseMethod('flatten')

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
#' @export

flatten.svg_chopped_list <- function(x, ...) {
  res <- lapply(x, flatten, ...)
  attributes(res) <- attributes(x)
  class(res) <- "svg_chopped_list_flat"
  res
}
#' @export

flatten.svg_chopped_flat <- function(x, ...) x

#' @export

flatten.svg_chopped_list_flat <- function(x, ...) x

#' Subset "svg_chopped_flat" Objects
#'
#' Versions of the base subsetting functions that do not drop attributes.  Among
#' other things, this allows us to subset complex objects and still benefit from
#' the inspection methods in the package.
#'
#' @rdname subset.svg_chopped
#' @param x an "svg_chopped" or related object.
#' @param i mixed subsetting indices.
#' @param ... additional parameters passed on to `.subset`.
#' @return on object of the same type as `x`.
#' @export

`[.svg_chopped` <- function(x, i, ...) update_extents(subset_chop(x, i, ...))

#' @export

`[.svg_chopped_flat` <- function(x, i, ...) {
  res <- update_extents(subset_chop(x, i, ...))
  names(res) <- flat_names(names(res))
  res
}
#' @export

`[.svg_chopped_list` <- function(x, i, ...) subset_chop(x, i, ...)

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
#' @export

str.svg_chopped_list <- function(object, give.attr=FALSE, ...)
  NextMethod("str", object=object, give.attr=give.attr, ...)

#' @export

str.svg_chopped_flat <- function(object, give.attr=FALSE, ...) {
  res <- NextMethod("str", object=object, give.attr=give.attr, ...)
  message('Attributes suppresssed; set "give.attr = TRUE" to display them')
  invisible(res)
}
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
#' Convert Objects into "svg_chopped" Objects into "svg_chopped_list"
#'
#' Useful in cases where we wish to take a list of "svg_chopped" objects, e.g.
#' because we produced them with `lapply`, but wish to have access to the
#' "svg_chopped_list" methods.
#'
#' @export
#' @param x a list of "svg_chopped" objects.
#' @return an "svg_chopped_list" object.
#' @examples
#' svgs <- lapply(svg_samples()[1:2], chop)
#' \dontrun{
#' plot(svgs)  ## Error!
#' }
#' if(interactive())
#'   plot(as.svg_chopped_list(svgs), mfrow=c(2,1), scale=TRUE)

as.svg_chopped_list <- function(x) UseMethod('as.svg_chopped_list')

#' @export

as.svg_chopped_list.default <- function(x)
  stop('No `as.svg_chopped` method for object of class ', deparse(class(x)))

#' @export

as.svg_chopped_list.list <- function(x) {
  if(!is.list(x) || !all(vapply(x, inherits, TRUE, 'svg_chopped')))
    stop("Argument 'x' must be a list containing only \"svg_chopped\" objects.")
  structure(x, class="svg_chopped_list")
}
#' @export

as.svg_chopped_list.svg_chopped <- function(x) {
  as.svg_chopped_list(list(x))
}
#' @export

as.svg_chopped_list.svg_chopped_list <- function(x) x

#' Retrieve Basic Data From "svg_chopped" Objects
#'
#' Specialized retrieval functions designed to return a small but useful subset
#' of the data encoded in "svg_chopped" objects so you don't have to wade
#' through the complex structure of those objects.
#'
#' "svg_chopped" objects are first [flatten()]ed and the data is retrieved from
#' each displayable element.
#'
#' `get_xy_coords` returns the coordinates in a format compatible with
#' [grDevices::xy.coords()] (a list with x and y coordinates as numeric
#' vectors), and `get_fills` and `get_strokes` return a character vector with
#' colors processed by [approximate_color()].
#'
#' @seealso [grDevices::xy.coords()], [approximate_color()],
#'   [plot.svg_chopped()].  @export
#' @export
#' @param x an "svg_chopped" or "svg_chopped_flat" object.
#' @return a list or character vector of the same length as `x`.  See details.
#' @examples
#' svg <- chop(R_logo(), steps=3)
#' str(xy <- get_xy_coords(svg))
#' (fills <- get_fills(svg))
#' if(interactive()) {
#'   plot.new()
#'   ext <- attr(svg, "extents")
#'   plot.window(ext$x, rev(ext$y), asp=1)
#'
#'   polypath(xy[[1]], col=fills[[1]], border=NA)
#'   polypath(xy[[2]], col=fills[[2]], border=NA)
#' }

get_xy_coords <- function(x) {
  vetr(
    structure(list(), class='svg_chopped') ||
    structure(list(), class='svg_chopped_flat')
  )
  x <- flatten(x)
  lapply(
    x,
    function(mat) {
      if(is.numeric(attr(mat, 'starts'))) {
        idx <- rep(
          seq_len(ncol(mat)),
          seq_len(ncol(mat)) %in% (attr(mat, 'starts') - 1) + 1
        )
        idx[duplicated(idx)] <- NA
        mat <- mat[,idx]
      }
      list(x=mat[1,], y=mat[2,])
    }
  )
}
get_colors <- function(x, type) {
  vetr(
    structure(list(), class='svg_chopped') ||
    structure(list(), class='svg_chopped_flat')
  )
  x <- flatten(x)
  col <- vapply(x, function(y) attr(y, 'style-computed')[[type]], 'character')
  unname(vapply(col, approximate_color, "", url=attr(x, 'url')))
}
#' @rdname get_xy_coords
#' @export

get_fills <- function(x) get_colors(x, 'fill')

#' @rdname get_xy_coords
#' @export

get_strokes <- function(x) get_colors(x, 'stroke')

# Seem silly to import stats just for this; maybe we should?

setNames <- function(x, y) {
  if(length(x) == length(y) && is.character(y)) names(x) <- y
  x
}

