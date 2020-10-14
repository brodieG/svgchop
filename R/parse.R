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

## @param x named character vector with properties attached to a polygon

parse_poly <- function(x) {
  if(!"points" %in% names(x)) x[['points']] <- ""
  raw <- regmatches(x[['points']], gregexpr("-?[0-9.]+", x[['points']]))[[1]]
  stopifnot(length(raw) %% 2 == 0)
  coord <- matrix(as.numeric(raw), ncol=2, byrow=TRUE)
  # remove sequential duplicates
  coord <- coord[c(TRUE, rowSums(coord[-1L,] == coord[-nrow(coord),]) < 2),]
  coords <- if(nrow(coord)) {
    # close poly if isn't already closed
    if(any(coord[1,] != coord[nrow(coord),])) {
      coord <- rbind(coord, coord[1,])
    }
    data.frame(x=coord[,1], y=coord[,2])
  } else {
    data.frame(x=numeric(), y=numeric())
  }
}
## @inheritParams parse_poly

parse_rect <- function(x) {
  props <- names(x)
  if(any(c('rx', 'ry', 'pathLength') %in% props))
    warning('"r[xy] and pathLength properties on "rect" not supported')

  if(!'x' %in% props) x[['x']] <- "0"
  if(!'y' %in% props) x[['y']] <- "0"
  if(!all(c('width', 'height') %in% props))
    stop('"rect" requires width and height specified')

  base.props <- c('x', 'y', 'width', 'height')
  coords <- setNames(as.numeric(x[base.props]), base.props)
  if(anyNA(coords))
    stop('"rect" can only be processed if all base properties are numeric')

  xs <- c(
    coords['x'], coords['x'] + coords['width'],
    coords['x'] + coords['width'], coords['x']
  )
  ys <- c(
    coords['y'], coords['y'],
    coords['y'] + coords['height'], coords['y'] + coords['height']
  )
  p <- paste(xs, ys, sep=",", collapse=" ")
  parse_poly(c(list(points=p), x[!props %in% base.props]))
}

#' Retrieve SVG Elements From File
#'
#' Pull all paths and polygons out of an SVG file and convert them to x-y
#' coordinates and L, M, and C SVG path commands corresponding to line segments.
#' Originally this was all built around SVG paths, so we're forcing polygons
#' through that pipeline even though we really don't need to.
#'
#' If the document contains multiple SVG elements, only the first will be
#' parsed.  In the future will likely add the option to directly pass an `xml2`
#' node to allow iterating over multiple svgs.
#'
#' @export
#' @importFrom xml2 xml_attrs xml_find_all xml_ns_strip read_xml xml_name
#' @seealso [interp_paths()]
#' @param file an SVG file
#' @param elements character the types of elements to parse, currently only
#'   "path" and "polygon" are supported.
#' @return an "svg_paths" S3 object, which is a list of "subpath" that have been
#'   converted from their original SVG form to a line segments.  The `x`, `y`,
#'   `width`, and `height` values of the outer SVG element recorded in the "box"
#'   attribute.

parse_svg <- function(file, steps=10) {
  xml <- xml_ns_strip(read_xml(file))
  if(!identical(xml_name(xml), "svg"))
    xml <- xml_find_first(xml, ".//svg")
  if(!identical(xml_name(xml), "svg"))
    stop("Document does not start with an svg node")
  attrs <- xml_attrs(xml)
  width <- height <- x <- y <- NA_real_
  if(all(c('width', 'height') %in% names(attrs))) {
    if(!grepl("^\\d+$", attrs['width']))
      stop("Unrecognize width format ", attrs['width'])
    if(!grepl("^\\d+$", attrs['height']))
      stop("Unrecognize height format ", attrs['height'])
    width <- as.numeric(attrs['width'])
    height <- as.numeric(attrs['height'])
  } else if ('viewBox' %in% names(attrs)) {
    # this isn't right, but appears to work in the couple of examples I've
    # worked with as the width/height and viewbox are the same
    num.rx <- "\\d*\\.?\\d+"
    vb.rx <- sprintf("^\\s*(%s\\s+){3}%s\\s*$", num.rx, num.rx)
    if(!grepl(vb.rx, attrs['viewBox']))
      stop("viewBox attribute in unknown format ", attrs['viewBox'])
    viewbox <- strsplit(trimws(attrs['viewBox']), "\\s+")[[1]]
    x <- as.numeric(viewbox[1])
    y <- as.numeric(viewbox[2])
    width <- as.numeric(viewbox[3])
    height <- as.numeric(viewbox[4])
  }
  if(is.na(x) && all(c('x', 'y') %in% names(attrs))) {
    if(!grepl("^\\d+$", attrs['x']))
      stop("Unrecognize width format ", attrs['x'])
    if(!grepl("^\\d+$", attrs['height']))
      stop("Unrecognize y format ", attrs['y'])
    x <- as.numeric(attrs['x'])
    y <- as.numeric(attrs['y'])
  }
  if(is.na(x)) x <- 0
  if(is.na(y)) y <- 0

  structure(
    parse_node(xml, steps=steps), class='svg_paths', box=c(x, y, width, height)
  )
}

## Parse a Node and All It's Children
##
## Given a single XML node, recurse through it and all children parsing any SVG
## element or path data encountered into X-Y line segment coordinates, and
## recursively collecting attributes listed in `attr.rec`.  These are attributes
## that you decide may sequentially apply to all child nodes (e.g. "transform").
##
## 
## Collected
## attributes are those that are supposed to lead to sequential modifications of
## data and affect
## child nodes.
##
## Only terminal nodes retain all attributes explicitly as members of the object
## list.  Non terminal nodes keep their attributes as R attributes.
##
## , so if you want to
## retain attributes from parents you must include those in 'attr.rec' (although
## this will not tell you which parent the attributes came from).
##
## @param steps
## @param attr.rec list any XML attributes that match the names in this list
##   will be appended to the existing elements associated with that list.
## @return A nested list.  Non-terminal node contain only other non-terminal
##   nodes or terminal nodes, though they retain their XML name and attributes
##   as R attributes.  Terminal nodes are recorded as lists containing elements:
##   * "coords": Coordinates of segmentized SVG objects.
##   * "attr": a character vector of unprocessed attributes attached to the node
##   * "attr.rec": a list of character vectors of the attributes that should be
##     tracked recursively.

parse_node <- function(
  node, attr.rec=list(transform=character(), class=character()),
  steps
) {
  vetr(structure(list(), class='xml_node'), list(), INT.1.POS.STR)
  term.nodes <- c('path', 'polygon', 'rect')  # parseable nodes

  attrs <- as.list(xml_attrs(node))
  for(i in names(attrs)[names(attrs) %in% names(attr.rec)]) {
    attr.rec[[i]] <- c(attr.rec[[i]], attrs[[i]])
  }
  if(xml_length(node, only_elements=TRUE)) {
    # Non-terminal node, recurse
    res <-
      lapply(xml_children(node), parse_node, attr.rec=attr.rec, steps=steps)
    attr(res, 'xml_attrs') <- attrs
    attr(res, 'xml_name') <- xml_name(node)
    res
  } else {
    # Parse terminal node
    ndat <- as.list(xml_attrs(node))
    list(
      name=xml_name(node),
      coords=switch(tolower(xml_name(node)),
        path=parse_path(ndat, steps),
        polygon=parse_poly(ndat),
        rect=parse_rect(ndat),
        data.frame(x=numeric(), y=numeric())
      ),
      attr.rec=attr.rec,
      attr=attrs
    )
  }
}
