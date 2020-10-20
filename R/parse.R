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
    rbind(x=coord[,1], y=coord[,2])
  } else {
    rbind(x=numeric(), y=numeric())
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

#' Convert SVG Elements to Polygons
#'
#' Parse and convert SVG elements into polygons.  SVG transforms are applied to
#' the polygon coordinates, and SVG presentation attributes are computed from
#' style sheets, inline styles and attributes, and are attached as the
#' "style-computed" R attribute.
#'
#' Currently only SVG path, rect, and polygon elements are supported.  Only some
#' transforms are implemented.  CSS support is likely to be particularly fragile
#' as the CSS parsing is regex based and only simple ASCII-only class and id
#' selectors are supported.
#'
#' @export
#' @importFrom xml2 xml_attrs xml_find_all xml_ns_strip read_xml xml_name
#'   xml_text xml_length xml_children
#' @param file an HTML or other XML based text file containing SVG elements.
#' @return an "svg_chopped_list" S3 object, which is a list of "svg_chopped"
#'   objects.  Each "svg_chopped" object represents an SVG viewport the
#'   dimensions of which are recorded in the "box" attribute.  "svg_chopped"
#'   objects are recursive lists with `2 x n` numeric matrices as terminal
#'   leaves.  The matrices contain the X-Y coordinates of the ordered `n`
#'   endpoints of the `n - 1` line segments that the polygon representation of
#'   the SVG elements comprise.

process_svg <- function(file, steps=10) {
  xml <- xml_ns_strip(read_xml(file))
  css <- get_css(xml)

  # top level svgs, nested ones will just be consumed in recursive traversal
  xml <-
    if(!identical(xml_name(xml), "svg"))
      xml_find_all(xml,"//svg[not(ancestor::svg)]")
    else list(xml)

  if(!length(xml))
    stop("Document does not contain svg nodes")

  parsed <- lapply(xml, parse_node, steps=steps)
  transformed <- lapply(parsed, transform_coords)
  styled <- lapply(transformed, process_css, style.sheet=css)

  # compute extents
  get_coords <- function(obj, coord)
    if(is.matrix(obj)) obj[coord,] else lapply(obj, get_coords, coord)
  w.extents <- lapply(
    styled,
    function(x) {
      xs <- range(unlist(lapply(x, get_coords, 1)))
      ys <- range(unlist(lapply(x, get_coords, 2)))
      attr(x, 'extents') <- list(x=xs, ys=ys)
      x
  } )
  structure(w.extents, class='svg_chopped_list')
}
num.pat <- "(-?\\d*\\.?\\d+)\\w*"
parse_length <- function(x) {
  vetr(character())
  num.like <- grepl(sprintf("^\\s*%s\\s*$", num.pat), x)
  res <- rep(NA_real_, length(x))
  res[num.like] <- as.numeric(
    sub(sprintf("^.*?%s.*$", num.pat), "\\1", x[num.like], perl=TRUE)
  )
  if(!all(num.like))
    warning(
      "Some lengths ", paste0(deparse(x[!num.like]), collapse="\n"),
      " could not be parsed into numbers."
    )
  res
}
parse_lengths <- function(x) {
  vetr(CHR.1)
  parse_length(strsplit(trimws(x), "\\s+")[[1]])
}

process_svg_node <- function(node.parsed, xml_attrs) {
  attrs <- xml_attrs
  width <- height <- x <- y <- NA_real_
  viewbox <- rep(NA_real_, 4)

  if('width' %in% names(attrs)) width <- parse_length(attrs[['width']])
  if('height' %in% names(attrs)) height <- parse_length(attrs[['height']])
  if('x' %in% names(attrs)) x <- parse_length(attr[['x']])
  if('y' %in% names(attrs)) y <- parse_length(attr[['y']])

  if ('viewBox' %in% names(attrs)) {
    viewbox <- parse_lengths(attrs[['viewBox']])
    if(length(viewbox) != 4) {
      warning("Unrecognized viewBox format")
      viewbox <- rep(NA_real_, 4)
    }
  }
  if(is.na(x)) x <- 0
  if(is.na(y)) y <- 0

  # return
  structure(
    node.parsed,
    class='svg_chopped',
    viewBox=viewbox,
    x=x, y=y, width=width, height=width
  )
}

## Parse a Node and All It's Children
##
## Given a single XML node, recurse through it and all children parsing any SVG
## element or path data encountered into X-Y line segment coordinates.  All
## nodes retain their XML attributes and XML name as the "xml_attr" and
## "xml_name" R attributes.
##
## Something to consider: use the 'xml_name' as the parent list names to make it
## easier to see what each element is...
##
## @param steps
## @return A nested list with data.frames containing line segment X-Y coords are
##   the leaves.  Branches that end in empty lists are possible.  XML attributes
##   and names are retained as R attributes to each node.

parse_node <- function(node, steps) {
  vetr(structure(list(), class='xml_node'), INT.1.POS.STR)

  attrs <- as.list(xml_attrs(node))

  res <- if(xml_length(node, only_elements=TRUE)) {
    # Non-terminal node, recurse
    lapply(xml_children(node), parse_node, steps=steps)
  } else {
    # Parse terminal node
    switch(tolower(xml_name(node)),
      path=parse_path(attrs, steps),
      polygon=parse_poly(attrs),
      rect=parse_rect(attrs),
      list()
    )
  }
  if(tolower(xml_name(node)) == 'svg') {
    res <- process_svg_node(res, attrs)
  }
  attr(res, 'xml_attrs') <- attrs
  attr(res, 'xml_name') <- xml_name(node)
  res
}
