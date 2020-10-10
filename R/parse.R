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

# - Basic Processing -----------------------------------------------------------

## Parse SVG Path Data
##
## Letter, space, comma and hyphen delimited x y coordinate pairs.
##
## @param x an svg path

path_components <- function(x) {
   pieces <- unname(split(x, cumsum(grepl("[a-zA-Z]", x))))
   lapply(pieces, function(y) list(y[1], as.numeric(y[-1])))
}
#' Convert Path To Absolute Curve and Lines
#'
#' Recomputes any relative commands into their absolute equivalents,
#' replaces H and V commands with their L equivalents, explicitly closes paths
#' with L commands instead of Z, and converts arc commands to line segments.
#'
#' Assumes first command is a "moveto" command.
#'
#' @param list of lists, each sub-lists contains the command type (e.g. M, C, L,
#'   V, H, Z, A, m, c, l, v, h, z, a) at position one, and then a set of
#'   coordinates as a numeric vector at position two.
#' @return a list of lists similar to the input, except the only commands
#'   therein will be M, C, L, and coordinates will be absolute

path_to_abs <- function(path, steps) {
  invalid_cmd <- function(i, cmd) stop("Invalid ", cmd, " command at index ", i)
  x0 <- 0
  y0 <- 0
  for(i in seq_along(path)) {
    el <- path[[i]]
    len <- length(el[[2]])
    path[[i]] <- switch(
      el[[1]],
      m=,l=,M=,L={
        if(!len || len %% 2) invalid_cmd(i, el[[1]])
        xs <- el[[2]][seq(1, length.out=len / 2, by=2)]
        ys <- el[[2]][seq(2, length.out=len / 2, by=2)]
        if(el[[1]] %in% c('c', 'm', 'l')) {
          xs <- cumsum(xs) + x
          ys <- cumsum(ys) + y
        }
        x <- xs[len / 2]
        y <- ys[len / 2]
        cmd <- toupper(el[[1]])
        if(cmd == 'M') {
          x0 <- x
          y0 <- y
        }
        list(
          c(cmd, rep("L", len / 2 - 1)),
          xs, ys
        )
      },
      c=,C={
        if(!len || len %% 6) invalid_cmd(i, el[[1]])
        xs <- el[[2]][seq(1, length.out=len / 2, by=2)]
        ys <- el[[2]][seq(2, length.out=len / 2, by=2)]
        if(el[[1]] == 'c') {
          # reset current point every 3 coordinate pairs; not clear if it should
          # be every three or at the end of a command, hoping it is every three
          # otherwise we have to distinguish b/w sequential C commands and a
          # longer C command.
          x.off <- c(0, cumsum(xs[seq(3, length.out=len / 6 - 1, by=3)])) + x
          y.off <- c(0, cumsum(ys[seq(3, length.out=len / 6 - 1, by=3)])) + y
          xs <- xs + rep(x.off, each=3)
          ys <- ys + rep(y.off, each=3)
        }
        x <- xs[len / 2]
        y <- ys[len / 2]
        list(rep("C", len / 2), xs, ys)
      },
      v=,h=,V=,H={
        rel <- el[[1]] == 'v' || el[[1]] == 'h'
        f <- if(rel) cumsum else identity
        cmd <- toupper(el[[1]])
        if(cmd == 'V') y <- f(el[[2]]) + y * rel
        else x <- f(el[[2]]) + x * rel
        list(rep("L", length(x)), x, y)
      },
      z=,Z={
        if(len) invalid_cmd(i, el[[1]])
        x <- x0
        y <- y0
        list("L", x, y)
      },
      a=,A={
        # big problem is current framework is designed to reduce all path
        # information into x-y ncoordinates and we can't do that with arcs.
        # So we're forced to turn the arcs into line segments earlier than we
        # woud have otherwise.  This is where turning them to beziers might make
        # more sense, but not worth the hassle ATM.

        if(!len || len %% 7) invalid_cmd(i, el[[1]])
        if(el[[1]] == 'a') {
          xsi <- seq(6, len, by=7)
          ysi <- seq(7, len, by=7)
          xs <- cumsum(el[[2]][xsi]) + x
          ys <- cumsum(el[[2]][ysi]) + y
          el[[2]][xsi] <- xs
          el[[2]][ysi] <- ys
        }
        segs <- arcs_to_line_segs(el[[2]], x, y, steps)
        x <- segs[[2]][length(segs[[2]])]
        y <- segs[[3]][length(segs[[3]])]
        segs
      },
      stop("unknown command ", el[[1]])
    )
  }
  cmds <- unlist(lapply(path, '[[', 1))
  xs <- unlist(lapply(path, '[[', 2))
  ys <- unlist(lapply(path, '[[', 3))
  data.frame(cmd=cmds, x=xs, y=ys)
}
#' Parse "d" Path Command
#'
#' Convert "d" path attribute into a more usable format containing only "M",
#' "C", and "L" commands.
#'
#' @export
#' @inheritParams parse_path
#' @param x character length 1
#' @return a list of of length equal to `x`'s, with each element a list
#'   containing as many data frames as there are sub-paths in the corresponding
#'   `x` element, with each data frame containing a column with commands in
#'   `c("M","L","C")`.

parse_d <- function(x, steps) {
  if(!is.character(x) || length(x) != 1) stop("Input not character(1L)")
  raw <- regmatches(x, gregexpr("-?[0-9]*\\.?[0-9]+|[a-zA-Z]", x))[[1]]
  raw <- unname(split(raw, cumsum(grepl("[a-zA-Z]", raw))))
  cmds <- lapply(
    raw, function(x) {
      if(length(x)) list(x[1], as.numeric(x[-1]))
      else list()
  } )
  # Convert qQ to cC

  is.q <- vapply(cmds, "[[", "", 1) %in% c('q', 'Q')
  cmds[is.q] <- lapply(
    cmds[is.q],
    function(x) {
      if(!length(x[[2]]) || length(x[[2]]) %% 2)
        stop("Malformed quadratic bezier ", paste0(unlist(x), collapse=" "))
      x[[2]] <- c(
        matrix(x[[2]], 2)[,
          rep(seq_len(length(x[[2]]) / 2), rep(2:1, length(x[[2]]) / 4))
      ] )
      x[[1]] <- c('c', 'C')[match(x[[1]], c('q', 'Q'))]
      x
  } )
  # Convert to absolute coords
  cmds.abs <- path_to_abs(cmds, steps)

  # Split subpaths into paths
  unname(split(cmds.abs, cumsum(cmds.abs[['cmd']] == 'M')))
}
#' Convert SVG Path to More Usable format
#'
#' For polygons there is only ever one sub-path.  This also converts arcs to
#' line segments.  Bézier curves are converted in the interpolation step later.
#' That interpolation happens both at parse and interpolation time is a design
#' flaw that originates from late addition of arc parsing.
#'
#' @export
#' @see_also [interp_paths()]
#' @param x a list representing a single SVG "path", which each element of the
#'   list a property of the path.  The "d" property will be a list of "subpath"
#'   S3 objects.
#' @param steps positive integer(1), how many line segments to use per radian of
#'   arc when converting arcs to line segments.
#' @return a list with as many elements as there are sub-paths in the path "d"
#'   property.  Each element is a "subpath" S3 object containing the path
#'   commands and coordinates in a data frame, and all other path properties as
#'   strings

parse_path <- function(x, steps=5) {
  x <- as.list(x)
  x[['coords']] <-
    if(!"d" %in% names(x)) list()
    else lapply(
      parse_d(x[['d']], steps=steps),
      structure, class=c('subpath','data.frame')
    )
  x
}
#' @rdname parse_path
#' @export

parse_poly <- function(x) {
  x <- as.list(x)
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
    data.frame(cmd=c('M', rep('L', nrow(coord) - 1L)), x=coord[,1], y=coord[,2])
  } else {
    data.frame(cmd=character(), x=numeric(), y=numeric())
  }
  x[['coords']] <- list(structure(coords, class=c('subpath','data.frame')))
  x
}

#' Retrieve SVG Elements From File
#'
#' Pull all paths and polygons out of an SVG file and convert them to x-y
#' coordinates and L, M, and C SVG path commands corresponding to line segments.
#' Originally this was all built around SVG paths, so we're forcing polygons
#' through that pipeline even though we really don't need to.
#'
#' If the document contains multiple SVG elements, only the first will be
#' parsed.
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

parse_svg <- function(file) {
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

  els <- xml_find_all(xml, ".//path|.//polygon")
  type <- xml_name(els)
  el.path <- type == 'path'
  el.poly <- type == 'polygon'

  res <- list(length(els))
  res[el.path] <- lapply(xml_attrs(els[el.path]), parse_path)
  res[el.poly] <- lapply(xml_attrs(els[el.poly]), parse_poly)

  structure(
    res, class='svg_paths', box=c(x, y, width, height),
    type=type
  )
}
