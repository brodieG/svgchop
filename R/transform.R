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




## Convert Sequence of Transforms into One Transformation Matrix
##
## It is inefficient to record the full set of transforms, and then recompute
## the transforms for each leaf in the tree.
##
## Drive through tree accumulating transform strings and converting them to
## transform matrix.  For debug purposes we want to keep the full transform
## string, maybe as an attribute to the CMS matrix.


## I recursion, we will have:
## * The previous transform matrix
## * The accumulated transform strings
## * The current transform string, if any
##
## Produce the new transform matrix, and append the transform string to the
## accumulation.  So we produce "transform" objects containing the matrix and
## the accumulation.

## @param x a list maybe containing 'transform' and 'coords' elements

#' Compute and Apply SVG Transforms to Coordinates
#'
#' SVG transforms should be applied recursively to nested elements.  We
#' implement this by recursively collecting the "transform" XML attributes,
#' parsing them into transformation matrices, and ultimately applying the
#' accumulated transformation matrices to the terminal SVG elements.
#'
#' @seealso [parse_svg()]
#' @export
#' @param x an "svg_chopped" object as produced by [parse_svg()].
#' @return x, with coordinates transformed.

transform_coords <- function(x, apply=TRUE) {
  vetr(structure(list(), class='svg_chopped'), LGL.1)
  trans.tree <- compute_transform(x)
  if(apply) apply_transform(trans.tree) else trans.tree
}

## Internal Transformation Functions
##
## For debugging purposes we have split the transformation process into
## a "compute" step in which we parse the transformation commands and calculate
## the transformation matrices, and an "apply" step where we transform
## previously computed segmentized SVG coordinates.  Because these coordinates
## are stored in a recursive list that reflects the topology of the original SVG
## document, [compute_transform()] accepts as input an "svg_chopped" object list
## with that topology and produces as output an "svg_transform" object, which is
## a list with the same topology but with "trans" objects as the leaves instead
## of the coordinates.
##
## "trans" objects contain the accumulated 3 x 3 transformation matrix along
## with a list of all the SVG transform commands that were combined to produce
## it.
## @param node a node from an "svg_chopped" object.
## @param trans a transformation object as produced by [trans()].
## @param trans.tree a recursive list of "trans" objects as produced by
##   [compute_transform()].
## @param mx the transformation matrix representing accumulated transformations
##   up to this point.
## @param cmds list of SVG transformation commands that when applied
##   sequentially are equivalent to applying `mx`.
## @return for [compute_transform()], an "svg_transform" object, for [trans()] a
##   "trans" object, for [apply_transform()] an "svg_chopped" object.  See
##   Details.


trans <- function(mx=diag(3), cmds=list()) {
  vetr(matrix(numeric(), 3, 3), list())
  structure(list(mx=mx, cmds=cmds), class='trans')
}

parse_transform <- function(node, trans.prev=trans()) {
  trans.dat <- attr(node, 'xml_attrs')[['transform']]
  mx <- trans.prev[['mx']]
  if(is.null(trans.dat)) {
    cmds.full <- character()
  } else {
    vet(character(1L), trans.dat, stop=TRUE)
    raw <- gregexpr("([a-zA-Z]+)\\s*\\(([^)]*)\\)", trans.dat, perl=TRUE)[[1L]]
    cs <- attr(raw, 'capture.start')
    cl <- attr(raw, 'capture.length')
    proc1 <- substr(rep(trans.dat, length(cs)), c(cs), c(cs + cl - 1))
    cmds <- proc1[seq_len(nrow(cs))]
    vals <- proc1[seq_len(nrow(cs)) + nrow(cs)]
    vals2 <- lapply(
      regmatches(
        vals,
        gregexpr("-?[0-9]*\\.?[0-9]+(?:e[+-][0-9]+)?", vals)
      ), as.numeric
    )
    if(any(vapply(vals2, anyNA, TRUE)))
      stop('unparseable parameters in SVG transform command')

    cmds.full <- character()
    for(i in seq_along(cmds)) {
      mx.tmp <- diag(3)
      valsi <- vals2[[i]]
      switch(cmds[i],
        translate={
          if(length(valsi) == 2) {
            mx.tmp[1:2,3] <- valsi
          } else if(length(valsi) == 1) {
            mx.tmp[1,3] <- valsi
          } else stop('Invalid "translate" command')
        },
        rotate={
          mx.tmp <- diag(3)
          if(!length(valsi) %in% c(1, 3))
            stop('Invalid "rotate" transform command')
          ang <- valsi[1] / 180 * pi

          mx.tmp[1:2, 1:2] <- c(cos(ang), sin(ang), -sin(ang), cos(ang))
          # 3 params means translate -> rotate -> untranslate
          if(length(valsi) == 3) {
            trans1 <- trans2 <- diag(3)
            trans1[1:2, 3] <- valsi[2:3]
            trans2[1:2, 3] <- -valsi[2:3]
            mx.tmp <- trans1 %*% mx.tmp %*% trans2
          } else if(length(valsi) != 1)
            stop('Invalid "rotate" transform command')
        },
        scale={
          if(length(valsi) %in% 1:2) {
            mx.tmp[cbind(1:2, 1:2)] <- valsi
          } else stop('Invalid "scale" transform command')
        },
        skewX={
          if(length(valsi) == 1) {
            mx.tmp[1,2] <- tan(valsi / 180 * pi)
          } else stop('Invalid "skewX" transform command')
        },
        skewY={
          if(length(valsi) == 1) {
            mx.tmp[2,1] <- tan(valsi / 180 * pi)
          } else stop('Invalid "skewX" transform command')
        },
        matrix={
          if(length(valsi) == 6) {
            mx.tmp[1:2,1:3] <- valsi
          } else stop('Invalid "matrix" transform command')
        },
        stop('"', cmds[i], '" transformation not supported')
      )
      mx <- mx %*% mx.tmp
      # for posterity...
      cmds.full <- c(
        cmds.full, sprintf("%s(%s)", cmds[[i]], paste0(valsi, collapse=" "))
  ) } }
  trans(mx, append(trans.prev[['cmds']], list(cmds.full)))
}

compute_transform <- function(x, trans.prev=trans()) {
  trans <- parse_transform(x, trans.prev)
  if(!is.list(x) || !length(x) || is.list(attr(x, 'clip-path'))) {
    attr(x, 'transform-computed') <- trans
    x
  }
  if(is.list(x) && length(x)) {
    x[] <- lapply(x, compute_transform, trans)
  }
  x
}

apply_transform <- function(x) {
  attrs <- attributes(x)
  trans <- attrs[['transform-computed']]
  clip <- attrs[['clip-path']]

  if(inherits(trans, 'trans')) {
    x <- if(!is.list(x)) {
      if(is.matrix(x) && ncol(x) && inherits(trans, 'trans')) {
        (trans[['mx']] %*% rbind(x, 1))[-3,,drop=FALSE]
      } else x
    } else x

    if(is.list(clip) && length(clip[[1]])) {
      clip.dat <- as_svg_chop_mx(clip, closed=FALSE)
      clip.trans <- (trans[['mx']] %*% rbind(clip.dat, 1))[-3,,drop=FALSE]
      clip <- as_polyclip_poly(clip.trans)
    }
  }
  if(is.list(x) && length(x)) {
    x[] <- lapply(x, apply_transform)
  }
  attrs[['clip-path']] <- clip
  attributes(x) <- attrs
  x
}



