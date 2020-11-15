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

## Get rid of repeated last point, and transform to list structure
##
## From testing open paths clip the same as closed ones so we don't care whether
## closed explicitly or not.

as_polyclip_poly <- function(mx) {
  res <- if(!length(attr(mx, 'starts'))) {
    list(x=mx[1,], y=mx[2,])
  } else {
    pieces <- cumsum(seq_len(ncol(mx)) %in% starts)
    xs <- unname(split(mx[1,], pieces))
    ys <- unname(split(mx[2,], pieces))
    Map(function(x, y) list(x=x, y=y), xs, ys)
  }
  lapply(
    res,
    function(x) {
      if(length(x[[1]]) > 1) {
        xs <- x[[1]][c(1, length(x[[1]]))]
        ys <- x[[2]][c(1, length(x[[1]]))]
        if(!diff(xs) && !diff(ys))
          list(x=x[[1]][-length(x[[1]])], y=x[[2]][-length(x[[1]])])
        else x
      } else x
} ) }

## Process Clip Paths
##
## Clip paths are OR intersections of their components.  When applied to other
## elements all clip paths in the ancestry chain are ANDed together.
##
## Clip path should be a list of coordinates.

process_clip_path <- function(node, transform=FALSE) {
  trans.tree <- compute_transform(node)
  res <- if(transform) apply_transform(trans.tree) else trans.tree

  # Take the OR of everything, assume all paths closed.  Polyclip requires paths
  # to be lists of lists with the last point not overlapping the first.  We'll
  # do the ORing sequentially.  We junk everything except the coordinates.

  flat <- flatten_rec2(res)
  if(length(flat)) {
    pcpoly <- lapply(flat, as_polyclip_poly)
    fill.rule <- lapply(
      flat, function(x) {
        fill.rule <- attr(x, 'xml-attr')[['fill-rule']]
        if(isTRUE(fill.rule %in% c('nonzero', 'evenodd'))) fill.rule
        else 'evenodd'
    } )
    res <- pcpoly[[1L]]
    if(length(pcpoly) > 1) {
      for(i in seq(2, length(pcpoly), 1)) {
        res <- polyclip::polyclip(
          res, pcpoly[[i]], 'union',
          fillA=fill.rule[[i - 1]], fillB=fill.rule[[i]]
    ) } }
    res
  } else {
    list()
  }
}

apply_clip_path <- function() {

}


