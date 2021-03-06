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
  starts <- attr(mx, 'starts')
  res <- if(!length(starts)) {
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
# Turn back a polyclip list into matrix format
#
# Main thing is recovering starts.  Need to figure out what to do with closed
# vs. not.  Seems like we can't really do this because there is no way to keep
# track of sub-paths.  We could do it in the case of single polygons.  For now
# we just assume closed if the first element is closed.

as_svg_chop_mx <- function(pc.poly, closed) {
  if(!length(pc.poly)) {
    matrix(numeric(), 2, 0)
  } else {
    if(!is.list(pc.poly[[1]])) {
      pc.poly <- list(pc.poly)
    }
    if(length(closed) && isTRUE(closed[[1]])) {
      closed <- TRUE
      pc.poly <- lapply(
        pc.poly,
        function(poly) {
          poly[[1]][length(poly[[1]]) + 1] <- poly[[1]][1]
          poly[[2]][length(poly[[2]]) + 1] <- poly[[2]][1]
          poly
      } )
    } else {
      closed <- FALSE
    }
    # re-compute starts for holes
    starts <- if(length(pc.poly) > 1) {
      c(
        0,
        cumsum(
          vapply(pc.poly, function(x) length(x[[1]]), 0)[-length(pc.poly)]
      ) ) + 1
    } else {
      NULL
    }
    xs <- unlist(lapply(pc.poly, '[[', 'x'))
    ys <- unlist(lapply(pc.poly, '[[', 'y'))
    res <- rbind(xs, ys)
    attr(res, 'starts') <- starts
    attr(res, 'closed') <- rep(closed, length(starts))
    res
  }
}
get_fill_rule <- function(x) {
  fill.rule <- attr(x, 'xml-attr')[['fill-rule']]
  if(isTRUE(fill.rule %in% c('nonzero', 'evenodd'))) fill.rule
  else 'evenodd'
}
get_clip_rule <- function(x) {
  clip.rule <- attr(x, 'xml-attr')[['clip-rule']]
  if(isTRUE(clip.rule %in% c('nonzero', 'evenodd'))) clip.rule
  else 'evenodd'
}

## Process Clip Paths
##
## Clip paths are OR intersections of their components.  When applied to other
## elements all clip paths in the ancestry chain are ANDed together.  Once
## processed these should be stored in the URL element.
##
## Clip path should be a list of coordinates.
##
## Clip paths elements are always transformed based on the transforms specified
## where they are defined.  This is different than whatever transformations they
## may be subject to once they are copied into the display tree.

process_clip_path <- function(node, transform=TRUE) {
  attrs <- attr(node, 'xml_attrs')
  if(
    !is.null(attrs[['clipPathUnits']]) &&
    !identical(attrs[['clipPathUnits']], "userSpaceOnUse")
  )
    sig_u("clipPathUnits set to something other than 'userSpaceOnUse'")

  trans.tree <- compute_transform(node)
  res <- if(transform) apply_transform(trans.tree) else trans.tree

  # Take the OR of everything, assume all paths closed.  Polyclip requires paths
  # to be lists of lists with the last point not overlapping the first.  We'll
  # do the ORing sequentially.  We junk everything except the coordinates.
  flat <- flatten_rec2(res)
  if(length(flat)) {
    pcpoly <- lapply(flat, as_polyclip_poly)
    clip.rule <- lapply(flat, get_clip_rule)
    res <- pcpoly[[1L]]
    if(length(flat) > 1) {
      for(i in seq(2, length(flat), 1)) {
        res <- polyclip::polyclip(
          res, pcpoly[[i]], 'union',
          fillA=clip.rule[[i - 1]], fillB=clip.rule[[i]]
      ) }
      attr(res, 'clip-rule') <- "evenodd" # either is valid
    } else {
      attr(res, 'clip-rule') <- clip.rule[[1]]
    }
    res
  } else {
    list()
  }
}
## Traverse svg_chopped Collecting Clip Paths And Applying Them
##
## In many cases we don't want to actually apply the clipping because the
## clipping doesn't directly make sense for the stroke.  What really needs to be
## done is to compute the stroke as a polygon e.g. with `polylineoffset` and
## clip that and the fill separately.
##
## From the polyclip C++ source website:
##
## > the solution fill type can be considered either EvenOdd or NonZero since 
## > it will comply with either filling rule
##
## http://www.angusj.com/delphi/clipper/documentation/Docs/Units/ClipperLib/Classes/Clipper/Methods/Execute.htm

apply_clip_path <- function(node, url, prev.clip=NULL) {
  clip.path <- attr(node, 'clip-path')
  clip <- if(is.list(clip.path)) {
    if(is.list(prev.clip)) {
      tmp <- polyclip::polyclip(
        prev.clip, clip.path,
        fillA=attr(prev.clip, 'clip-rule'), fillB=attr(clip.path, 'clip-rule')
      )
      attr(tmp, 'clip-rule') <- 'evenodd'
      tmp
    } else clip.path
  } else prev.clip

  res <- if(!is.list(node)) {
    if(length(clip)) {
      res.pc <- polyclip::polyclip(
        as_polyclip_poly(node), clip,
        fillA=get_fill_rule(node), fillB=attr(clip, 'clip-rule')
      )
      as_svg_chop_mx(res.pc, attr(node, 'closed'))
    } else {
      node
    }
  } else lapply(node, apply_clip_path, url=url, prev.clip=clip)

  old.dim <- dim(res)
  old.dimnames <- dimnames(res)
  new.attrs <- attributes(node)[
    !names(attributes(node)) %in% c('dim', 'dimnames', 'starts', 'closed')
  ]
  attributes(res)[names(new.attrs)] <- new.attrs
  attr(res, 'clip-path') <- clip
  dim(res) <- old.dim
  dimnames(res) <- old.dimnames
  res
}


