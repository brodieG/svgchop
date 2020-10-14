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

compute_transform <- function(CMS, transform) {

}
## @param x a list maybe containing 'transform' and 'coords' elements

parse_transform <- function(x, CMS=diag(3)) {
  ## raw <- [[1L]]
  cstart <- attr

  raw <- regmatches(x, gregexpr("-?[0-9]*\\.?[0-9]+|[a-zA-Z]", x))[[1]]

  if('transform' %in% names(x) && 'coords' %in% names(x)) {
    trans <- x['transform']
    vet(character(1L), trans)
    raw <- gregexpr("([a-zA-Z]+)\\s*\\(([^)]*)\\)", trans, perl=TRUE)[[1L]]
    cs <- attr(raw, 'capture.start')
    cl <- attr(raw, 'capture.length')
    proc1 <- substr(rep(x, length(cs)), c(cs), c(cs + cl))
    cmds <- proc1[seq_len(nrow(cs))]
    vals <- proc1[seq_len(nrow(cs)) + nrow(cs)]

    vals2 <- lapply(
      regmatches(x, gregexpr("-?[0-9]*\\.?[0-9]+")), as.numeric
    )
    if(any(vapply(vals2, anyNA, TRUE)))
      stop('unparseable parameters in transform command')

    coords <- as.matrix(x[['coords']][, c('x', 'y')])
    CMS <- diag(3)

    for(i in seq_along(cmds)) {
      CMS.tmp <- diag(3)
      valsi <- as.numeric(vals[[i]])
      switch(cmds[i],
        translate={
          if(lengths(valsi) == 2) {
            CMS.tmp[1:2,3] <- valsi
          } else if(lengths(valsi) == 1) {
            CMS.tmp[1,3] <- valsi
          } else stop('Invalid "translate" command')
        },
        rotate={
          CMS.tmp <- diag
          if(!length(valsi) %in% c(1, 3)) stop('Invalid "rotate" command')
          ang <- valsi[1] / 180 * pi

          CMS.tmp[1:2, 1:2] <- c(cos(ang), sin(ang), -sin(ang), cos(ang))
          if(length(valsi) == 3) {
            trans1 <- trans2 <- diag(3)
            trans1[3, 1:2] <- valsi[2:3]
            trans2[3, 1:2] <- -valsi[2:3]
            CMS.tmp <- trans1 %*% CMS.tmp %*% trans2
          }
        },
        stop('"', cmds[i], '" transformation not supported')
      )
      CMS <- CMS %*% CMS.tmp
    }
  }
  CMS
}


