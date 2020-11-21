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

interleave <- function(x, y) {
  c(x, y)[order(c(seq_along(x), seq_along(y)))]
}
## @param mult how many cols of y per col of x where ncol(y) > ncol(x)

interleave_cols <- function(x, y, mult) {
  cbind(x, y)[,
    order(c(seq_len(ncol(x)), rep(seq_len(ncol(y) / mult), each=mult)))
  ]
}
## Convert Path To Absolute Coordinates
##
## Recomputes any relative commands into their absolute equivalents.
##
## Assumes first command is a "moveto" command.
##
## @param path list of lists, each sub-lists contains the command type (e.g. M,
##   C, L, V, H, Z, A, m, c, l, v, h, z, a) at position one, and then a numeric
##   vector of parameters at position two.
## @return a list of lists similar to the input, except all commands will be in
##   absolute form.

path_to_abs <- function(path) {
  invalid_cmd <- function(i, cmd) stop("Invalid ", cmd, " command at index ", i)
  x0 <- x <- 0
  y0 <- y <- 0
  res <- vector('list', length(path))
  for(i in seq_along(path)) {
    el <- path[[i]]
    len <- length(el[[2]])
    res[[i]] <- switch(
      el[[1]],
      m=,l=,M=,L={
        if(!len || len %% 2) invalid_cmd(i, el[[1]])
        xs <- el[[2]][seq(1, length.out=len / 2, by=2)]
        ys <- el[[2]][seq(2, length.out=len / 2, by=2)]
        if(el[[1]] %in% c('m', 'l')) {
          xs <- cumsum(xs) + x
          ys <- cumsum(ys) + y
        }
        x <- xs[len / 2]
        y <- ys[len / 2]
        cmd <- toupper(el[[1]])
        if(cmd == 'M') {
          x0 <- xs[1]
          y0 <- ys[1]
        }
        list(cmd, interleave(xs, ys))
      },
      c=,C=,q=,Q=,t=,T=,s=,S={
        blen <- switch(tolower(el[[1]]), c=6, s=4, q=4, t=2)
        if(!len || len %% blen) invalid_cmd(i, el[[1]])
        xs <- el[[2]][seq(1, length.out=len / 2, by=2)]
        ys <- el[[2]][seq(2, length.out=len / 2, by=2)]
        if(el[[1]] %in% letters) {  # relative
          # reset current point after each implied Bézier command; not clear if
          # it should be every implied command or end of a command, hoping it is
          # every implied otherwise we have to distinguish b/w sequential C
          # commands and a longer C command.

          xy.i <- seq(blen / 2, length.out=len / blen - 1, by=blen / 2)
          x.off <- c(0, cumsum(xs[xy.i])) + x
          y.off <- c(0, cumsum(ys[xy.i])) + y
          xs <- xs + rep(x.off, each=blen / 2)
          ys <- ys + rep(y.off, each=blen / 2)
        }
        x <- xs[len / 2]
        y <- ys[len / 2]
        list(toupper(el[[1]]), interleave(xs, ys))
      },
      v=,h=,V=,H={
        rel <- el[[1]] %in% c('v', 'h')
        f <- if(rel) cumsum else identity
        cmd <- toupper(el[[1]])
        off <- if(cmd == 'V') y else x
        coords <- f(el[[2]]) + off * rel

        if(cmd == 'V') y <- coords[len] else x <- coords[len]

        list(cmd, coords)
      },
      z=,Z={
        if(len) invalid_cmd(i, el[[1]])
        x <- x0
        y <- y0
        el[[1]] <- toupper(el[[1]])
        el
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
        x <- el[[2]][length(el[[2]]) - 1]
        y <- el[[2]][length(el[[2]])]
        list("A", el[[2]])
      },
      stop("unknown command ", el[[1]])
    )
  }
  res
}
## Convert Path to Basic Commands
##
## V, H, Z, and A commands are converted to L, the last one by approximating the
## arc with `steps` segments.
##
## @inheritParams path_to_abs
## @inheritParams parse_path
## @return a list of "data.frames", each containing a column of commands, x
##   coordinates, and y coordinates.

path_simplify <- function(path, steps) {
  res <- vector('list', length(path))
  x0 <- 0
  y0 <- 0
  for(i in seq_along(path)) {
    el <- path[[i]]
    cmd <- el[[1]]
    len <- length(el[[2]])
    res[[i]] <- switch(
      cmd,
      M=,L={
        # M command with more than one coordinate pair becomes implicit L
        # command thereafter.
        xs <- el[[2]][seq(1, length.out=len / 2, by=2)]
        ys <- el[[2]][seq(2, length.out=len / 2, by=2)]
        x <- xs[len / 2]
        y <- ys[len / 2]
        if(cmd == 'M') {
          x0 <- xs[1]
          y0 <- ys[1]
        }
        list(c(cmd, rep("L", len / 2 - 1)), xs, ys)
      },
      S=,T=,Q=,C={
        coords <- matrix(el[[2L]], 2)
        # Last Control Reflected (T, S)
        if(cmd %in% c('T', 'S')) {
          blen <- 2L + (cmd == 'S') * 2L
          ctrls <- matrix(0, 2, len / blen)
          ctrl <- if(i > 1L) {
            prev <- do.call(rbind, res[[i-1L]][-1L])
            prevc <- path[[i - 1L]][1L]
            cur <- ref <- prev[, ncol(prev)]
            if(
              prevc %in% c('Q','T') && cmd == 'T' ||
              prevc %in% c('C','S') && cmd == 'S'
            )
              ref <- prev[, ncol(prev) - 1L]
          } else {
            stop("'", cmd, "' command not valid as first command in path.")
          }
          for(j in seq_len(len / blen)) {
            new <- cur + (cur - ref)
            ctrls[, j] <- new
            ref <- new
            cur <- coords[, j * blen / 2L]
          }
          coords <- interleave_cols(ctrls, coords, blen / 2L)
          cmd <- c('Q', 'C')[match(cmd, c('T', 'S'))]
        }
        # Quadratic -> Cubic Bézier, each cubic control point must be 2/3 of the
        # way from endpoints to Quadratic control point.
        if(cmd == 'Q') {
          cmd <- 'C'
          coords.all <- cbind(c(x, y), coords)
          ep1 <- coords.all[, seq(1, ncol(coords.all) - 1, by=2), drop=FALSE]
          ep2 <- coords.all[, seq(3, ncol(coords.all), by=2), drop=FALSE]
          cpq <- coords.all[, seq(2, ncol(coords.all), by=2), drop=FALSE]

          cpc1 <- cpq * 2/3 + ep1 * 1/3
          cpc2 <- cpq * 2/3 + ep2 * 1/3

          coords <- cbind(cpc1, cpc2, ep2)[, order(rep(seq_len(ncol(cpc1)), 3))]
        }
        xs <- coords[1L,]
        ys <- coords[2L,]
        x <- xs[length(xs)]
        y <- ys[length(ys)]

        # Cubic Bézier -> Line segments
        start <- vapply(res[[i-1]][2:3], function(x) x[length(x)], 1)
        coords.i <- bezier_interp(list(xs, ys), start, steps=steps)

        c(list(rep('L', length(coords.i[[1L]]))), coords.i)
      },
      V=,H={
        if(cmd == 'V') {
          ys <- el[[2]]
          xs <- rep(x, len)
        } else {
          xs <- el[[2]]
          ys <- rep(y, len)
        }
        x <- xs[len]
        y <- ys[len]
        list(rep("L", len), xs, ys)
      },
      Z={
        if(len) invalid_cmd(i, el[[1]])
        x <- x0
        y <- y0
        list("L", x, y)
      },
      A={
        if(!len || len %% 7) invalid_cmd(i, el[[1]])
        segs <- arcs_to_line_segs(el[[2]], x, y, steps)
        x <- segs[[2]][length(segs[[2]])]
        y <- segs[[3]][length(segs[[3]])]
        segs
      },
      stop("unknown command ", el[[1]])
    )
  }
  cmds <- unlist(lapply(res, '[[', 1))
  xs <- unlist(lapply(res, '[[', 2))
  ys <- unlist(lapply(res, '[[', 3))

  # Confirm that only remaining commands are M/L and drop them
  if(!all(cmds %in% c('M','L'))) {
    # nocov start
    stop(
      "Internal Error: simplified path command other than 'M', or 'L' found."
    )
    # nocov end
  }
  rbind(x=xs, y=ys)
}
#' Convert SVG Path to Line Segments
#'
#' Parses the "d" path attribute into X-Y coordinates of line segments collected
#' into sub-paths.  Sub-paths are designated by "M" or "m" commands embedded in
#' the path command.  Bézier curves and paths are interpolated.
#'
#' @export
#' @param x a path SVG node.
#' @param steps positive integer(1), how many line segments to use to
#'   approximate Bézier curves or elliptical arcs.  For arcs, it is the number
#'   of steps for a complete ellipse, so for partial ellipses fewer steps will
#'   be used.
#' @return numeric matrix, 2 x n containing the x and y coordinates of the
#'   endpoints of the n - 1 concatenated line segments that approximate the path
#'   described by the "d" attribute of the path SVG element `x`.  If there is
#'   more than one sub-path, the starting column of sub-paths following the
#'   first will be stored as the "starts" attribute of the matrix.  Whether the
#'   (sub)paths are open or closed is recorded as the "closed" attribute.
#'   (Sub)paths are considered closed if they end in a "Z" or "z" command.
#'   (Sub)paths that end at the starting coordinate, but do not end in "Z" or
#'   "z", are not considered closed.

parse_path <- function(x, steps=20) {
  if(!'d' %in% names(x))
    matrix(numeric(), 2, 0, dimnames=list(c('x','y'), NULL))
  else {
    raw <- regmatches(
      x[['d']], gregexpr(sprintf("%s|[a-zA-Z]", num.pat.core),  x[['d']])
    )[[1]]
    raw <- unname(split(raw, cumsum(grepl("^[a-zA-Z]$", raw))))
    cmds <- lapply(
      raw, function(x) {
        if(length(x)) list(x[1], as.numeric(x[-1]))
        else list()
    } )
    # Convert to absolute coords
    cmds.abs <- path_to_abs(cmds)
    cmds.cmds <- vapply(cmds.abs, '[[', "", 1)

    # Drop trailing Ms and split
    cmds.rle <- rle(cmds.cmds)
    grp.len <- length(cmds.rle$values)
    if(identical(cmds.rle$values[grp.len], 'M')) {
      m.drop <-
        -seq(length(cmds.abs), length.out=cmds.rle$lengths[grp.len], by=-1)
      cmds.abs <- cmds.abs[m.drop]
      cmds.cmds <- cmds.cmds[m.drop]
    }
    cmds.split <- split(cmds.abs, cumsum(cmds.cmds == 'M'))

    # Simplify to x,y coords
    coords <- unname(lapply(cmds.split, path_simplify, steps))

    # compute which paths are closed
    closed <- unname(
      vapply(cmds.split, function(x) x[[length(x)]][[1]] == 'Z', TRUE)
    )
    # Compute sub-path starting points
    coords <- if(length(coords) > 1) {
      starts <- cumsum(vapply(coords, ncol, 0))
      starts <- starts[-length(starts)] + 1L
      coords <- do.call(cbind, coords)
      attr(coords, 'starts') <- starts
      coords
    } else {
      coords[[1]]
    }
    attr(coords, 'closed') <- closed
    coords
} }

