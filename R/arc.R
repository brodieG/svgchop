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
#
# The `svg_angle` and `arc_endpoint_to_center` functions in this file started
# off as utilities from the MIT licensed fuse-open fuselibs library.  However,
# upon encountering seeming oddities we re-implemented them completely based on
# the SVG 1.1 Appendix at:
#
# https://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes
#
# Some parameter names survive from the fuselibs implementation.

## Convert SVG Arc Coordinates to Center Point Based
##
## Perform the endpoint to center arc parameter conversion as detailed in
## the SVG 1.1 spec.  F.6.5 Conversion from endpoint to center parameterization
##
## @param p1 numeric(2) first end point coords, in form c(x, y)
## @param p2 numeric(2) second end point coords, in form c(x, y)
## @param r numeric(2) ellipse radii, in form c(x, y)
## @param xAngle numeric(1) ellipse X axis angle to coord system X angle
## @param flagA logical(1) large arc flag
## @param flagB logical(1) sweep flag
## @return list containing:
## * "r" `numeric(2)`, the x and y radii which may have been scaled
## * "c" `numeric(2)`, the x and y coordinates of the center point
## * "ang" `numeric(2)`, the theta (from +ve x-axis, clockwise
##    presumably, actually not obvious as standard rot matrix rotates
##    counter-clockwise) and delta angles in degrees.

vec_ang <- function(u, v) {
  det <- sign(u[1] * v[2] - u[2] * v[1])
  cos <- min(1, max(-1, sum(u*v) / (sqrt(sum(u^2))*sqrt(sum(v^2)))))
  (acos(cos) %% (2*pi)) * if(det) det else 1
}
arc_ep_to_c <- function(p1, p2, r, phi, flagA, flagS) {
  phi <- phi / 180 * pi

  # 6.5.1
  phi_m <- matrix(c(cos(phi), -sin(phi), sin(phi), cos(phi)), 2)
  p1_ <- phi_m %*% matrix(c(p1 - p2) / 2)
  x1_ <- p1_[1]
  y1_ <- p1_[2]

  # 6.6.1 Radii Adjustment
  if(any(r == 0)) {
    stop('not handled')
  }
  # 6.6.2
  r <- abs(r)

  # 6.6.3
  r_adj <- sum(p1_^2 / r^2)
  if(r_adj > 1) r <- r_adj * r

  # 6.5.2
  rx <- r[1]
  ry <- r[2]
  c_ <- sqrt(
    max(
      0,
      (prod(r^2) - rx^2*y1_^2 - ry^2*x1_^2) / (rx^2 * y1_^2 + ry^2 * x1_^2)
    )
  ) * c(rx * y1_ / ry, -ry*x1_ / rx)
  if(!xor(flagA, flagS)) c_ <- -c_

  # 6.5.3
  c <- t(phi_m) %*% matrix(c_) + (p1 + p2) / 2

  # 6.5.4-6
  v <- (p1_ - c_) / r
  theta.1 <- vec_ang(c(1, 0), v)
  w <- (-p1_ - c_) / r
  d.theta <- vec_ang(v, w)
  d.theta <- d.theta +
    if(!flagS && d.theta > 0) -2*pi
    else if(flagS && d.theta < 0) 2*pi
    else 0

  list(
    center=c, angles=c(theta.1, d.theta)/ pi * 180, radii=r,
    c_=c_, p1_=p1_

  )
}

## Convert Absolute Arcs To Path Segments
##
## @return list with x and y coordinates for arc segments

arc_to_line_seg <- function(x0, y0, middle, xn, yn, steps) {
  # reparametrize
  params <- arc_ep_to_c(
    p1=c(x0, y0), p2=c(xn, yn),
    r=middle[1:2], phi=middle[3], flagA=middle[4], flagS=middle[5]
  )
  # generate arc angles
  angles <- params[['angles']] / 180 * pi
  steps <- ceiling(steps * abs(angles[2])) + 1L # at least 2
  theta <- seq(
    params[['angles']][1], params[['angles']][1] + params[['angles']][2],
    length.out=steps
  ) / 180 * pi
  # generate points on arc
  phir <- middle[3]/180*pi
  points <- matrix(c(cos(phir), sin(phir), -sin(phir), cos(phir)), 2) %*%
    (params[['radii']] * rbind(cos(theta), sin(theta))) + c(params[['center']])

  # return skipping first point
  list(xs=points[1,-1], ys=points[2,-1])
}

arcs_to_line_segs <- function(A, x0, y0, steps) {
  vetr(
    A=numeric() && !(length(.) %% 7),
    x0=numeric(1), y0=numeric(1), steps=INT.1.POS.STR
  )
  len <- length(A)
  xi <- seq(6, len, by=7)
  yi <- seq(7, len, by=7)
  xs <- c(x0, A[xi])
  ys <- c(y0, A[yi])
  middle <- split(A[-c(xi, yi)], rep(seq(1, length(xi), by=1), each=5))

  segs <- Map(
    arc_to_line_seg,
    x0=xs[-length(xs)], y0=ys[-length(ys)],
    middle=middle,
    xn=xs[-1], yn=ys[-1],
    steps=steps
  )
  xs <- unlist(lapply(segs, '[[', 1L))
  ys <- unlist(lapply(segs, '[[', 2L))
  list(rep("L", length(xs)), xs, ys)
}
