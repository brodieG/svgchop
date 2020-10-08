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

svg_angle <- function(ux, uy, vx, vy ) {
  u <- c(X=ux, Y=uy);
  v <- c(X=vx, Y=vy);
  # (F.6.5.4)
  dot <- sum(u * v)
  # # Not sure this one is translated correctly
  # len <- Vector.Length(u) * Vector.Length(v);
  len <- sqrt(sum(u^2)) * sqrt(sum(v^2))
  #floating point precision, slightly over values appear
  ang <- acos(max(min(dot / len,1),-1))
  if ( (u[1L]*u[2L] - u[2L]*u[1L]) < 0)
          ang <- -ang;
  ang
};
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

arc_endpoint_to_center <- function(
  p1, p2, r, xAngle, flagA, flagS
)
{
  rX <- abs(r[1L])
  rY <- abs(r[2L])
  xAngle <- xAngle / 180 * pi

  #(F.6.5.1) re-center and rotate
  dx2 <- (p1[1L] - p2[1L]) / 2.0;
  dy2 <- (p1[2L] - p2[2L]) / 2.0;
  x1p <- cos(xAngle)*dx2 + sin(xAngle)*dy2;
  y1p <- -sin(xAngle)*dx2 + cos(xAngle)*dy2;

  #(F.6.5.2)
  rxs <- rX * rX;
  rys <- rY * rY;
  x1ps <- x1p * x1p;
  y1ps <- y1p * y1p;

  # check if the radius is too small `pq < 0`, when `dq > rxs * rys` (see below)
  # cr is the ratio (dq : rxs * rys)
  cr <- x1ps/rxs + y1ps/rys;
  if (cr > 1) {
    #scale up rX,rY equally so cr == 1
    s <- sqrt(cr);
    rX <- s * rX;
    rY <- s * rY;
    rxs <- rX * rX;
    rys <- rY * rY;
  }
  dq <- (rxs * y1ps + rys * x1ps);
  pq <- (rxs*rys - dq) / dq;
  q <- sqrt(max(0,pq)) #use Max to account for float precision
  if (flagA == flagS)
    q <- -q;
  cxp <- q * rX * y1p / rY;
  cyp <- - q * rY * x1p / rX;

  #(F.6.5.3)
  cx <- cos(xAngle)*cxp - sin(xAngle)*cyp + (p1[1L] + p2[1L])/2
  cy <- sin(xAngle)*cxp + cos(xAngle)*cyp + (p1[2L] + p2[2L])/2

  #(F.6.5.5)
  theta <- svg_angle(1, 0, (x1p-cxp) / rX, (y1p - cyp)/rY )
  #(F.6.5.6)
  delta <- svg_angle(
    (x1p - cxp)/rX, (y1p - cyp)/rY,
    (-x1p - cxp)/rX, (-y1p - cyp)/rY
  )
  delta <- delta %% (2 * pi);
  if (!flagS && delta > 0) delta <- delta - 2 * pi;

  r <- c(rX, rY)
  c <- c(cx, cy)
  angles <- c(theta, delta)
  list(r=r, c=c, ang=angles / pi * 180)
}


vec_ang <- function(u, v) {
  det <- sign(u[1] * v[2] - u[2] * v[1])
  cos <- min(1, max(-1, sum(u*v) / (sqrt(sum(u^2))*sqrt(sum(v^2)))))
  (acos(cos) %% (2*pi)) * if(det) det else 1
}

arc_ep_to_c <- function(p1, p2, r, xAngle, flagA, flagS) {
  phi <- xAngle / 180 * pi

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
    (prod(r^2) - rx^2*y1_^2 - ry^2*x1_^2) / (rx^2 * y1_^2 + ry^2 * x1_^2)
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

  ## radius adjustment not implemented
  list(
    center=c, angles=c(theta.1, d.theta)/ pi * 180, radii=r,
    c_=c_, p1_=p1_

  )
}
