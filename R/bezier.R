#' Interpolate a Bézier Curve
#'
#' For each Bézier curve implied in `coords`, interpolate `steps` segments using
#' the formula taken from
#' [Wikipedia](https://en.wikipedia.org/wiki/Bézier_curve):
#'
#' (1 - t)^3*P0 + 3*(1 - t)^2*t*P1 + 3*(1 - t)*t^2*P2 + t^3*P3
#'
#' where `P0` is the start point, `P1` and `P2` the controls, and `P3` the end
#' point.  The steps are computed by using evenly spread values of `t` between 0
#' and 1.
#'
#' @export
#' @param coords "data.frame" or "list" containing "x" and "y" columns
#'   corresponding to the points in cubic Bézier, with `3n` rows, where `n`
#'   is the number of Bézier curves spliced together in an SVG path.  The
#'   first item is the first control point, as the actual start point must be
#'   retrieved from the prior element and provided to this function via the
#'   `start` parameter.
#' @param start numeric(2) x and y coordinates for the start point of the Bézier
#' @param steps how many steps to use for each curve

bezier_interp2 <- function(coords, start, steps) {
  vetr(
    list(numeric(), numeric()) && length(unique(lengths(.))) == 1,
    numeric(2),
    INT.1.POS.STR
  )
  nc <- length(coords[[1]])
  xy <- do.call(rbind, coords)
  ends <- !(seq_len(nc) %% 3)
  outer <- cbind(start, xy[,ends])
  P0 <- outer[,-ncol(outer), drop=FALSE]
  P3 <- outer[,-1, drop=FALSE]
  P1 <- xy[, seq(1, nc, by=3), drop=FALSE]
  P2 <- xy[, seq(2, nc, by=3), drop=FALSE]

  stopifnot(length(unique(lengths(list(P0, P1, P2, P3)))) == 1L)

  t <- seq(0, 1, length.out=(steps + 1))

  # cubic formula (https://en.wikipedia.org/wiki/B%C3%A9zier_curve), 1st and 2nd
  # derivatives provided too so we could allocate points based on those.  Can be
  # made faster if we use lists instead of matrices.

  bi <- function(t, n, row) {
    (1 - t)^3 * P0[row, n] +
    3 * (1 - t)^2 * t * P1[row, n] +
    3 * (1 - t) * t^2 * P2[row, n] +
    t^3 * P3[row, n]
  }
  xi <- outer(t, seq_len(nc / 3), bi, 1)
  yi <- outer(t, seq_len(nc / 3), bi, 2)

  # Each bezier needs to remove the first point as that will exist as the end
  # point of the prior

  list(c(xi[-1,]), c(yi[-1,]))
}
