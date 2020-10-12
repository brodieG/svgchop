#' Interpolate a Bézier Curve
#'
#' @export
#' @param coords "data.frame" or "list" containing "x" and "y" columns
#'   corresponding to the points in cubic bezier, with `3n` rows, where `n`
#'   is the number of Bezier curves spliced together.
#' @param start numeric(2) x and y coordinates for the start point of the bezier
#' @param steps how many steps to use for each curve

bezier_interp2 <- function(coords, start, steps) {
  vetr(
    list(x=numeric(), y=numeric()) && length(unique(lengths(.))) == 1,
    numeric(2),
    INT.1.POS.STR
  )
  nc <- length(coords[['x']])
  xy <- do.call(rbind, coords[c('x','y')])
  ends <- !(seq_len(nc) %% 3)
  outer <- cbind(start, xy[,ends])
  P0 <- outer[,-ncol(outer)]
  P3 <- outer[,-1]
  P1 <- xy[, seq(1, nc, by=3)]
  P2 <- xy[, seq(2, nc, by=3)]

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
  list(xi, yi)
}
