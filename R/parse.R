
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
#' with L commands instead of Z.
#'
#' Assumes first command is a "moveto" command.
#'
#' @param list of lists, each sub-lists contains the command type (e.g. M, C, L,
#'   V, H, Z, m, c, l, v, h, z) at position one, and then a set of coordinates
#'   as a numeric vector at position two.
#' @param x starting x coordinate
#' @param y starting y coordinate
#' @return a list of lists similar to the input, except the only commands therein
#'   will be M, C, L, and coordinates will be a nx2 matrix of x-y coordinates.

path_to_abs <- function(path) {
  invalid_cmd <- function(i, cmd) stop("Invalid ", cmd, " command at index ", i)
  x0 <- 0
  y0 <- 0
  for(i in seq_along(path)) {
    el <- path[[i]]
    len <- length(el[[2]])
    path[[i]] <- switch(
      el[[1]],
      c=,C=,m=,l=,M=,L={
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
          c(cmd, rep(if(cmd == "C") "C" else "L"), len / 2 - 1),
          xs, ys
        )
      },
      v=,h=,V=,H={
        rel <- el[[1]] == 'v' || el[[1]] == 'h'
        f <- if(rel) cumsum else identity
        if(el[[1]] == 'v') y <- f(el[[2]]) + y * rel
        else x <- f(el[[2]]) + x * rel
        list(rep("L", length(x)), x, y)
      },
      z=,Z={
        if(len) invalid_cmd(i, el[[1]])
        x <- x0
        y <- y0
        list("L", x, y)
      },
      stop("unknown command ", i[[1]])
    )
  }
  cmds <- unlist(lapply(path, '[[', 1))
  xs <- unlist(lapply(path, '[[', 2))
  ys <- unlist(lapply(path, '[[', 3))
  data.frame(cmd=cmds, x=xs, y=ys)
}
#' Split Path Into sub-paths
#'
#' A sub-path starts with M and contains no other Ms.
#'
#' @noRd
#' @param data frame with cmd,x,y
#' @return list of data frames each with a single sub-path

split_path <- function(path)
  unname(split(path, cumsum(path[['cmd']] == 'M')))


#' Parse "d" Path Command
#'
#' Convert "d" path attribute into a more usable format containing only "M",
#' "C", and "L" commands.
#'
#' @export
#' @param x character a vector of the contents of the d attribute of SVG paths
#' @return a list of of length equal to `x`'s, with each element a list
#'   containing as many data frames  as there are sub-paths in the corresponding
#'   `x` element, with each data frame containing a column with commands in
#'   `c("M","L","C")`.

parse_path <- function(x) {
  if(!is.character(x) || length(x) != 1) stop("Input not character(1L)")
  raw <- regmatches(x, gregexpr("-?[0-9.]+|[a-zA-Z]", x))[[1]]
  raw <- unname(split(raw, cumsum(grepl("[a-zA-Z]", raw))))
  cmds <- lapply(
    raw, function(x) {
      if(length(x)) list(x[1], as.numeric(x[-1]))
      else list()
  } )
  # Convert to absolute coords
  cmds.abs <- lapply(cmds, path_to_abs)

  # Split subpaths into paths
  cmds.s <- unlist(lapply(cmds.abs, split_path), recursive=FALSE)

  cmds.s
}

get_path <- function(dat) {
  path <- unlist(strsplit(sub('^"(.*)"$', '\\1', dat), " "))
  path <- path[-length(path)]  # drop "Z" command at end
  cmd <- sub("^([A-Z]*).*$", "\\1", path)
  x <- sub("[^0-9]*([0-9.]*),.*$", "\\1", path)
  y <- sub(".*,([0-9.]*).*$", "\\1", path)
  d <- data.frame(i=seq_along(x), x=as.numeric(x), y=as.numeric(y), cmd=cmd)
  d
}
