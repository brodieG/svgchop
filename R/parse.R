
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
      m=,l=,M=,L={
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
          c(cmd, rep("L", len / 2 - 1)),
          xs, ys
        )
      },
      c=,C={
        if(!len || len %% 6) invalid_cmd(i, el[[1]])
        xs <- el[[2]][seq(1, length.out=len / 2, by=2)]
        ys <- el[[2]][seq(2, length.out=len / 2, by=2)]
        if(el[[1]] == 'c') {
          # reset current point every 3 coordinate pairs; not clear if it should
          # be every three or at the end of a command, hoping it is every three
          # otherwise we have to distinguish b/w sequential C commands and a
          # longer C command.
          x.off <- c(0, cumsum(xs[seq(3, length.out=len / 6 - 1, by=3)])) + x
          y.off <- c(0, cumsum(ys[seq(3, length.out=len / 6 - 1, by=3)])) + y
          xs <- xs + rep(x.off, each=3)
          ys <- ys + rep(y.off, each=3)
        }
        x <- xs[len / 2]
        y <- ys[len / 2]
        list(rep("C", len / 2), xs, ys)
      },
      v=,h=,V=,H={
        rel <- el[[1]] == 'v' || el[[1]] == 'h'
        f <- if(rel) cumsum else identity
        cmd <- toupper(el[[1]])
        if(cmd == 'V') y <- f(el[[2]]) + y * rel
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
#' Parse "d" Path Command
#'
#' Convert "d" path attribute into a more usable format containing only "M",
#' "C", and "L" commands.
#'
#' @export
#' @param x character length 1
#' @return a list of of length equal to `x`'s, with each element a list
#'   containing as many data frames as there are sub-paths in the corresponding
#'   `x` element, with each data frame containing a column with commands in
#'   `c("M","L","C")`.

parse_d <- function(x) {
  if(!is.character(x) || length(x) != 1) stop("Input not character(1L)")
  raw <- regmatches(x, gregexpr("-?[0-9.]+|[a-zA-Z]", x))[[1]]
  raw <- unname(split(raw, cumsum(grepl("[a-zA-Z]", raw))))
  cmds <- lapply(
    raw, function(x) {
      if(length(x)) list(x[1], as.numeric(x[-1]))
      else list()
  } )
  # Convert to absolute coords
  cmds.abs <- path_to_abs(cmds)

  # Split subpaths into paths
  unname(split(cmds.abs, cumsum(cmds.abs[['cmd']] == 'M')))
}
#' Convert SVG Path to More Usable format
#'
#' @export
#' @param x a list representing a single SVG "path", which each element of the
#'   list a property of the path.  It may be useful to ensure paths have an id
#'   attribute to keep track of where each sub-path produced by this function
#'   came from.
#' @return a list with as many elements as there are sub-paths in the path "d"
#'   property.  Each element is a "subpath" S3 object containing the path
#'   commands and coordinates in a data frame, and all other path properties as
#'   strings

parse_path <- function(x) {
  if(!"d" %in% names(x)) {
    list()
  } else {
    d <- parse_d(x[['d']])
    lapply(d, function(y) {x[['d']] <- y; structure(x, class='subpath')})
  }
}
#' Retrieve SVG Paths From File
#'
#' Pull all paths out of SVG file.
#'
#' @export
#' @importFrom xml2 xml_attrs xml_find_all xml_ns_strip
#' @param file an SVG file
#' @return a list of lists, each element containing all the attributes of one of
#'   the paths

get_paths <- function(file) {
  xml <- xml_ns_strip(read_xml(file))
  paths <- xml_find_all(xml, ".//path")
  lapply(xml_attrs(paths), as.list)
}
