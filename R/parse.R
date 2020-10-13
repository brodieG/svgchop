## @param x named character vector with properties attached to a polygon

parse_poly <- function(x) {
  x <- as.list(x)
  if(!"points" %in% names(x)) x[['points']] <- ""
  raw <- regmatches(x[['points']], gregexpr("-?[0-9.]+", x[['points']]))[[1]]
  stopifnot(length(raw) %% 2 == 0)
  coord <- matrix(as.numeric(raw), ncol=2, byrow=TRUE)
  # remove sequential duplicates
  coord <- coord[c(TRUE, rowSums(coord[-1L,] == coord[-nrow(coord),]) < 2),]
  coords <- if(nrow(coord)) {
    # close poly if isn't already closed
    if(any(coord[1,] != coord[nrow(coord),])) {
      coord <- rbind(coord, coord[1,])
    }
    data.frame(cmd=c('M', rep('L', nrow(coord) - 1L)), x=coord[,1], y=coord[,2])
  } else {
    data.frame(cmd=character(), x=numeric(), y=numeric())
  }
  x[['coords']] <- list(structure(coords, class=c('subpath','data.frame')))
  x
}
## @inheritParams parse_poly

parse_rect <- function(x) {
  props <- names(x)
  if(any(c('rx', 'ry', 'pathLength') %in% props))
    warning('"r[xy] and pathLength properties on "rect" not supported')

  if(!'x' %in% props) x['x'] <- "0"
  if(!'y' %in% props) x['y'] <- "0"
  if(!all(c('width', 'height') %in% props))
    stop('"rect" requires width and height specified')

  base.props <- c('x', 'y', 'width', 'height')
  coords <- setNames(as.numeric(x[base.props]), base.props)
  if(anyNA(coords))
    stop('"rect" can only be processed if all base properties are numeric')

  xs <- c(
    coords['x'], coords['x'] + coords['width'],
    coords['x'] + coords['width'], coords['x']
  )
  ys <- c(
    coords['y'], coords['y'],
    coords['y'] + coords['height'], coords['y'] + coords['height']
  )
  p <- paste(xs, ys, sep=",", collapse=" ")
  parse_poly(c(points=p, x[!props %in% base.props]))
}

## @param x a list maybe containing 'transform' and 'coords' elements

parse_transform <- function(x, CMS=diag(3)) {
  raw <- [[1L]]
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
}

#' Retrieve SVG Elements From File
#'
#' Pull all paths and polygons out of an SVG file and convert them to x-y
#' coordinates and L, M, and C SVG path commands corresponding to line segments.
#' Originally this was all built around SVG paths, so we're forcing polygons
#' through that pipeline even though we really don't need to.
#'
#' If the document contains multiple SVG elements, only the first will be
#' parsed.  In the future will likely add the option to directly pass an `xml2`
#' node to allow iterating over multiple svgs.
#'
#' @export
#' @importFrom xml2 xml_attrs xml_find_all xml_ns_strip read_xml xml_name
#' @seealso [interp_paths()]
#' @param file an SVG file
#' @param elements character the types of elements to parse, currently only
#'   "path" and "polygon" are supported.
#' @return an "svg_paths" S3 object, which is a list of "subpath" that have been
#'   converted from their original SVG form to a line segments.  The `x`, `y`,
#'   `width`, and `height` values of the outer SVG element recorded in the "box"
#'   attribute.

parse_svg <- function(file) {
  xml <- xml_ns_strip(read_xml(file))
  if(!identical(xml_name(xml), "svg"))
    xml <- xml_find_first(xml, ".//svg")
  if(!identical(xml_name(xml), "svg"))
    stop("Document does not start with an svg node")
  attrs <- xml_attrs(xml)
  width <- height <- x <- y <- NA_real_
  if(all(c('width', 'height') %in% names(attrs))) {
    if(!grepl("^\\d+$", attrs['width']))
      stop("Unrecognize width format ", attrs['width'])
    if(!grepl("^\\d+$", attrs['height']))
      stop("Unrecognize height format ", attrs['height'])
    width <- as.numeric(attrs['width'])
    height <- as.numeric(attrs['height'])
  } else if ('viewBox' %in% names(attrs)) {
    # this isn't right, but appears to work in the couple of examples I've
    # worked with as the width/height and viewbox are the same
    num.rx <- "\\d*\\.?\\d+"
    vb.rx <- sprintf("^\\s*(%s\\s+){3}%s\\s*$", num.rx, num.rx)
    if(!grepl(vb.rx, attrs['viewBox']))
      stop("viewBox attribute in unknown format ", attrs['viewBox'])
    viewbox <- strsplit(trimws(attrs['viewBox']), "\\s+")[[1]]
    x <- as.numeric(viewbox[1])
    y <- as.numeric(viewbox[2])
    width <- as.numeric(viewbox[3])
    height <- as.numeric(viewbox[4])
  }
  if(is.na(x) && all(c('x', 'y') %in% names(attrs))) {
    if(!grepl("^\\d+$", attrs['x']))
      stop("Unrecognize width format ", attrs['x'])
    if(!grepl("^\\d+$", attrs['height']))
      stop("Unrecognize y format ", attrs['y'])
    x <- as.numeric(attrs['x'])
    y <- as.numeric(attrs['y'])
  }
  if(is.na(x)) x <- 0
  if(is.na(y)) y <- 0

  els <- xml_find_all(xml, ".//path|.//polygon|.//rect")
  type <- xml_name(els)
  el.path <- type == 'path'
  el.poly <- type == 'polygon'
  el.rect <- type == 'rect'

  res <- list(length(els))
  res[el.path] <- lapply(xml_attrs(els[el.path]), parse_path)
  res[el.poly] <- lapply(xml_attrs(els[el.poly]), parse_poly)
  res[el.rect] <- lapply(xml_attrs(els[el.rect]), parse_rect)
  res <- parse_transform(res)
  structure(
    res, class='svg_paths', box=c(x, y, width, height),
    type=type
  )
}
