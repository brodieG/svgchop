
process_stops <- function(node) {
  stop.children <-
    vapply(node, function(x) identical(attr(x, 'xml_name'), 'stop'), TRUE)

  stops <- node[stop.children]
  stop.style <- lapply(stops, attr, 'style-computed')
  stop.offset <- vapply(stops, "[[", 0, 1)
  stop.color <- vapply(stop.style, '[[', "", 'stop-color')
  stop.opacity <- vapply(stop.style, '[[', "", 'stop-opacity')
  list(offset=stop.offset, color=stop.color, opacity=stop.opacity)
}

common_grad_attr <- function(x) {
  attrs <- attr(x, 'xml_attrs')
  gradientTransform <- diag(3)
  gradientUnits <- "objectBoundingBox"
  spreadMethod <- "pad"
  if(!is.null(attrs[['gradientUnits']]))
    gradientUnits <- attrs[['gradientUnits']]
  if(!is.null(attrs[['gradientTransform']])) {
    # mock the transform object.  Not 100% sure this transform should be
    # computed now; will have ot figure out how it interacts with transform
    # application.
    gradientTransform <-
      parse_transform(structure(NULL, transform=attrs[['gradientTransform']]))
  }
  if(!is.null(attrs[['spreadMethod']]))
    spreadMethod <- attrs[['spreadMethod']]
  list(
    gradientTransform=gradientTransform, gradientUnits=gradientUnits,
    spreadMethod=spreadMethod
  )
}

## Process Gradients
##
## Should be a list with parsed value, and a "stops" object
##
## "stops" object will be list of offsets in 0-1, opacities in 0-1, and colors

process_gradient_linear <- function(node) {
  x1 <- y1 <- 0
  x2 <- y2 <- 1

  attrs <- attr(node, 'xml_attrs')
  if(!is.null(attrs[['x1']])) x1 <- parse_length(attrs[['x1']])
  if(!is.null(attrs[['x2']])) x2 <- parse_length(attrs[['x2']])
  if(!is.null(attrs[['y1']])) y1 <- parse_length(attrs[['y1']])
  if(!is.null(attrs[['y2']])) y2 <- parse_length(attrs[['y2']])

  structure(
    c(
      list(x1=x1, x2=x2, y1=y1, y2=y2, stops=process_stops(node)),
      common_grad_attr(node)
    ),
    class=c('linearGradient', 'gradient'),
    xml_attrs=attrs
  )
}
process_gradient_radial <- function(node) {
  cx <- cy <- .5
  fr <- 0

  attrs <- attr(node, 'xml_attr')
  if(!is.null(attrs[['cx']])) cx <- parse_length(attrs[['cx']])
  if(!is.null(attrs[['cy']])) cy <- parse_length(attrs[['cy']])
  if(!is.null(attrs[['fr']])) fr <- parse_length(attrs[['fr']])

  fx <- cx
  fy <- cy
  if(!is.null(attrs[['fx']])) fx <- parse_length(attrs[['fx']])
  if(!is.null(attrs[['fy']])) fy <- parse_length(attrs[['fy']])

  structure(
    c(
      list(cx=cx, cy=cy, fx=fx, fy=fy, fr=fr, stops=process_stops(node)),
      common_grad_attr(node)
    ),
    class=c('radialGradient', 'gradient')
  )
}

parse_stop <- function(node) {
  pat <- sprintf("^\\s*%s(\\w*)\\s*$", num.pat.core)
  offset <- trimws(xml_attr(node, 'offset'))
  offset <- if(!grepl(pat, offset)){
    NA
  } else {
    m <- regmatches(offset, regexec(pat, offset))[[1]]
    val <- if(m[3] == "%") as.numeric(m[2]) / 100 else as.numeric(m[2])
    max(c(min(c(1, val)), 0))
  }
  structure(offset, class=c('gradient-stop'))
}
