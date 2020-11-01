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

# We're going to assume that no one is going to provide a different SVG
# namespace, and if they do, that it's going to be close enough that nothing
# will go horribly wrong.  Same with xlink.  We do this because of the silly
# notion of just naming namespace d1, d2, etc., which means we have no idea
# what namespace d1 actually refers to.

NSMAP <-
  c(svg="http://www.w3.org/2000/svg", xlink="http://www.w3.org/1999/xlink")

## @param x named character vector with properties attached to a polygon

parse_poly <- function(x, close=TRUE) {
  if(!"points" %in% names(x)) x[['points']] <- ""
  raw <- regmatches(x[['points']], gregexpr(num.pat, x[['points']]))[[1]]
  stopifnot(length(raw) %% 2 == 0)
  coords <- matrix(as.numeric(raw), nrow=2)
  # We used to remove sequential duplicates, but that my cause problems
  # downstream so now we don't anymore

  # Close poly if isn't already closed
  if(ncol(coords) && close && any(coords[,1] != coords[,-1]))
    coords <- cbind(coords, coords[,1])

  attr(coords, "closed") <- close
  coords
}

## @inheritParams parse_poly

parse_rect <- function(x) {
  props <- names(x)
  if(any(c('rx', 'ry', 'pathLength') %in% props))
    warning('"r[xy]" and "pathLength" properties on "rect" not supported')

  if(!'x' %in% props) x[['x']] <- "0"
  if(!'y' %in% props) x[['y']] <- "0"
  if(!all(c('width', 'height') %in% props))
    stop('"rect" requires width and height specified')

  base.props <- c('x', 'y', 'width', 'height')
  coords <- setNames(as.numeric(x[base.props]), base.props)
  if(anyNA(coords))
    stop('"rect" can only be processed if all base properties are numeric')

  xs <- c(
    coords['x'],
    coords['x'] + coords['width'],
    coords['x'] + coords['width'],
    coords['x']
  )
  ys <- c(
    coords['y'],
    coords['y'],
    coords['y'] + coords['height'],
    coords['y'] + coords['height']
  )
  p <- paste(xs, ys, sep=",", collapse=" ")
  parse_poly(c(list(points=p), x[!props %in% base.props]))
}
parse_line <- function(x) {
  props <- names(x)
  if(any(c('pathLength') %in% props))
    warning('"pathLength" property on "line" not supported')

  base.props <- c('x1', 'y1', 'x2', 'y2')
  points <- x[base.props]
  points[is.na(points)] <- 0
  p <- paste(points[c(1, 3)], points[c(2, 4)], sep=",", collapse=" ")
  parse_poly(c(list(points=p), x[!props %in% base.props]), close=FALSE)
}
parse_circle <- function(x, steps) {
  x[c('rx', 'ry')] <- x['r']
  props <- names(x)
  parse_ellipse(x, steps)
}
parse_ellipse <- function(x, steps) {
  props <- names(x)
  if(any(c('pathLength') %in% props))
    warning('"pathLength" property "circle/ellipse"')

  # Unspecified attributes set to 0.  this is not quite right, ellipses default
  # to 'auto' rx and ry, which we don't support.  It's right for circles though.

  key.props <- setNames(rep("0", 4), c('cx', 'cy', 'rx', 'ry'))
  x <- c(x, key.props[!names(key.props) %in% props])
  lens <- parse_length(unname(x[c('cx', 'cy', 'rx', 'ry')]))
  lens[is.na(lens)] <- 0
  angles <- seq(0, 2 * pi, length.out=(steps * 2) + 1)
  res <- (lens[3:4] * rbind(cos(angles), sin(angles))) + lens[1:2]
  attr(res, 'closed') <- TRUE
  res
}
## Parse use link
##
## Still requires special handling in the caller to ensure that the transform is
## recorded properly

parse_use <- function(node, steps) {
  href <- xml_attr(node, 'xlink:href')
  if(is.na(href)) href <- xml_attr(node, 'href')
  href <- trimws(href)
  if(!is.na(href) && grepl("^#", href)) {
    ref <- xml_find_first(
      xml_root(node),
      sprintf('.//svg:*[@id="%s"]', sub("^#", "", href)),
      ns=NSMAP
    )
    if(is(ref, "xml_missing")) {
      list()
    } else {
      # Check for potential recursion
      pars <- xml_parents(ref)
      if(any(vapply(pars, identical, TRUE, ref)))
        stop(
          'Possible infinite recursion dectected when substituting "use" ',
          'element with href "', href, '"'
        )
      list(parse_node(ref, steps))
    }
  } else {
    warning('"use" element with non-ID based "href"')
    list()
  }
}
process_use_node <- function(node.parsed) {
  attrs <- attr(node.parsed, 'xml_attrs')
  x <- y <- 0
  if('x' %in% names(attrs)) x <- parse_length(attrs[['x']])
  if('y' %in% names(attrs)) y <- parse_length(attrs[['y']])

  if(x != 0 || y != 0) {
    transform <- paste0("translate(", x, " ", y, ")")
    attrs[['transform']] <-
      if(is.null(attrs[['transform']])) transform
      else paste(attrs[['transform']], transform)
  }
  attr(node.parsed, 'xml_attrs') <- attrs
  node.parsed
}
#' Convert SVG Elements to Polygons or Polylines
#'
#' Parse and convert SVG elements into polygons.  SVG transforms are applied to
#' the polygon coordinates, and SVG presentation attributes are computed from
#' style sheets, inline styles and attributes, and are attached as the
#' "style-computed" R attribute.  The SVG 1.1 specification is only loosely
#' followed so do not expect outputs to be exactly the same as in a conforming
#' SVG rendering engine.  This function is experimental and the API and
#' structure of the return value will likely change in future versions.  The
#' code is optimized neither for speed nor memory use.
#'
#' @section Details:
#'
#' The primary objective of this function is to compute vertex coordinates for
#' polygons and polylines that approximate SVG display elements so that they may
#' be used elsewhere.  We wrote this code to make it easier to render extruded
#' SVG objects in 3D with
#' [`rayrender`](https://cran.r-project.org/package=rayrender).  See the
#' implementation of the `plot.svg_chopped` for ideas on how to extract the data
#' for your own use.
#'
#' In addition to vertex coordinates, this function will attempt to compute
#' styles using an approximation of SVG styling and CSS semantics for a
#' limited set of the styles (see the "Styling" section).
#'
#' The return value is an "svg_chopped_list" object, a list of
#' "svg_chopped" objects with some additional meta data attached as attributes.
#' Because HTML documents may contain multiple top level SVG viewports this
#' function always returns an "svg_chopped_list", even for the common case where
#' there is only one viewport.
#'
#' Each "svg_chopped" object is a recursive list representing a top-level SVG
#' viewport.  If the leaves of the tree are known display elements they will
#' appear as `2 x n` numeric matrices.  The matrices contain the X-Y coordinates
#' of the ordered `n` endpoints of the `n - 1` line segments that the polygon or
#' path representation of the SVG elements comprise.  These matrices may have
#' attributes attached that modify how they should be interpreted.  See
#' "Elements" section.  If the leaves are not known display elements they will
#' appear as empty lists.  There may be terminal elements that are neither lists
#' nor matrices (e.g. gradient "stop"s).
#'
#' SVG meta data such as style sheets, gradients, patterns, may be removed from
#' the recursive structure and either re-inserted as attributes to relevant
#' child nodes, or stored under the "url" attribute of the "svg_chopped" and
#' "svg_chopped_list" objects.  This "url" attribute will be the same for the
#' "svg_chopped_list" object and all its child "svg_chopped" objects.  See the
#' "Styling", "Gradients", and "Patterns, Masks, and Clip Paths" sections for
#' more discussion of this.
#'
#' Almost all the data present in the SVG document is retained as part of the
#' recursive list structure of the return value.  In particular, the list
#' "incarnation" of each XML node will have "xml_name" and "xml_attrs"
#' attributes containing respectively the element name and attributes.  You can
#' retrieve them and parse them with your own logic if so desired.
#'
#' @section Lengths:
#'
#' All lengths and coordinates are assumed to be unit-less except for the
#' "width" and "height" attributes on the top-level SVG elements, and the
#' "offset" attribute for gradient stops.  In other words for most elements
#' "px", "em", "cm", "%", etc. values are completely ignored with all lengths
#' assumed to be in user coordinate system units.
#'
#' The "width" and "height" attributes of the top-level SVG elements will be
#' interpreted as display device percentages if "%" units are used, or pixels if
#' they are not.  The "offset" attribute to gradient stops will be interpreted
#' as percentages if "%" units are used, or as unit-less numbers otherwise.
#'
#' Future releases may switch to preserving units with e.g. [grid::unit()].
#'
#' @section Elements:
#'
#' The basic elements "line", "rect", "polygon", "polyline", "circle",
#' "ellipse", and "path" (with all commands in the SVG 1.1 spec) are
#' implemented.  "rect" does not support rounded corners, and "ellipse" does not
#' support "auto" values for "rx" and "ry" (that is the default, but we assume
#' 0).  "pathLength" is not supported on any element.
#'
#' XML attribute data is processed to compute x and y coordinates for a set of
#' vertices that approximates the outline of each element.  These are stored as
#' 2 x n matrices.  The attribute "closed" will be set to a logical vector
#' representing whether the element should be interpreted as a closed polygon or
#' as an open "polyline".  "path" elements may contain sub-paths, and the
#' "closed" vector will contain one entry for each sub-path.  The starting
#' column in the coordinate matrix for each sub-path is recorded in the "starts"
#' attribute.
#'
#' "g" elements act as containers for child elements and convey their properties
#' to them.
#'
#' The "use" element is supported, but only if the "xlink:href" or "href"
#' elements point to the id of an element within the same document.  Support is
#' also non-conforming (not that anything in this package is truly
#' conforming...) as CSS selector matching behaves as if the cloned copy of the
#' object were a full DOM child of the "use" element.  However, this
#' non-conformance is likely superseded by the very limited CSS selector
#' implementation (see "Styling" section).  The "use" element will be treated
#' as if it were a "g" element with the referenced element as a child and the
#' "x" and "y" attributes specified as a translate transform.
#'
#' Elements not explicitly referenced here are not directly supported and how
#' they are processed is not specified.  Generally though such elements with
#' children will behave like "g" elements, and those without will be omitted.
#'
#' @section Transforms:
#'
#' Only SVG transforms are supported (i.e. not CSS ones).  The transform
#' attribute of every element in the SVG is read, parsed, and accumulated
#' through element generations.  It is then applied to the computed coordinates
#' of the terminal nodes.  You may turn off the application of the transforms by
#' setting `transform=FALSE`, in which case you will be responsible for
#' retrieving the transform data from the "transform-computed" attribute of the
#' terminal leaves of the "svg_chopped" objects.  This attribute will be a
#' "trans" S3 object containing the transformation matrix as the "mx" member and
#' the commands that were processed to produce that matrix as the "cmds" member.
#'
#' The rendering agent will be responsible for scaling stroke.  For
#' [plot.svg_chopped()] we use the average of the absolute scaling factors from
#' the computed transformation matrix.
#'
#' @section Styling:
#'
#' A subset of the [SVG 1.1 styling
#' properties](https://www.w3.org/TR/SVG11/styling.html#SVGStylingProperties)
#' is explicitly computed from the SVG data (see [styles_computed()] for the
#' list).  Values are taken from the properties defined in-line in the SVG
#' elements or its ancestors, CSS style sheets, and in-line "style" properties.
#' CSS selector support is limited to direct match lookups on
#' "&#lt;element&gt;.&lt;class&gt;" or "&lt;element&gt;#&lt;id&gt;" where "*"
#' may be used as a wild card.  Selector hierarchies, properties, or anything
#' other than basic selectors is not supported.  For example, the following
#' selectors are supported:
#'
#' * "*"
#' * "*.class"
#' * "rect.class"
#' * "rect#id"
#' * ".class"
#' * "#id"
#'
#' But these are not:
#'
#' * "g class"        (element of class "class" a descendant of a "g" element)
#' * "*.class.klass"  (two classes)
#'
#' Style sheets are parsed with regex, so parsing may fail if you have
#' particularly pathological text therein.
#'
#' Styles, classes, and ids are accumulated through element generations and
#' computed into the "styles-computed" attribute of the terminal nodes, which is
#' a list with scalar elements representing the computed style values.  Missing
#' or uncomputable styles are reported as NA, except in the cases where the spec
#' defines default values or it is convenient for us to assert a default.
#' Default values will have class "default".  The computation is an
#' approximation of what the spec mandates.
#'
#' Fill and stroke values, with three exceptions, are returned as 6 digit
#' hex-codes or NA so that it is easy to append alpha values derived from the
#' opacity values.  Supported color formats are 6 digit hex, 3 digit hex, named
#' colors in [svg_colors_all()] (the 147 named SVG 1.1 colors), and `rgb(x,y,z)`
#' where `x`, `y`, and `z` are numeric or percentage values as per the CSS spec.
#' `url(#id)` values are returned as is.  Supported external styling such as
#' gradients will be recorded as part of the "svg_chopped_list" object and may
#' be retrieved with the [svg_url()] function.
#'
#' If an element specifies both "opacity" and "stroke-opacity" or
#' "fill-opacity", the latter two are multiplied with the value of "opacity".
#' Since the "opacity" value is thus reflected in "stroke-opacity" and
#' "style-opacity" it is dropped to avoid confusion.
#'
#' @section Gradients:
#'
#' Both linear and radial gradients have limited support.  Gradients are parsed
#' and stop style is computed based on where they are defined.  "href" or
#' "xlink:href" attributes are not followed.  Gradients are attached as members
#' of the "url" attribute of the return value.  All the gradient coordinate
#' values (e.g. "x", "y", "cx", "cy", ...) are assumed to be specified in \[0,
#' 1\], and not as percentages.
#'
#' Unlike with typical opacity attributes "stop-opacity" is not accumulated
#' nor affected by any parent element "opacity" values, under the assumption
#' that "stop" elements are unlikely to be nested.
#'
#' "gradientTransform" is computed into a transformation matrix, but nothing
#' else is done with it.
#'
#' The `plot` method for "svg_chopped" objects will use [approximate_fill()] to
#' compute a single color from the gradient data.
#'
#' @section Patterns, Masks, and Clip Paths:
#'
#' These are collected under the "url" attribute of the return value, and while
#' they may be processed in some way or other, it is not specified how.  More
#' support for these instructions may be added in the future.  Or not.
#'
#' @export
#' @seealso [plot.svg()], [flatten()] for an easier-to-manage data structure,
#'   [styles_computed()] for what styles are actively processed.
#' @importFrom xml2 xml_attrs xml_find_all xml_ns_strip read_xml xml_name
#'   xml_text xml_length xml_children xml_attr xml_find_first xml_root
#'   xml_parents
#' @param file an HTML or other XML based text file containing SVG elements.
#' @param steps integer(1L) > 0, how many line segments to use to approximate
#'   Bézier curves, arcs, ellipses, and circles.  For Bézier curves, it is how
#'   many segment each individual curve gets, so sequences of Bézier curves will
#'   get that many for each curve in the sequence.  For arcs, ellipses, and
#'   circles, it is how many segments per 180 degrees of arc.  The hope is that
#'   in the future this parameter will be deprecated in favor of tolerance
#'   based ones.
#' @param transform TRUE (default) or FALSE whether to apply the transformation
#'   to the computed element coordinates.
#' @return an "svg_chopped_list" S3 object, which is a list of "svg_chopped"
#'   objects.  See "Details".
#' @examples
#' svg <- process_svg(file.path(R.home(), 'doc', 'html', 'Rlogo.svg'))
#' if(interactive()) plot(svg)

process_svg <- function(file, steps=10, transform=TRUE) {
  vetr(CHR.1, INT.1.POS.STR, LGL.1)
  xml <- try(read_xml(file))
  if(inherits(try, 'try-error'))
    stop(
      "Argument `file` could not be interpreted as an XML file; ",
      "see prior errors"
    )
  css <- get_css(xml)

  # top level svgs, nested ones will just be consumed in recursive traversal
  xml <-
    if(!identical(xml_name(xml), "svg"))
      xml_find_all(xml, "//svg:svg[not(ancestor::svg:svg)]", NSMAP)
    else list(xml)

  if(!length(xml))
    stop("Document does not contain svg nodes")

  # Extract relevant data from XML and convert to nested R list.  Elements
  # coordinates are computed
  tmp <- lapply(xml, parse_node, steps=steps)

  # Extract and compute styles for terminal nodes
  tmp <- lapply(tmp, process_css, style.sheet=css)

  # Process elements that are used via `url(#id)`, e.g. gradients, patterns,
  # clip paths, masks, and patterns, although currently only gradients are
  # supported.  These are also extracted from tree into the `url` list.
  tmp <- process_url(tmp)
  url <- attr(tmp, 'url')
  attr(tmp, 'url') <- NULL

  # Apply the `url()` elements.  This is most meaningful for clip paths and
  # patterns as we could in theory apply them ourselves here, although probably
  # on an optional basis should we want the graphical device to do the work.
  #
  # At this time we don't apply any of the url elements directly

  # Apply transformations
  tmp <- lapply(tmp, transform_coords, apply=transform)

  # compute extents
  get_coords <- function(obj, coord)
    if(is.matrix(obj)) obj[coord,]
    else if(is.list(obj) && length(obj)) lapply(obj, get_coords, coord)

  tmp <- lapply(
    tmp,
    function(x) {
      xs <- range(c(0, unlist(lapply(x, get_coords, 1))))
      ys <- range(c(0, unlist(lapply(x, get_coords, 2))))
      attr(x, 'extents') <- list(x=xs, y=ys)
      attr(x, 'url') <- url
      attr(x, 'css') <- css
      x
  } )
  structure(
    give_names(tmp),
    class='svg_chopped_list',
    url=url, css=css
  )
}
num.pat.core <- "(-?\\d*\\.?\\d+)"
num.pat <- sprintf("%s(?:\\w|%%)*", num.pat.core)

## Vectorized, parses lengths dropping units.

parse_length <- function(x) {
  vetr(character())
  num.like <- grepl(sprintf("^\\s*%s\\s*$", num.pat), x)
  res <- rep(NA_real_, length(x))
  res[num.like] <- as.numeric(
    sub(sprintf("^.*?%s.*$", num.pat), "\\1", x[num.like], perl=TRUE)
  )
  if(!all(num.like))
    warning(
      "Some lengths ", paste0(deparse(x[!num.like]), collapse="\n"),
      " could not be parsed into numbers."
    )
  res
}

is_pct <- function(x) {
  vetr(character())
  grepl(sprintf("^\\s*%s%%\\s*$", num.pat.core), x)
}
## For lengths that are pasted together; note parse_length IS vectorized
## e.g. "5 5 5 5"
parse_lengths <- function(x) {
  vetr(CHR.1)
  parse_length(strsplit(trimws(x), "\\s+")[[1]])
}

process_svg_node <- function(node.parsed) {
  attrs <- attr(node.parsed, 'xml_attrs')
  width <- height <- x <- y <- NA_real_
  viewbox <- rep(NA_real_, 4)

  if('width' %in% names(attrs)) width <- parse_length(attrs[['width']])
  if('height' %in% names(attrs)) height <- parse_length(attrs[['height']])
  wh.pct <- c(width=FALSE, height=FALSE)
  if(!is.na(width)) wh.pct['width'] <- is_pct(attrs[['width']])
  if(!is.na(height)) wh.pct['height'] <- is_pct(attrs[['height']])

  if('x' %in% names(attrs)) x <- parse_length(attrs[['x']])
  if('y' %in% names(attrs)) y <- parse_length(attrs[['y']])

  if ('viewBox' %in% names(attrs)) {
    viewbox <- parse_lengths(attrs[['viewBox']])
    if(length(viewbox) != 4) {
      warning("Unrecognized viewBox format")
      viewbox <- rep(NA_real_, 4)
    }
  }
  if(is.na(x)) x <- 0
  if(is.na(y)) y <- 0

  # return
  structure(
    node.parsed,
    class='svg_chopped',
    viewBox=viewbox,
    x=x, y=y, width=width, height=height, wh.pct=wh.pct
  )
}

parse_element <- function(node, steps) {
  attrs <- xml_attrs(node)
  name <- tolower(xml_name(node))

  res <- switch(name,
    path=parse_path(attrs, steps),
    polygon=parse_poly(attrs),
    polyline=parse_poly(attrs, close=FALSE),
    line=parse_line(attrs),
    rect=parse_rect(attrs),
    circle=parse_circle(attrs, steps),
    ellipse=parse_ellipse(attrs, steps),
    use=parse_use(node, steps),
    stop=parse_stop(node),
    list()
  )
  res
}

## Parse a Node and All It's Children
##
## Given a single XML node, recurse through it and all children parsing any SVG
## element or path data encountered into X-Y line segment coordinates.  All
## nodes retain their XML attributes and XML name as the "xml_attr" and
## "xml_name" R attributes.
##
## Something to consider: use the 'xml_name' as the parent list names to make it
## easier to see what each element is...
##
## @param steps
## @param defs tracks when we're in a def tag
## @return A nested list with data.frames containing line segment X-Y coords are
##   the leaves.  Branches that end in empty lists are possible.  XML attributes
##   and names are retained as R attributes to each node.

parse_node <- function(node, steps, defs=FALSE) {
  vetr(structure(list(), class='xml_node'), INT.1.POS.STR, LGL.1)

  res <- if(xml_length(node, only_elements=TRUE)) {
    # Non-terminal node, recurse
    lapply(
      xml_children(node), parse_node, steps=steps,
      defs=defs || tolower(xml_name(node)) == 'defs'
    )
  } else {
    # Parse terminal node
    tmp <- parse_element(node, steps)
    if(defs) class(tmp) <- c('hidden', class(tmp))
    tmp
  }
  # attach attributes; this should be done before final processing
  attr(res, 'xml_attrs') <- as.list(xml_attrs(node))
  attr(res, 'xml_name') <- xml_name(node)
  switch(
    tolower(xml_name(node)),
    svg=process_svg_node(res),
    use=process_use_node(res),
    res
  )
}
