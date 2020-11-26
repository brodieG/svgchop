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
  if('rx' %in% props) sig_u("'rx' property in <rect>")
  if('ry' %in% props) sig_u("'ry' property in <rect>")
  if('pathLenght' %in% props) sig_u("'pathLength' property in <rect>")

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
  if(any(c('pathLength') %in% props)) sig_u("'pathLength' property in <line>")

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
    sig_u("'pathLength' property <circle>/<ellipse>")

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

parse_use <- function(xnode, steps) {
  href <- xml_attr(xnode, 'xlink:href')
  if(is.na(href)) href <- xml_attr(xnode, 'href')
  href <- trimws(href)
  if(!is.na(href) && grepl("^#", href)) {
    ref <- xml_find_first(
      xml_root(xnode),
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
    sig_u("non-id based 'href' in <use>")
    list()
  }
}
process_use_node <- function(node) {
  attrs <- attr(node, 'xml_attrs')
  x <- y <- 0
  if('x' %in% names(attrs)) x <- parse_length(attrs[['x']])
  if('y' %in% names(attrs)) y <- parse_length(attrs[['y']])

  if(x != 0 || y != 0) {
    transform <- paste0("translate(", x, " ", y, ")")
    attrs[['transform']] <-
      if(is.null(attrs[['transform']])) transform
      else paste(attrs[['transform']], transform)
  }
  attr(node, 'xml_attrs') <- attrs
  node
}
#' Approximate SVG Documents With Line Segments
#'
#' Parse and convert SVG elements into line segments and supporting meta data.
#' SVG transforms and clip paths are optionally applied, and SVG presentation
#' attributes are computed from style sheets, inline styles and attributes.  The
#' [SVG 1.1 specification](https://www.w3.org/TR/SVG11/) is only loosely
#' followed so expect divergences from conforming SVG rendering engines.  This
#' function is experimental and the API and structure of the return value will
#' likely change in future versions.  The code is optimized neither for speed
#' nor memory use.
#'
#' @section Details:
#'
#' The primary objective of these functions is to compute vertex coordinates of
#' piecewise linear approximations of SVG display elements so that they may
#' be used elsewhere.  We wrote this code to make it easier to render extruded
#' SVG objects in 3D with
#' [`rayrender`](https://cran.r-project.org/package=rayrender).  See the
#' examples, and the implementation of [plot.svg_chopped()] for ideas on how
#' to extract the data for your own use.
#'
#' In addition to vertex coordinates, this function will attempt to compute
#' styles using an approximation of SVG styling and CSS semantics for a
#' limited set of styles (see the "Styling" section).
#'
#' It is likely best to start by looking at the examples for this function and
#' for [flatten()] to get a sense for how to interact with the outputs.
#'
#' @section Return Value:
#'
#' For `chop`, an "svg_chopped" object, a recursive list that represents the
#' _first_ top-level SVG viewport in the document.  The leaves are usually `2 x n`
#' numeric matrices that contain the X-Y coordinates of the ordered `n`
#' endpoints of the `n - 1` line segments that approximate the SVG element they
#' represent.  These matrices will have attributes attached that modify how they
#' should be interpreted.  If the leaves are not known display elements they
#' will appear as empty lists.  There may be terminal elements that are neither
#' lists nor matrices (e.g. gradient "stop"s).
#'
#' For "chop_all", an "svg_chopped_list" object, a list of "svg_chopped"
#' objects, each representing one of the top-level SVG viewport in the document.
#'
#' Each leaf in the tree will have several attributes attached.  An incomplete
#' list of possible attributes:
#'
#' * "xml_attrs": the original SVG element attributes, which you may retrieve
#'   and parse with your own logic.
#' * "xml_name": the name of the SVG element.
#' * "styles-computed": a list of the parsed and computed element styles (see
#'   the "Styling" section).
#' * "transform-computed": accumulated transformation data (see "Transforms"
#'   section).
#' * "starts": indicates the starting column of sub-paths in paths; paths with
#'   embedded "M" or "m" commands will contain sub-paths.  Will always start
#'   with 1 when present so it is the same length as "closed".  For use with
#'   e.g. [`decido`](https://cran.r-project.org/package=decido) you will need to
#'   drop the first element.
#' * "closed": logical vector indicating whether each subpath is closed (e.g.
#'   ends in a "Z" or "z" command.
#' * "extents": the extents of the element including clipped areas, transformed
#'   if the transform is applied by `svgchop`.
#' * "clip-path": if present, the clip path to apply to the element, in
#'   `polyclip` format.  If you intend to manually apply the clip path to the
#'   leaves instead of letting `chop` do it, you will have to collect all the
#'   ancestor clip paths and AND combine them.  If you are manually applying the
#'   transforms you will need to transform each clip path before combining them
#'   and applying them (see the "Gradients, Patterns, Masks, and Clip Paths"
#'   section).
#'
#' Leaves that are defined within &lt;defs&gt; blocks will inherit from
#' "hidden".
#'
#' Inspecting "svg_chopped*" objects is best done by starting with `str(obj)` as
#' they tend to be very complex.  This package implements [str()] methods that
#' omit attributes by default as there are many of those and it is difficult to
#' see the object structure with them displayed.
#'
#' @section Dimensions:
#'
#' "svg_chopped" objects will have "width", "height", "viewBox", and "extents"
#' attributes.  The first three correspond to the SVG attributes you might find
#' on an "svg" element.  The last is a list containing the range of X and Y
#' coordinates contained within the SVG display elements (after transforms if
#' they are applied, but before clipping).
#'
#' All lengths and coordinates are assumed to be unit-less except for the
#' "width" and "height" attributes on the top-level SVG elements, and the
#' "offset" and "stop-opacity" attributes for gradient stops.  In other words
#' for most elements "px", "em", "cm", "%", etc. values are completely ignored
#' with all lengths assumed to be in user coordinate system units.
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
#' "ellipse", and "path" with all commands in the SVG 1.1 spec are
#' implemented.  "rect" does not support rounded corners, and "ellipse" does not
#' support "auto" values for "rx" and "ry" (that is the default, but we assume
#' 0).  "pathLength" is not supported on any element.
#'
#' XML attribute data is processed to compute X and Y coordinates for a set of
#' vertices that approximates the outline of each element.  These are stored as
#' 2 x n matrices.  The attribute "closed" will be set to a logical vector
#' representing whether the element should be interpreted as a closed polygon or
#' as an open "polyline".  "path" elements may contain sub-paths, and the
#' "closed" vector will contain one entry for each sub-path.  The starting
#' column in the coordinate matrix for each sub-path is recorded in the "starts"
#' attribute.
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
#' "g" elements behave similarly as they do in SVG documents.  Elements not
#' explicitly referenced here are not directly supported and how they are
#' processed is not specified.  Generally though such elements with children
#' will behave like "g" elements, and those without will be omitted.
#'
#' @section Transforms:
#'
#' Only SVG transforms are supported (i.e. not CSS ones).  The transform
#' attribute of every element in the SVG is read, parsed, and accumulated
#' through element generations.  It is then applied to the computed coordinates
#' of the terminal nodes, and to any clip paths attached to the tree (see the
#' "Gradients, Patterns, Masks, and Clip Paths" section).  You may turn off the
#' application of the transforms by setting `transform=FALSE`, in which case you
#' will be responsible for retrieving the transform data from the
#' "transform-computed" attribute of the terminal leaves of the "svg_chopped"
#' objects or the nodes that have clip paths attached.  This attribute will be a
#' "trans" S3 object containing the transformation matrix as the "mx" member and
#' the commands that were processed to produce that matrix as the "cmds" member.
#'
#' "transform-computed" data is accumulated through generations, so you need not
#' re-fetch ancestral transforms if you are manually applying transforms.
#'
#' The rendering agent is responsible for scaling stroke.  For
#' [plot.svg_chopped()] we use the average of the absolute scaling factors from
#' the computed transformation matrix as an approximation.
#'
#' @section Styling:
#'
#' A subset of the [SVG 1.1 styling
#' properties](https://www.w3.org/TR/SVG11/styling.html#SVGStylingProperties)
#' is explicitly computed from the SVG data (see [styles_computed()] for the
#' list).  Values are taken from the properties defined in-line in the SVG
#' elements or its ancestors, internal CSS style sheets, and in-line "style"
#' properties.  The computed style will be attached as the "style-computed"
#' attribute to nodes of the tree.  For inheritable styles the attribute will be
#' attached to the leaves of the tree (i.e the vertex coordinate matrices).  For
#' non inheritable styles they will be attached to the node they are defined on.
#'
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
#' "url(#id)" values are returned as is, dropping any fallback value that
#' follows them.  See the "Gradients, Patterns, Masks, and Clip Paths" for more
#' details on how to handle "url" values.
#'
#' If an element specifies both "opacity", and "stroke-opacity" or
#' "fill-opacity", the latter two are multiplied with the value of "opacity".
#' Since the "opacity" value is thus reflected in "stroke-opacity" and
#' "style-opacity" it is dropped to avoid confusion.
#'
#' @section Gradients, Patterns, Masks, and Clip Paths:
#'
#' All SVG elements that are intended to be referenced via "url(#id)" from
#' within other elements are extracted from the SVG tree and stored as named
#' elements in the list attached as the "url" attribute to the "svg_chopped" and
#' "svg_chopped_list" objects.  In their place in the original tree structure
#' will be empty lists.
#'
#' A common use case for "url" elements are fill styles that use gradients.
#' Typically the fill will be specified as `fill=url(#gradientid)`.  If you see
#' such a value in the "style-computed" attribute of an "svg_chopped" node, you
#' can use e.g. `attr(svg_chopped_object, 'url')[['gradientid']]` to retrieve
#' it.  For a usage example see [approximate_color()] which is used by the
#' "svg_chopped" plot method to approximate a gradient by a single color.
#'
#' Both linear and radial gradients have limited support.  Gradients are parsed
#' and stop style is computed based on where they are defined.  Gradients are
#' attached as members of the "url" attribute of the return value.  All the
#' gradient coordinate values (e.g. "x", "y", "cx", "cy", ...) are assumed to be
#' specified in \[0, 1\], and not as percentages.  "gradientTransform" is
#' computed into a transformation matrix, but nothing else is done with it.
#'
#' Clip paths are computed and attached as the "clip-path" attribute of any node
#' that references them.  This means that, unlike with gradients, they have
#' already been fetched and incorporated into the "svg_chopped" tree
#' (although they remain available in the "url" attribute too).  They are in the
#' format favored by `polyclip` (list with "x" and "y" elements with last point
#' not overlapping first).
#'
#' If `clip` is set to TRUE (default), then the clip path will be applied to the
#' elements, but this will only work well with polygons.  To handle this
#' properly for open paths and similar you will need to run with `clip = FALSE`,
#' make polygons in the shapes of the open paths, e.g. with
#' [polyclip::polylineoffset()], and then retrieve the clipping path from the
#' "clip-path" attribute to apply it yourself.
#'
#' If you wish to manually apply the clip paths, you will need to retrieve all
#' the ancestors and AND combine them yourself.  This is unlike transforms,
#' which are computed accounting for ancestor transforms.  This oddity is
#' required to ensure that clip paths still make sense for the case where
#' transforms are deferred and applied manually later.
#' In the transform deferral case, you will need to apply the corresponding
#' transforms to each ancestor clip path before AND combining them.  The
#' transform data should be attached as the "transform-computed" attribute of
#' any node that also has a "clip-path" attribute.  Note that transforms on the
#' clip path definitions (i.e. those affecting the contents of &lt;clipPath&gt;
#' elements) will always be applied, unlike those of that affect the elements
#' that reference them.
#'
#' "clip-path" attributes on "clipPath" elements are not followed, which is a
#' departure from the spec.  "clipPathUnits" are assumed to be "userSpaceOnUse".
#'
#' @section Unsupported Features:
#'
#' An incomplete list of known unsupported features:
#'
#' * Filters.
#' * Masks.
#' * Patterns.
#' * Plotting of gradients.
#' * Mixed length units, and most non user space units.
#' * Rounded corners or length constraints on paths / elements.
#' * CSS transforms.
#' * Complex CSS selectors.
#' * "rgba" color specification.
#' * Clip path "clipPathUnits" other than "userSpaceOnUse".
#' * Nested SVG viewports.
#' * "preserveAspectRatio" values other than "meet".
#'
#' There are other features that are either unimplemented, incompletely
#' implemented, or incorrectly implemented.  The parser will signal conditions
#' that inherit class "svgchop" when encountering some of these.  See
#' examples for ideas on how to use handlers to examine the problematic SVGs.
#'
#' There is a distinction between features unimplemented in the parser and those
#' unimplemented in the `plot` method.  For example, gradients are mostly
#' implemented in the parser, but the `plot` method is not able to represent
#' them other than as a single color.
#'
#' You can ask `chop` and `chop_all` to report when they encounter unsupported
#' features by setting `warn=TRUE` or `options(svgchop.warn=TRUE), although the
#' reporting is not comprehensive.  Both of these functions always signal
#' conditions inheriting class "svgchop" so you may handle them (see example).
#'
#' @export
#' @seealso [plot.svg_chopped()], [flatten()] for an easier-to-manage data
#'   structure, [get_xy_coords()] for an easy way to get some basic data out of
#'   the "svg_chopped" objects, [styles_computed()] for what styles are actively
#'   processed, [approximate_color()] for how to approximate gradient fills.
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
#'   to the computed element coordinates.  There is a chance this parameter will
#'   become deprecated and we will no longer provide the option to leave
#'   transforms un-applied.  The option complicates code substantially.  If set
#'   to FALSE, you must should also set `clip` to FALSE and apply the clip-paths
#'   yourself.
#' @param clip TRUE (default) or FALSE whether to apply clipping paths to the
#'   output.  See "Gradients, Patterns, Masks, and Clip Paths".
#' @param warn TRUE or FALSE (default) whether to warn when unsupported SVG
#'   features are encountered.  Note the warnings are not comprehensive.  See
#'   "Unsupported Features" section for details.
#' @return an "svg_chopped" S3 object for `chop`, an "svg_chopped_list" S3
#'   object for `chop_all`.  See "Return Value" section for details.
#' @examples
#' ## Chop and plot to demonstrate we reproduce the logo
#' svg <- chop(R_logo())
#' if(interactive()) plot(svg)
#'
#' ## Directly access the internals for our purposes
#' str(svg) # str(svg, give.attr=TRUE) # to show attributes
#' hoop <-  svg[[2]]
#' r <-     svg[[3]]
#' ext <-   attr(svg, 'extents')
#' if(interactive()) {
#'   ## Basic geometry; this is the default R plot method, not
#'   ## the `svgchop` we used first.
#'   plot(
#'     t(hoop), type='l', ann=FALSE, asp=1, axes=FALSE,
#'     xlim=ext[['x']], ylim=rev(ext[['y']])
#'   )
#'   lines(t(r))
#' }
#' if(interactive()) {
#'   ## Let's do better
#'   plot.new()
#'   plot.window(xlim=ext[['x']], ylim=rev(ext[['y']]), asp=1)
#'   r2 <- r
#'   ## Abuse the fact we know the polygons are explicitly closed,
#'   ## so make the last value of each sub-path NA so they are treated
#'   ## as separate by polypath.
#'   r2[,attr(r, 'starts')] <- NA
#'   polypath(t(r2), col='blue')
#'
#'   ## Color not right, let's retrieve actual color
#'   rfill <- attr(r, 'style-computed')[['fill']]
#'   rfill
#'   ## Gah, it's a gradient.  But that's okay.
#'   rfill <- approximate_color(rfill, attr(svg, 'url'))
#'   plot.new()
#'   plot.window(xlim=ext[['x']], ylim=rev(ext[['y']]), asp=1)
#'   polypath(t(r2), col=rfill)
#' }
#' if(interactive()) {
#'   ## We control the geometry, so let's play by drawing the full
#'   ## logo but applying arbitrary transforms (note: SVG transforms
#'   ## are applied automatically).
#'   h2 <- hoop
#'   h2[, attr(hoop, 'starts')] <- NA
#'   hfill <- attr(hoop, 'style-computed')[['fill']]
#'   hfill <- approximate_color(hfill, attr(svg, 'url'))
#'   ## rotation matrices
#'   ang <- pi/16
#'   center <- vapply(ext, mean, 0)
#'   rot1 <- matrix(c(cos(ang), sin(ang), -sin(ang), cos(ang)), 2)
#'   rot2 <- matrix(c(cos(-ang), sin(-ang), -sin(-ang), cos(-ang)), 2)
#'   plot.new()
#'   plot.window(xlim=ext[['x']], ylim=rev(ext[['y']]), asp=1)
#'   polypath(t(rot2 %*% (h2 - center) + center), col=hfill)
#'   polypath(t(rot1 %*% (r2 - center) + center), col=rfill)
#' }
#' ## Run with handlers to intercept errors
#' \dontrun{
#' withCallingHandlers(
#'   svg_gallery(svg_samples('pie-and-arcs')),
#'
#'   ## set a handler on 'svgchop' signals, and
#'   ## inspect stack at moment of problem
#'   svgchop=function(e) recover()
#' )
#' }

chop <- function(
  file, steps=10, transform=TRUE, clip=TRUE,
  warn=getOption('svgchop.warn', FALSE)
) {
  vetr(file=CHR.1, steps=INT.1.POS.STR, transform=LGL.1, clip=LGL.1, warn=LGL.1)
  chop_internal(
    file=file, steps=steps, transform=transform, clip=clip, first.only=TRUE,
    warn=warn
  )[[1]]
}
#' @export
#' @rdname chop

chop_all <- function(
  file, steps=10, transform=TRUE, clip=TRUE,
  warn=getOption('svgchop.warn', FALSE)
) {
  vetr(file=CHR.1, steps=INT.1.POS.STR, transform=LGL.1, clip=LGL.1, warn=LGL.1)
  chop_internal(
    file=file, steps=steps, transform=transform, clip=clip, warn=warn,
    first.only=FALSE
  )
}
chop_core <- function(file, steps, transform, clip, first.only, warn) {
  xml <- try(read_xml(file))
  if(inherits(xml, 'try-error'))
    stop(
      "Argument `file` could not be interpreted as an XML file; ",
      "see prior errors."
    )
  css <- get_css(xml)

  # top level svgs, nested ones will just be consumed in recursive traversal
  xml <-
    if(!identical(xml_name(xml), "svg"))
      xml_find_all(xml, "//svg:svg[not(ancestor::svg:svg)]", NSMAP)
    else list(xml)

  if(!length(xml))
    stop("Document does not contain svg nodes.")

  if(first.only) xml <- xml[1]

  # Extract relevant data from XML and convert to nested R list.  Elements
  # coordinates are computed
  tmp <- lapply(xml, parse_node, steps=steps)

  # Extract and compute styles for terminal nodes
  tmp <- lapply(tmp, process_css, style.sheet=css)

  # Process elements that are used via `url(#id)`, e.g. gradients, patterns,
  # clip paths, masks, and patterns, although currently only gradients and
  # clip-paths are supported.  These are also extracted from tree into the
  # `url` list.
  tmp <- process_url(tmp, transform=transform)
  url <- attr(tmp, 'url')
  attr(tmp, 'url') <- NULL

  # We need to attach some URL objects to the tree, in particular clip-paths
  # so that they may be transformed correctly.  Can only do this once all
  # url references are resolved above.
  tmp <- lapply(tmp, attach_url, url=url)

  # Compute and apply transformations
  tmp <- lapply(tmp, transform_coords, apply=transform)

  # Compute extents (note we do this before clipping, after transform)
  tmp <- lapply(tmp, compute_leaf_extents)
  tmp <- lapply(tmp, update_extents)

  # Apply the `url()` elements.  This is most meaningful for clip paths and
  # patterns as we could apply them here. At this time we ony apply clipping
  if(clip)
    tmp <- lapply(tmp, apply_clip_path, url=url)

  # Attach global objects
  tmp <- lapply(
    tmp, function(x) {
      attr(x, 'url') <- url
      attr(x, 'css') <- css
      x
  } )
  # Return with pretty names
  as.svg_chopped_list(give_names(tmp))
}

chop_internal <- function(file, steps, transform, clip, first.only, warn) {
  sigs <- list(unsupported=integer(), error=integer(), other=integer())
  res <- withCallingHandlers(
    chop_core(file, steps, transform, clip, first.only, warn),
    svgchop=function(e) {
      if(inherits(e, 'svgchop_unsupported')) {
        sigs[['unsupported']][conditionMessage(e)] <<-
          if(is.na(sigs[['unsupported']][conditionMessage(e)])) 1
          else sigs[['unsupported']][conditionMessage(e)] + 1
      } else if (inherits(e, 'svgchop_error')) {
        sigs[['error']][conditionMessage(e)] <<-
          if(is.na(sigs[['error']][conditionMessage(e)])) 1
          else sigs[['error']][conditionMessage(e)] + 1
      } else {
        sigs[['other']][conditionMessage(e)] <<-
          if(is.na(sigs[['other']][conditionMessage(e)])) 1
          else sigs[['other']][conditionMessage(e)] + 1
      }
    }
  )
  if(any(lengths(sigs)) && warn) {
    make_msg <- function(x, msg, file) {
      if(length(x))
        c(
          sprintf("\r%s in '%s':\n", msg, basename(file)),
          paste0(
             "* ", names(x),
            ifelse(x > 1, sprintf(" (%d times)", x), ""),
            collapse="\n"
    ) ) }
    warning(
      c(
        make_msg(
          sigs[['unsupported']], "Unsupported SVG features encountered", file
        ),
        make_msg(
          sigs[['error']], "Errors parsing SVG", file
        ),
        make_msg(
          sigs[['other']], "Messages parsing SVG", file
        )
  ) ) }
  res
}
# Extent calc broken up into steps so we can update them when we subset.
# Note we don't just recompute extents from the terminal leaves after the
# initial computation because those may be modified by clipping.

compute_leaf_extents <- function(x) {
  if(
    is.matrix(x) && !inherits(x, 'hidden') &&
    ncol(x)
  ) {
    attr(x, 'extents') <- list(x=range(x[1,]), y=range(x[2,]))
  } else if (is.list(x) && length(x)) {
    x[] <- lapply(x, compute_leaf_extents)
  }
  x
}
sum_extents <- function(x) {
  if(is.list(x)) {
    Reduce(
      function(A, B) {
        if(is.null(A) && is.null(B)) NULL
        else if(is.null(B)) A
        else if(is.null(A)) B
        else
          list(
            x=range(c(A[['x']], B[['x']])),
            y=range(c(A[['y']], B[['y']]))
          )
      },
      lapply(x, sum_extents)
    )
  } else attr(x, 'extents')
}
update_extents <- function(x) {
  ext <- sum_extents(x)
  attr(x, 'extents') <- ext
  x
}
## Vectorized, parses lengths dropping units.


num.pat.core <- "([+-]?[0-9]*\\.?[0-9]+(?:[Ee][+-][0-9]+)?)"
num.pat <- sprintf("%s(\\w*|%%)", num.pat.core)

parse_length <- function(x) {
  vetr(character())
  x <- trimws(x)
  num.like <- grepl(num.pat, x)
  res <- rep(NA_real_, length(x))
  unit <- character(length(x))

  parsed <- vapply(
    regmatches(x[num.like], gregexec(num.pat, x[num.like], perl=TRUE)),
    '[', character(2), -1
  )
  res[num.like] <- as.numeric(parsed[1,])
  unit[num.like] <- parsed[2,]
  attr(res, 'unit') <- unit
  res
}

is_pct <- function(x) {
  vetr(character())
  grepl(sprintf("^\\s*%s%%\\s*$", num.pat.core), x)
}
# Convert to numeric accounting for percentage signs if any.  Assumes
# other non-pct are in natural numeric format
#
# @param whether to constraint pct values to a range

parse_pct <- function(x, range=c(-Inf,Inf)) {
  vetr(range=numeric(2))
  range <- sort(range)
  is <- is_pct(x)
  res <- numeric(length(x))
  if(any(is)) {
    tmp <- as.numeric(
      sub(sprintf("^\\s*(%s)%%\\s*$", num.pat.core), "\\1", x[is])
    )
    res[is] <- pmin(range[2], pmax(range[1], tmp / 100))
  }
  res[!is] <- as.numeric(x[!is])
  res
}

## For lengths that are pasted together; note parse_length IS vectorized
## e.g. "5 5 5 5"
parse_lengths <- function(x) {
  vetr(CHR.1)
  parse_length(strsplit(trimws(x), "\\s+")[[1]])
}

process_svg_node <- function(node.parsed) {
  attrs <- attr(node.parsed, 'xml_attrs')
  width <- height <- x <- y <- structure(NA_real_, unit="")
  viewbox <- rep(NA_real_, 4)

  if('width' %in% names(attrs)) width <- parse_length(attrs[['width']])
  if('height' %in% names(attrs)) height <- parse_length(attrs[['height']])
  wh.pct <- c(width=FALSE, height=FALSE)
  if(!is.na(width)) wh.pct['width'] <- isTRUE(attr(width, 'unit') == "%")
  if(!is.na(height)) wh.pct['height'] <- isTRUE(attr(height, 'unit') == "%")

  if('x' %in% names(attrs)) x <- parse_length(attrs[['x']])
  if('y' %in% names(attrs)) y <- parse_length(attrs[['y']])

  if(!isTRUE(attr(width, 'unit') %in% c('%', '', 'px')))
    sig_u(sprintf("unit (%s) in <svg width='...'>.", attr(width, 'unit')))
  if(!isTRUE(attr(height, 'unit') %in% c('%', '', 'px')))
    sig_u(sprintf("unit (%s) in <svg height='...'>.", attr(height, 'unit')))
  if(!isTRUE(attr(x, 'unit') %in% c('', 'px')))
    sig_u(sprintf("unit (%s) in <svg x='...'>.", attr(x, 'unit')))
  if(!isTRUE(attr(y, 'unit') %in% c('', 'px')))
    sig_u(sprintf("unit (%s) in <svg y='...'>.", attr(y, 'unit')))
  if(
    'preserveAspectRatio' %in% names(attrs) &&
    'meet' != trimws(attrs['preserveAspectRatio'])
  )
    sig_u(
      sprintf(
        "'preserveAspectRatio' value '%s' unsupported",
        trimws(attrs['preserveAspectRatio'])
    ) )

  if('viewBox' %in% names(attrs)) {
    viewbox <- parse_lengths(attrs[['viewBox']])
    if(length(viewbox) != 4) {
      sig_u("unrecognized viewBox format")
      viewbox <- rep(NA_real_, 4)
    } else if (!all(nzchar(attr(viewbox, 'unit')) == 0)) {
      sig_u("units in <svg viewBox=''>.")
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
  name <- xml_name(node)
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

    # If we're dealing with gardients here, means that they don't have their own
    # stops so have to get others.  Otherwise they are not terminal.
    linearGradient=parse_gradient_terminal(node),
    radialGradient=parse_gradient_terminal(node),
    list()
  )
  res
}
# Required because some elements (e.g. gradients) will come back with the xml
# attrs already set.

merge_xml_attrs <- function(x, attrs) {
  attrs.new <- attr(x, 'xml_attrs')
  if(is.null(attrs.new)) attrs.new <- list()
  attrs.new[names(attrs)] <- attrs
  attr(x, 'xml_attrs') <- attrs.new
  x
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

parse_node <- function(node, steps, in.defs=FALSE, in.clip=FALSE) {
  vetr(structure(list(), class='xml_node'), INT.1.POS.STR, LGL.1)

  res <- if(xml_length(node, only_elements=TRUE)) {
    # Non-terminal node, recurse
    lapply(
      xml_children(node), parse_node, steps=steps,
      in.defs=in.defs || xml_name(node) == 'defs',
      in.clip=in.clip || xml_name(node) == 'clipPath'
    )
  } else {
    # Parse terminal node
    tmp <- parse_element(node, steps)
    if(in.defs) class(tmp) <- c('hidden', class(tmp))
    tmp
  }
  # attach attributes; this should be done before final processing
  res <- merge_xml_attrs(res, xml_attrs(node))
  attr(res, 'xml_name') <- xml_name(node)

  # known unimplemented
  switch(
    xml_name(node),
    filter=,mask=,marker=,pattern=,symbol=,text=
    sig_u(sprintf("'<%s>' not implemented", xml_name(node)))
  )
  if(in.clip && 'clip-path' %in% names(xml_attrs(node)))
    sig_u("'clip-path' attribute in <clipPath> children not implemented")

  switch(
    xml_name(node),
    svg=process_svg_node(res),
    use=process_use_node(res),
    res
  )
}
