#' Approximate SVG Elements With Line Segments
#'
#' Parse and convert SVG elements into line segments and supporting meta data.
#' SVG transforms and clip paths are applied, and SVG presentation attributes
#' are computed from style sheets, inline styles and attributes.  The SVG 1.1
#' specification is only loosely followed so do not expect outputs to be exactly
#' the same as in a conforming SVG rendering engine.  This package is
#' experimental and the API will likely change in future versions.  The code is
#' optimized neither for speed nor memory use.
#'
#' The workhorse function is [chop()] and you should look at its documentation
#' to get started, in particular the examples.  [compare_svg()] will give you a
#' sense of how well the approximation performs.
#'
#' @import vetr
#' @importFrom utils globalVariables
#' @name svgchop
#' @docType package

NULL

globalVariables(".")
