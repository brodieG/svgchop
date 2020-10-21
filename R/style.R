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

## SVG Display Properties We're Tracking.  Some are cumulative and require
## special handling (only transparency ones for the time being).

STYLE.PROPS.NORM <- c('fill', 'stroke', 'stroke-width')
STYLE.PROPS.CUM <- c('fill-opacity', 'stroke-opacity', 'opacity')
STYLE.PROPS <- c(STYLE.PROPS.NORM, STYLE.PROPS.CUM)

## Determine What Styles Apply to Each Element
##
## Parses and collects CSS classes, inline style properties, and SVG display
## properties, and for each element computes which CSS rules apply.
##
## @param x "svg_chopped" object
## @return "svg_choped" object with a "style-computed" attribute attached, which
##   is a character vector of SVG display attributes with those attributes as
##   the names and the values as the values.

process_css <- function(x, style.sheet) {
  vetr(
    structure(list(), class='svg_chopped'), structure(list(), class='css')
  )
  parse_inline_style_rec(x, style.sheet=style.sheet)
}

## Retrieve and Parse All CSS Style Sheets
##
## Operating under the assumption that any style sheet, anywhere on the page
## could affect CSS anywhere.
##
## @param xml xml2 "xml_document" object.
## @return character vector of all contents (presumably CSS rules) across all
##   style sheets in `xml`

get_css <- function(xml) {
  parse_css(
    unlist(lapply(xml_find_all(xml, ".//style"), xml_text))
  )
}
## Parse a CSS Sheet
##
## We implement a naive regex based CSS parser that will work in some limited
## well behaved cases, i.e. simple class names with no hierarchy whatsoever.
##
## For parse_css, the input should be the character representation of a single
## style sheet.
##
## * Remove all comments (under the assumption that the '*/' token may not be
##   contained in any form within a comment)
## * Tokenize with "selector { rule }" where we assume neither "selector"
##   nor "rule" may contain curly braces ("{}").
## * For identifier:
##     * Tokenize by ",", assuming that character only exists to separate
##       sub-selectors
##     * Match classes with "^\\s*\\*?\\.[a-zZ-Z\-_]\\s*$", that is, only
##       simple ASCII classes with no hierarchy will be matched.
## * For rule declarations:
##     * Tokenize by ";" assuming that character does not exist within the
##       declaration
##     * Tokenize by ":" assuming that character does not exist within the
##       declaration name or value.

parse_css <- function(x) {
  x <- paste0(x, collapse="\n")
  x <- gsub("/\\*.*?\\*/", "", x)  # strip comments
  m <- regmatches(
    x, gregexec("\\s*([^{]*)\\s*\\{([^}]*?)\\}", x, perl=TRUE)
  )[[1]]
  if(!length(m) || length(m) %% 3)
    stop("CSS style sheet not in recognized format.")

  rules <- lapply(m[seq(3, length(m), by=3)], parse_css_rule)
  selectors <- lapply(m[seq(2, length(m), by=3)], parse_css_selector)

  # for each rule, we want it linked to the class / id:
  # order so id selectors are last
  #
  # rule_name - class/id - value
  # rule_name - selector - value

  tmp <- Map(
    function(rule, selector) {
      nm <- rep(names(rule), each=length(selector))
      rl <- rep(unname(rule), each=length(selector))
      sel <- rep(selector, length(rule))
      split(data.frame(selector=sel, value=rl), nm)
    },
    rules,
    selectors
  )
  res <- setNames(
    lapply(
      lapply(STYLE.PROPS, function(x) lapply(tmp, '[[', x)),
      function(y) {
        z <- do.call(rbind, y)
        # make sure ids go last for higher priority during matching
        z[order(substr(z[['selector']], 1, 1) == "#"),]
      }
    ),
    STYLE.PROPS
  )
  structure(res, class="css")
}
parse_css_rule <- function(x) {
  # Need to do this again b/c this may get called for inline style
  x <- gsub("/\\*.*?\\*/", "", x)  # strip comments
  m <- regmatches(
    x,
    gregexec(
      "\\h*([0-9a-zA-Z_\\-]+)\\h*:\\h*([^;\n]*)\\h*(?:[;\n]|$)", x, perl=TRUE
  ) )[[1]]
  if(!length(m) || length(m) %% 3)
    stop("CSS style sheet not in recognized format.")

  setNames(m[seq(3, length(m), by=3)], m[seq(2, length(m), by=3)])
}
parse_css_selector <- function(x) {
  sel <- strsplit(x, ",")[[1]]
  m <- regmatches(
    sel, regexec("^\\h*\\*?(?:([.#][0-9a-zA-Z_\\-]+))", sel)
  )
  if(!all(lengths(m) == 2))
    stop("CSS selector not in recognized format.")

  vapply(m, '[', "", 2)
}

style <- function() {
  as.list(setNames(rep(NA, length(STYLE.PROPS)), STYLE.PROPS))
}

# Track Computed Styles
#
# Normal styles overwrite prior ones.  Cumulative ones need to be tracked so
# they can have a final computation applied at time of styling.  Currently we
# only have opacity, which we could recompute as we go along.

update_style <- function(old, new) {
  vetr(list(), list())

  new.no.na <- names(new)[!vapply(new, anyNA, TRUE)]
  o.n <- names(old)
  o.n.norm <- o.n[o.n %in% new.no.na & o.n %in% STYLE.PROPS.NORM]
  o.n.cum <- o.n[o.n %in% names(new) & o.n %in% STYLE.PROPS.CUM]

  old[o.n.norm] <- new[o.n.norm]
  old[o.n.cum] <- Map(c, old[o.n.cum], new[o.n.cum])
  old
}
## Compute Value for Property
##
## Given a property `x`, a "class", "id", style sheet, presentation attributes,
## and parent computed style, compute style for that property.
##
## Implicit in this approach is that class resolution for the parents can be
## completely conveyed by the prior style.

compute_prop <- function(
  x, style.prev, inline, props, style.sheet, classes, id
) {
  prop <- props[x]
  inline <- inline[x]
  css <- style.sheet[[x]]

  css.lookup <- setNames(css[['value']], css[['selector']])
  lookup <- c(
    character(),
    if(!is.na(prop)) setNames(prop, 'prop'), css.lookup,
    if(!is.na(inline)) setNames(inline, 'inline')
  )
  search <- c(
    'prop',
    paste0(rep_len(".", length(classes)), classes),
    paste0(rep_len("#", length(id)), id),
    'inline'
  )
  target <- which.max(match(search, names(lookup)))
  if(length(target)) unname(lookup[search[target]]) else NA_character_
}
proc_color <- function(colors) {
  colors <- gsub(
    '^#([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])$', '#\\1\\1\\2\\2\\3\\3',
    colors
  )
  not.hex <- !grepl("#[0-9a-fA-F]{6}", colors)
  not.color <- !colors[not.hex] %in% colors()
  colors[not.hex][not.color] <- NA_character_
  colors[not.hex][!not.color] <-
    rgb(t(col2rgb(colors[not.hex][!not.color])), maxColorValue=255)
  colors

}
proc_computed <- function(x) {
  # Colors, converting to hex codes
  x[c('fill', 'stroke')] <- lapply(x[c('fill', 'stroke')], proc_color)

  # Compute total opacity and report it back.  We do not attach it to an RGB hex
  # code b/c we might want to use it separately
  op <- prod(as.numeric(x[['opacity']]), na.rm=TRUE)
  x[['fill-opacity']] <-
    prod(as.numeric(x[['fill-opacity']]), na.rm=TRUE) * op
  x[['stroke-opacity']] <-
    prod(as.numeric(x[['stroke-opacity']]), na.rm=TRUE) * op

  x
}
parse_inline_style <- function(node, style.prev=style(), style.sheet) {
  xml_attr <- attr(node, 'xml_attrs')
  if(is.null(xml_attr)) xml_attr <- setNames(list(), character())

  # Retrieve inline style/property and update the style object
  inline <- if(!is.null(xml_attr[['style']])) {
    parse_css_rule(xml_attr[['style']])
  } else setNames(character(), character())

  attr_props <- xml_attr[names(xml_attr) %in% STYLE.PROPS]
  props <- setNames(as.character(attr_props), names(attr_props))

  # Cumulative attributes need to resolve style sheet conflicts now.  Initially
  # we defered computation of class styles to the end, but we have to do it here
  # for things like transparency.

  classes <-
    if(is.null(xml_attr[['class']])) character()
    else unlist(strsplit(trimws(xml_attr[['class']]), "\\s+"))
  id <- if(is.null(xml_attr[['id']])) character() else xml_attr[['id']]

  style.computed <- sapply(
    names(style.sheet),
    compute_prop,
    inline=inline, props=props, classes=classes, id=id,
    style.prev=style.prev, style.sheet=style.sheet,
    simplify=FALSE
  )
  update_style(style.prev, style.computed)
}
parse_inline_style_rec <- function(node, style.prev=style(), style.sheet) {
  style <- parse_inline_style(node, style.prev, style.sheet)
  if(is.matrix(node)) {
    attr(node, 'style-computed') <- proc_computed(style)
  } else {
    node[] <- lapply(
      node, parse_inline_style_rec, style.prev=style, style.sheet=style.sheet
    )
  }
  node
}

