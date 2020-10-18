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

## SVG Display Properties We're Tracking

PROPS <- c(
  'fill', 'fill-opacity', 'stroke', 'stroke-opacity', 'stroke-width',
  'opacity'
)

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
  x <- compute_inline_style(x)
  apply_style(x, style.sheet=style.sheet)
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
      lapply(PROPS, function(x) lapply(tmp, '[[', x)),
      function(y) {
        z <- do.call(rbind, y)
        # make sure ids go last for higher priority during matching
        z[order(substr(z[['selector']], 1, 1) == "#"),]
      }
    ),
    PROPS
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

# Track non-Stylesheet Styles
#
# "inline" contains styles recovered from inline "style" properties, and
# "props" those from inline properties such as "fill", etc.  The former have
# higher priority than the style sheet, while the latter lower priority.

chr0 <- setNames(character(), character())

style <- function(
  styles=chr0, props=chr0, classes=character(), ids=character()
) {
  s.t <- p.t <- setNames(character(length(PROPS)), PROPS)
  style.nm <- names(styles)[names(styles) %in% PROPS]
  s.t[style.nm] <- styles[style.nm]
  prop.nm <- names(props)[names(props) %in% PROPS]
  p.t[prop.nm] <- props[prop.nm]

  structure(
    list(inline=s.t, props=p.t, classes=classes, ids=ids), class='style'
  )
}
# For use in vetting

style.tpl <- style()

update_style <- function(old, new) {
  vetr(style.tpl, style.tpl)
  new.s <- nzchar(new[['inline']])
  new.p <- nzchar(new[['props']])
  old[['inline']][new.s] <- new[['inline']][new.s]
  old[['prop']][new.p] <- new[['prop']][new.p]
  old[['classes']] <- c(old[['classes']], new[['classes']])
  old[['ids']] <- c(old[['ids']], new[['ids']])
  old
}

parse_inline_style <- function(node, style.prev=style()) {
  xml_attr <- attr(node, 'xml_attrs')
  if(is.null(xml_attr)) xml_attr <- setNames(list(), character())

  # Retrieve inline style/property and update the style object
  inline <- if(!is.null(xml_attr[['inline']])) {
    parse_css_rule(xml_attr[['inline']])
  } else chr0
  attr_props <- xml_attr[names(xml_attr) %in% PROPS]
  props <- setNames(as.character(attr_props), names(attr_props))
  update_style(
    style.prev,
    style(
      inline, props,
      if(is.null(xml_attr[['class']])) character() else xml_attr[['class']],
      if(is.null(xml_attr[['ids']])) character() else xml_attr[['ids']]
    )
  )
}

compute_inline_style <- function(node, style.prev=style()) {
  style <- parse_inline_style(node, style.prev)
  if(is.matrix(node)) {
    attr(node, 'style-inline') <- style
  } else {
    node[] <- lapply(node, compute_inline_style, style)
  }
  node
}

apply_style <- function(x, style.sheet) {
  style <- attr(x, 'style-inline')
  vet(style.tpl, style, stop=TRUE)

  # match accumulated class vector against property class vector and take the
  # highest index, and have match return 1 for no-match, so that the inlined
  # element at position one is taken if no classes match.

  if(is.matrix(x)) {
    styles.computed <- vapply(
      names(style.sheet),
      function(y) {
        prop <- style[['props']][y]
        inline <- style[['inline']][y]
        lookup <- c(
          if(!is.na(prop)) setNames(prop, 'prop'),
          style.sheet[[y]],
          if(!is.na(inline)) setNames(inline, 'inline'),
        )
        target <- which.max(
          match(
            c(
              'prop',
              paste0(".", style[['classes']]),
              paste0("#", style[['ids']]),
              'inline',
            ),
            names(y)
        ) )
        if(!is.na(target)) lookup['target'] else NA_character_
      },
      character(1L)
    )
    attr(x, 'style-computed') <- styles.computed
    x
  } else {
    x[] <- lapply(x, apply_style, style.sheet)
  }
  x
}
