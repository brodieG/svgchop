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

STYLE.PROPS.NORM <- c(
  'fill', 'stroke', 'fill-opacity', 'fill-rule',
  'stroke-width', 'stroke-opacity',
  'opacity',
  'stop-color', 'stop-opacity'
)
STYLE.PROPS.CUM <- c()  # used to think some styles needed to accumulate
STYLE.PROPS <- c(STYLE.PROPS.NORM, STYLE.PROPS.CUM)
STYLE.PROPS.COLOR <- c('fill', 'stroke', 'stop-color')

web.colors <- readRDS(system.file('extdata/web-colors.RDS', package='svgchop'))

#' SVG Color to Hex Code Mapping
#'
#' A list of the 147 known SVG colors from the
#' [W3C SVG 1.1. Spec](https://www.w3.org/TR/SVG11/types.html#ColorKeywords).
#'
#' @export
#' @param character of color keywords (names).
#' @return character vector named with color and with the 6 digit RGB hex code
#'   as the value.  Unmatched color names return NA.
#' @examples
#' svg_colors(c('green', 'plum', 'NOT A REAL COLOR'))
#' length(svg_colors_all()) # 147 named colors

svg_colors <- function(colors) web.colors[tolower(colors)]

#' @rdname svg_colors
#' @export

svg_colors_all <- function() web.colors

#' Report What Styles Are Computed
#'
#' `svgchop` computes a small subset of style attributess based on element
#' attributes, CSS style sheets, and inheritance.  This function returns which
#' styles receive that treatment.
#'
#' @return character names of styles computed by `svgchop`
#' @export
#' @examples
#' styles_computed()

styles_computed <- function() STYLE.PROPS

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
    unlist(lapply(xml_find_all(xml, ".//svg:style", NSMAP), xml_text))
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
##     * Match identifiers of form <element>#<id> or <element>.<class> where
##       <element> may be "*".  Either class or id may be specified, but not
##       both, and only one of each.
## * For rule declarations:
##     * Tokenize by ";" assuming that character does not exist within the
##       declaration
##     * Tokenize by ":" assuming that character does not exist within the
##       declaration name or value.
##
## Return value is a "css" object, a list names by the supported CSS properties,
## which each element a data.frame with a "selector" column and a "value"
## column.

parse_css <- function(x) {
  x <- paste0(x, collapse="\n")
  x <- gsub("/\\*.*?\\*/", "", x)  # strip comments
  m <- regmatches(
    x, gregexec("\\s*([^{]*)\\s*\\{([^}]*?)\\}", x, perl=TRUE)
  )[[1]]

  res <- if(length(m) && !(length(m) %% 3)) {
    rules <- lapply(m[seq(3, length(m), by=3)], parse_css_rule)
    selectors <- lapply(m[seq(2, length(m), by=3)], parse_css_selector)

    # for each rule, we want it linked to the class / id: order so id selectors
    # are last
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
    setNames(
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
  } else {
    warning("CSS style sheet not in recognized format.")
    list()
  }
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
  x <- trimws(x)
  sel <- strsplit(x, ",")[[1]]
  # Actual rule also include <U+00A0> and higher; we approximate that by
  # allowing all digits and word characters, but that will be incomplete.
  # Also, this will match the first element in an ancestry chain.
  m <- regmatches(
    sel,
    regexec(
      "^\\h*(\\*|(?:\\w|[_\\-])*)((?:[.#](?:\\w|[_\\-])+)?)\\h*$",
      sel, perl=TRUE
    )
  )
  bad <- lengths(m) != 3
  if(any(bad))
    warning(
      "CSS selector \"", trimws(gsub('\\s+', ' ', x)),
      "\" contains unrecognized tokens."
    )

  good <- matrix(unlist(m[!bad]), 3)
  # infer "*" if unspecified element selector
  good[2, !nzchar(good[2,])] <- "*"
  # infer "*" if unspecified class/id selector and generate both the class and
  # id selector.  Most of the complication that follows is trying to get the
  # additional generated values re-inserted into the vector in the right order
  # UPDATE: it's not obvious that we need "*.*" and "*#*"?
  postfix <- as.list(good[3,])
  postfix.wild <- !nzchar(good[3,])
  postfix[postfix.wild] <- list(c(".*", "#*"))
  p.lens <- lengths(postfix)
  good.cat <- paste0(rep(good[2,], p.lens), unlist(postfix))

  res <- character(length(m) + sum(postfix.wild))
  ids <- rep(1, length(bad))
  ids[!bad][postfix.wild] <- 2
  bad2 <- rep(bad, ids)
  res[!bad2] <- good.cat
  res
}

style <- function() {
  as.list(setNames(rep(NA, length(STYLE.PROPS)), STYLE.PROPS))
}

## Append Alpha Hex to 6 Digit Hex

append_alpha <- function(color, alpha) {
  if(is.character(color) && is.numeric(alpha)) {
    is.hex <- grepl("^#[0-9a-fA-F]{6}$", color)
    alpha.hex <- as.hexmode(round(pmin(pmax(0, alpha), 1) * 255))
    color[is.hex] <- paste0(color[is.hex], toupper(format(alpha.hex, width=2)))
  }
  color
}

## Track Computed Styles
##
## Normal styles overwrite prior ones.  Cumulative ones need to be tracked so
## they can have a final computation applied at time of styling.  Currently we
## only have opacity, which we could recompute as we go along.

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
  x, style.prev, inline, props, style.sheet, classes, id, name
) {
  # if(x == 'stroke') browser()
  prop <- props[x]
  inline <- inline[x]
  css <- style.sheet[[x]]

  css.lookup <- setNames(css[['value']], css[['selector']])
  lookup <- c(
    character(),
    if(!is.na(prop)) setNames(prop, 'prop'),
    css.lookup,
    if(!is.na(inline)) setNames(inline, 'inline')
  )
  # Match any prop style props first, then any  CSS identifiers that match on
  # class, then that match on id, then that match on element and class, then on
  # element and id, and finally any specified via inline css
  search <- c(
    'prop',
    '*.*', '*#*', sprintf('%s.*', name), sprintf('%s#*', name),
    paste0(rep_len("*.", length(classes)), classes),
    paste0(rep_len("*#", length(id)), id),
    paste0(rep_len(sprintf("%s.", name), length(classes)), classes),
    paste0(rep_len(sprintf("%s#", name), length(id)), id),
    'inline'
  )
  target <- which.max(match(search, names(lookup)))
  if(length(target)) unname(lookup[search[target]]) else NA_character_
}
## Must convert all colors to hex so they can be combined with transparency
## codes.  Lone exception is "none" as we must distinguish it from NA missing as
## the default for fill is to fill in black (i.e. NA == 'black', "none" ==
## "none"). "transparent" is converted to "none".

proc_color <- function(colors) {
  # Short color codes
  colors <- gsub(
    '^#([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])$', '#\\1\\1\\2\\2\\3\\3',
    colors
  )
  # rgb color codes
  rgb.el <- "\\s*(\\d+%?)\\s*"
  rgb.pat <- sprintf("^\\s*rgb\\(%s,%s,%s\\)\\s*$", rgb.el, rgb.el, rgb.el)
  rgb <- grepl(rgb.pat, colors)
  if(any(rgb)) {
    raw <- regmatches(colors[rgb], regexec(rgb.pat, colors[rgb]))
    vals <- vapply(raw, '[', character(3), -1)
    pct <- grepl("%$", vals)
    vals <- array(as.numeric(gsub("%$", "", vals)), dim=dim(vals))
    vals[pct] <- vals[pct] * (255 / 100)
    vals[] <- pmax(pmin(255, round(vals)), 0)
    hex <- matrix(format(as.hexmode(vals), width=2), nrow=nrow(vals))
    colors[rgb] <- paste0('#', hex[1,], hex[2,], hex[3,])
  }
  # url id codes
  is.url <- grepl("^\\s*url\\(#[^\\)]+\\)\\s*$", colors)

  # color-name colors
  not.hex <- !grepl("#[0-9a-fA-F]{6}", colors)
  colors[tolower(colors) == 'transparent'] <- 'none'
  none <- tolower(colors) == 'none'
  not.color <- not.hex & !is.url & !tolower(colors) %in% names(svg_colors_all())
  colors[not.color & !none] <- NA_character_
  colors[!not.color & !none & !is.url] <-
    svg_colors(colors[!not.color & !none & !is.url])

  colors
}
# Convert computed values to friendlier ones
#
# Colors as 6 digit hex codes, defaults accounted for, opacity, etc.

proc_computed <- function(x) {
  # Colors, converting to hex codes
  x[STYLE.PROPS.COLOR] <- lapply(x[STYLE.PROPS.COLOR], proc_color)

  # Defaults / special cases
  if(is.na(x[['fill']]))
    x[['fill']] <- structure('#000000', class="default")
  if(x[['fill']] == 'none') x[['fill']] <- NA_character_
  if(is.na(x[['fill-rule']]))
    x[['fill-rule']] <- structure("nonzero", class="default")
  if(!is.na(x[['stroke']]) && x[['stroke']] == 'none')
    x[['stroke']] <- NA_character_
  if(is.na(x[['stop-color']]))
    x[['stop-color']] <- structure('#000000', class="default")
  if(x[['stop-color']] == 'none') x[['stop-color']] <- NA_character_
  if(is.na(x[['stroke-width']]))
    x[['stroke-width']] <- structure("1", class="default")
  x[['stroke-width']] <- parse_length(x[['stroke-width']])

  # Compute total opacity and report it back.  We do not attach it to an RGB hex
  # code b/c we might want to use it separately.  The `prod` business is
  # leftover from when we thought we should accumulate opacities, which we don't
  # do anymore.
  op <- prod(as.numeric(x[['opacity']]), na.rm=TRUE)
  x[['fill-opacity']] <-
    prod(as.numeric(x[['fill-opacity']]), na.rm=TRUE) * op
  x[['stroke-opacity']] <-
    prod(as.numeric(x[['stroke-opacity']]), na.rm=TRUE) * op
  x[['opacity']] <- NULL

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
  # for things like transparency. UPDATE: this was wrong, it's not clear that
  # there are actually any style attributes that accumulate this way, so we
  # could have left it to the end.  For some reason got confused when dealing
  # with transforms and thinking same thing would happen here.

  classes <-
    if(is.null(xml_attr[['class']])) character()
    else unlist(strsplit(trimws(xml_attr[['class']]), "\\s+"))
  id <- if(is.null(xml_attr[['id']])) character() else xml_attr[['id']]
  name <-
    if(is.null(attr(node, 'xml_name'))) character()
    else attr(node, 'xml_name')

  style.computed <- sapply(
    names(style.sheet),
    compute_prop,
    inline=inline, props=props, classes=classes, id=id,
    style.prev=style.prev, style.sheet=style.sheet,
    name=name,
    simplify=FALSE
  )
  update_style(style.prev, style.computed)
}
parse_inline_style_rec <- function(node, style.prev=style(), style.sheet) {
  style <- parse_inline_style(node, style.prev, style.sheet)
  if(!is.list(node) || !length(node)) {
    attr(node, 'style-computed') <- proc_computed(style)
  } else {
    node[] <- lapply(
      node, parse_inline_style_rec, style.prev=style, style.sheet=style.sheet
    )
  }
  node
}
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

