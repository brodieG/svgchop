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

## Properties We're Tracking

props <- c(
  'fill', 'fill-opacity', 'stroke', 'stroke-opacity', 'stroke-width',
  'opacity'
)

## Retrieve All Style Sheets
##
##

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
      lapply(props, function(x) lapply(tmp, '[[', x)),
      function(y) {
        z <- do.call(rbind, y)
        # make sure ids go last for higher priority during matching
        z[order(substr(z[['selector']], 1, 1) == "#"),]
      }
    ),
    props
  )
  res
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

# Track Inline Styles
#
# "style" slot contains styles recovered from inline "style" properties, and
# "props" those from inline properties such as "fill", etc.  The former have
# higher priority than style sheet


style <- function(style=character(), props=character()) {
  s.t <- p.t <- setNames(character(length(props)), props)
  style.nm <- names(styles)[names(styles) %in% props]
  s.t[style.nm] <- style[style.nm]
  prop.nm <- names(props)[names(props) %in% props]
  p.t[prop.nm] <- prop[prop.nm]

  structure(list(style=s.t, props=p.t), class='style')
}
style.tpl <- structure(
  list(
    style=setNames(character(length(props)), props),
    props=setNames(character(length(props)), props)
  ),
  class='style'
)

update_style <- function(old, new) {
  vetr(style.tpl, style.tpl)
  new.s <- nzchar(new[['style']])
  new.p <- nzchar(new[['props']])
  old[['style']][new.s] <- new[['style']][new.s]
  old[['prop']][new.p] <- new[['prop']][new.p]
  old
}

parse_style <- function(node, style.prev=style()) {
  # check style
  # carry along accumulated style
  # carry along accumulated props

  xml_attr <- attr(node, 'xml_attrs')

  if(!is.null(xml_attr[['style']])) {
    style <- update_style(style.prev,)
  }

  mx <- trans.prev[['mx']]
  if(is.null(trans.dat)) {
    cmds.full <- character()
  } else {
    vet(character(1L), trans.dat, stop=TRUE)
    raw <- gregexpr("([a-zA-Z]+)\\s*\\(([^)]*)\\)", trans.dat, perl=TRUE)[[1L]]
    cs <- attr(raw, 'capture.start')
    cl <- attr(raw, 'capture.length')
    proc1 <- substr(rep(trans.dat, length(cs)), c(cs), c(cs + cl - 1))
    cmds <- proc1[seq_len(nrow(cs))]
    vals <- proc1[seq_len(nrow(cs)) + nrow(cs)]
    vals2 <- lapply(
      regmatches(vals, gregexpr("-?[0-9]*\\.?[0-9]+", vals)), as.numeric
    )
    if(any(vapply(vals2, anyNA, TRUE)))
      stop('unparseable parameters in SVG transform command')

    mx <- diag(3)

    for(i in seq_along(cmds)) {
      mx.tmp <- diag(3)
      valsi <- vals2[[i]]
      switch(cmds[i],
        translate={
          if(length(valsi) == 2) {
            mx.tmp[1:2,3] <- valsi
          } else if(length(valsi) == 1) {
            mx.tmp[1,3] <- valsi
          } else stop('Invalid "translate" command')
        },
        rotate={
          mx.tmp <- diag(3)
          if(!length(valsi) %in% c(1, 3)) stop('Invalid "rotate" command')
          ang <- valsi[1] / 180 * pi

          mx.tmp[1:2, 1:2] <- c(cos(ang), sin(ang), -sin(ang), cos(ang))
          # 3 params means translate -> rotate -> untranslate
          if(length(valsi) == 3) {
            trans1 <- trans2 <- diag(3)
            trans1[3, 1:2] <- valsi[2:3]
            trans2[3, 1:2] <- -valsi[2:3]
            mx.tmp <- trans1 %*% mx.tmp %*% trans2
          } else if(length(valsi) != 1)
            stop('Invalid "rotate" command')
        },
        stop('"', cmds[i], '" transformation not supported')
      )
      mx <- mx %*% mx.tmp
      # for posterity...
      cmds.full <- sprintf("%s(%s)", cmds, paste0(vals, collapse=", "))
    }
  }
  trans(mx, append(trans.prev[['trans']], list(cmds.full)))
}

compute_style <- function(x, trans.prev=trans()) {
  trans <- parse_style(x, trans.prev)
  if(is.matrix(x)) {
    trans
  } else {
    lapply(x, compute_transform, trans)
  }
}

apply_style <- function(x, trans.tree) {
  res <- if(is.matrix(x) && inherits(trans.tree, 'trans')) {
    (trans.tree[['mx']] %*% rbind(x, 1))[-3,,drop=FALSE]
  } else if(
    is.list(x) && is.list(trans.tree) && length(x) == length(trans.tree)
  ) {
    Map(apply_transform, x, trans.tree)
  } else {
    stop("Topology of `x` and `trans.tree` not identical")
  }
  attributes(res) <- attributes(x)
  res
}
