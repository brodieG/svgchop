#' Global Regexec
#'
#' A mocked up version of a global [regexec()].  Only works in PERL mode.  One
#' day we'll look at submitting this to r-devel.
#'
#' When fed to [base::regmatches()], each entry in the result list will either
#' be 0 long character vector if no-matches, or a vector that has n x m entries
#' where n is number of match groups + 1.  So each "n" represents one full match
#' and the matching groups.
#'
#' @noRd
#' @export
#' @inheritParams base::grep

gregexec <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE, method = 'matrix') {
    stopifnot(perl)
    dat <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                    fixed = fixed, useBytes = useBytes, perl = TRUE)
    capt.attr <- c('capture.start', 'capture.length', 'capture.names')
    process <- function(x) {
        if(anyNA(x) || any(x < 0)) y <- x
        else {
            # we want to interleave matches with captures
            y <- t(cbind(x, attr(x, "capture.start"), deparse.level=0L))
            attributes(y)[names(attributes(x))] <- attributes(x)
            attr(y, "match.length") <-
                t(cbind(attr(x, "match.length"), attr(x, "capture.length"),
                        deparse.level=0L))
            attributes(y)[capt.attr] <- NULL
        }
        y
    }
    lapply(dat, process)
}
#' @export

gregexec2a <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE, method = 'matrix') {
    stopifnot(perl)
    dat <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                    fixed = fixed, useBytes = useBytes, perl = TRUE)
    capt.attr <- c('capture.start', 'capture.length', 'capture.names')
    process <- function(x) {
        if(anyNA(x) || any(x < 0)) y <- x
        else {
            # we want to interleave matches with captures
            y <-
                asplit(cbind(x, attr(x, "capture.start"), deparse.level=0L), 1L)
            attributes(y)[names(attributes(x))] <- attributes(x)
            attr(y, "match.length") <-
                asplit(cbind(attr(x, "match.length"), attr(x, "capture.length"),
                             deparse.level=0L), 1L)
            attributes(y)[capt.attr] <- NULL
        }
        y
    }
    lapply(dat, process)
}
#' @export

gregexec1 <- gregexec

#' @export

gregexec2 <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = FALSE) {
    dat <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                    fixed = fixed, useBytes = useBytes, perl = perl)
    g <- lapply(regmatches(text, dat),
                regexec, pattern = pattern, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes)
    Map(function(global, local) Map(`+`, local, global - 1L), dat, g)
}
#' @export

regmatches1 <- function(x, m, invert = FALSE) {
    res <- regmatches(x = x, m = m, invert = invert)
    Map(function(x, y) { dim(x) <- dim(y); x }, res, m)
}
#' @export

regmatches2 <- function(x, m, invert = FALSE) {
    lens <- integer()
    if (length(m) && is.list(m[[1L]]) && length(m[[1L]])) {
        lens <- lengths(m)
        x <- rep(x, lens)
        m <- unlist(m, recursive = FALSE)
    }
    res <- regmatches(x = x, m = m, invert = invert)
        if (length(lens)) {
          unname(split(res, rep(seq_along(lens), lens)))
    } else res
}

# regexec:
# * returns list with one vector per input element
# * if input is ascii each element has attr "index.type" == 'x' which missing
#   for UTF-8, "chars" normally, or "bytes" if "useBytes=TRUE" (even for UTF-8
#   in the latter case).
# * The types seem to be global, i.e. mixed encoding vectors with ASCII and
#   UTF-8 behave like UTF-8.
#
# regexpr:
# * As above, except there is only on item
#
# gregexpr


