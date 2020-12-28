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
                     fixed = FALSE, useBytes = FALSE) {
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
        }
        attributes(y)[capt.attr] <- NULL
        y
    }
    lapply(dat, process)
}
#' Original gregexec, returns matches with their captures as matrices
#'
#' @export

gregexec_M <- gregexec

#' Based on `?regexec` example, returns matches and captures as nested lists.
#'
#' @export

gregexec_L <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = FALSE) {
    dat <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                    fixed = fixed, useBytes = useBytes, perl = perl)
    g <- lapply(regmatches(text, dat),
                regexec, pattern = pattern, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes)
    Map(function(global, local) Map(`+`, local, global - 1L), dat, g)
}
#' Matrix output, but computed from as `gregexec_L` with the result
#' post-processed into matrix.  This allows it to work with perl=FALSE.
#'
#' @noRd
#' @export

gregexec_M2 <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) {
    m0 <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                   fixed = fixed, useBytes = useBytes, perl = perl)
    m1 <- lapply(regmatches(text, m0),
                 regexec, pattern = pattern, ignore.case = ignore.case,
                 perl = perl, fixed = fixed, useBytes = useBytes)
    mlen <- lengths(m1)
    res <- vector("list", length(m1))
    im <- mlen > 0
    res[!im] <- m0[!im]   # -1, NA
    res[im] <- Map(
        function(outer, inner) {
            tmp <- do.call(cbind, inner)
            attributes(tmp)[names(attributes(inner))] <- attributes(inner)
            attr(tmp, 'match.length') <-
                do.call(cbind, lapply(inner, `attr`, 'match.length'))
            # useBytes/index.type should be same for all so use outer vals
            attr(tmp, 'useBytes') <- attr(outer, 'useBytes')
            attr(tmp, 'index.type') <- attr(outer, 'index.type')
            tmp + rep(outer - 1L, each = nrow(tmp))
        },
        m0[im],
        m1[im]
    )
    res
}

#' Experimental, trying to see if it is worth trying to generate the list
#' version from the matrix version for perl = TRUE
#'
#' @export

gregexec_L2 <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE, method = 'matrix') {
    stopifnot(perl)
    dat <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                    fixed = fixed, useBytes = useBytes, perl = TRUE)
    drop.attr <- c('capture.start', 'capture.length', 'capture.names',
                   'match.lenght')
    copy.attr <- c('index.type', 'useBytes')
    # `asplit` is slow relative to this naive implementation for special case
    split_rows <- function(m) lapply(seq_len(nrow(m)), function(i) m[i,])
    process <- function(x) {
        if(anyNA(x) || any(x < 0)) y <- x
        else {
            # we want to interleave matches with captures
            y <- split_rows(
                cbind(x, attr(x, "capture.start"), deparse.level=0L))
            m.l <- split_rows(
                cbind(attr(x, "match.length"), attr(x, "capture.length"),
                      deparse.level=0L))
            y <- Map(
                function(a, b) {
                    attributes(a)[copy.attr] <- attributes(x)[copy.attr]
                    attr(a, 'match.length') <- b
                    a
                },
                y, m.l
            )
            attributes(y)[drop.attr] <- NULL
        }
        y
    }
    lapply(dat, process)
}

#' Like `regmatches` but copies input dimension to output so matrices are
#' preserved.
#'
#' @export

regmatches_M <- function(x, m, invert = FALSE) {
    res <- regmatches(x = x, m = m, invert = invert)
    Map(function(x, y) { dim(x) <- dim(y); x }, res, m)
}
#' Like `regmatches` but can handle nested list input.
#'
#' @export

regmatches_L <- function(x, m, invert = FALSE) {
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


