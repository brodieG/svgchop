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
#' @inheritParams base::grep

gregexec <- function(
  pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE) {
   stopifnot(perl)

   dat <- gregexpr(
     pattern=pattern, text=text, ignore.case = ignore.case, fixed = fixed,
     useBytes = useBytes, perl=TRUE
   )
   lapply(dat,
     function(x) {
       if(anyNA(x) || any(x < 0)) y <- x
       else {
         # we want to interleave matches with captures
         y <- c(t(cbind(x, attr(x, "capture.start"))))
         attributes(y) <- attributes(x)
         attr(y, "match.length") <- c(
           t(cbind(attr(x, "match.length"), attr(x, "capture.length")))
         )
       }
       y
     }
   )
}

