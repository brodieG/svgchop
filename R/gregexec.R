#' Global Regexec
#'
#' @export
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
       y <- c(t(cbind(x, attr(x, "capture.start"))))
       attributes(y) <- attributes(x)
       attr(y, "match.length") <- c(
         t(cbind(attr(x, "match.length"), attr(x, "capture.length")))
       )
       y
     }
   )
}

