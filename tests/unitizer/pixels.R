library(svgchop)

unitizer_sect("pixel match", 
  compare=function(ref, new) {
    if(length(ref) != length(new)) {
      sprintf("length mismatch (ref: %d new: %d)", length(ref), length(new))
    } else if (!identical(names(ref), names(new))) {
      "name mismatch"
    } else if (anyNA(new)) {
      "NA values in new"
    } else {
      thresh <- new - ref < -.01
      if(any(thresh))
        paste0(
          c("elements ", deparse(which(thresh)) ," exceed error threshold."),
          collapse=""
        )
      else TRUE
    }
  },
  {
    error <- local({
      page <- compare_rsvg(display=0, steps=50, quietly=TRUE)
      diffs <-
        list.files(dirname(page), full.names=TRUE, pattern="^diff.*\\.png$")
      res <- setNames(
        vapply(diffs, function(x) mean(png::readPNG(x)), 0),
        sub(".*[0-9]{2}-(.*)\\.png$", "\\1", basename(diffs))
      )
      unlink(dirname(page), recursive=TRUE)
      res
    })
    error
  }
)
unitizer_sect("basic plotting", {
  tmp <- tempfile()

  svg <- chop(system.file(package='svgchop', 'svg', 'test', 'simple-03.svg'))
  png(tmp, width=3, height=3, antialias='none')
  plot(svg)
  invisible(dev.off())
  refdat <- png::readPNG(tmp)
  # Simple plot
  refdat

  png(tmp, width=3, height=3, antialias='none')
  plot(as.svg_chopped_list(svg))
  invisible(dev.off())
  plist <- png::readPNG(tmp)
  # List plot
  identical(refdat, plist)

  png(tmp, width=3, height=3, antialias='none')
  plot(flatten(svg))
  invisible(dev.off())
  pflat <- png::readPNG(tmp)
  # Flat plot
  identical(refdat, pflat)

  png(tmp, width=3, height=3, antialias='none')
  plot(flatten(as.svg_chopped_list(svg)))
  invisible(dev.off())
  pflist <- png::readPNG(tmp)
  # Flat List plot
  identical(refdat, plist)
})
unitizer_sect("dimensions", {
  tmp <- tempfile()

  svg <- chop(system.file(package='svgchop', 'svg', 'test', 'simple-03.svg'))
  png(tmp, width=5, height=3, antialias='none')
  plot(svg)
  invisible(dev.off())
  wide <- png::readPNG(tmp)
  wide

  png(tmp, width=3, height=5, antialias='none')
  plot(svg)
  invisible(dev.off())
  tall <- png::readPNG(tmp)
  tall
})
