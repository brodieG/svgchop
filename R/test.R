

#' Generate test HTML From SVGs
#'
#' [svg_gallery()] generates a single HTML file with all the sample SVGs
#' embedded in it.  [svg_gallery_compare()] processes them with [process_svg()],
#' saves the resulting plotted images as rasters (png), and generates a web page
#' juxtaposing the original SVG and the re-rendered version.
#'
#' @export
#' @param source character vector of paths or URLs for SVGs to compile into
#'   single HTML file.
#' @param target a file to write the html to.
#' @param ... additional arguments passed on to [process_svg()]
#' @return character(1L) the name of the file written to

svg_gallery <- function(
  source=svg_samples(),
  target=paste0(tempfile(), ".html")
) {
  writeLines("<!DOCTYPE html><html><body>", target)
  lapply(source, file.append, file1=target)
  cat( "</body></html>\n", file=target, append=TRUE)
  target
}
#' @export
#' @rdname svg_gallery

svg_gallery_compare <- function(
  source=svg_samples(),
  target=tempfile(),
  ppi=96,
  ...
) {
  dir.create(target)
  out <- file.path(target, "index.html")
  writeLines("<!DOCTYPE html><html><body>", out)
  imgs <- character(length(source))
  for(i in seq_along(source)) {
    file.append(out, source[i])
    f <- file.path(target, sprintf("img-%03d.png",i))
    imgs[i] <- f
    png(f, width=500, height=500, res=ppi)
    plot(process_svg(source[i], ...), ppi=ppi)
    dev.off()
    cat(sprintf("<img src='%s' />", f), file=out, append=TRUE)
  }
  cat( "</body></html>\n", file=out, append=TRUE)
  target
}
#' @rdname svg_gallery
#' @export

svg_samples <- function()
  list.files(
    system.file(package='svgchop', 'svg'), pattern="\\.svg$", ignore.case=TRUE,
    full.names=TRUE
  )

