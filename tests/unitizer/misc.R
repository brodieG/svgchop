library(svgchop)

unitizer_sect("list stuff",{
  dir <- system.file(package='svgchop', 'svg', 'test')
  files <- list.files(dir, full.names=TRUE, pattern='^simple-0[1-2]')
  html <- svgchop:::svg_to_html(files)
  svg <- chop_all(html)

  # produce svg_chopped_flat
  svgf <- flatten(svg)
  svgf2 <- svgf[2]   # should retain attributes
  svgf2

  svgf2[1]           # also retain attributes

  capture.output(str(svg))
  capture.output(str(svg[[1]]))
  capture.output(str(svgf))
  capture.output(str(svgf[[2]]))

  as.svg_chopped_list(svg[[1]])
  as.svg_chopped_list(svg)
  as.svg_chopped_list("hello world")
  as.svg_chopped_list(list(svg[[1]], "hello world"))

  unlink(html)
})

unitizer_sect("chop errors", {
  chop(tempfile())

  f <- tempfile()
  invisible(writeLines("<html><body>Hello World</body></html>", f))
  chop(f)

})
unitizer_sect("get", {
  file <- system.file(package='svgchop', 'svg', 'test', 'simple-01.svg')
  svg <- chop(file)
  get_xy_coords(svg)
  get_fills(svg)
  get_strokes(svg)
})

