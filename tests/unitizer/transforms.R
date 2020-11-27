library(svgchop)

unitizer_sect("bad transforms", {
  chop(
    system.file(
      package='svgchop',
      file.path('svg', 'test', 'bad-transform.svg')
    ),
    warn=TRUE
  )
})
