library(svgchop)
unitizer_sect('Color conversion to hex', {
  cols <- c(
    'none', 'transparent', '#1Bc', '#FF0011',
    'blahblah', 'pink',
    '  rgb(255 , 0, 3)', 'rgb(255 , 300, 3)',
    'rgb(255%, 300, 30%)', 'rgb(5%,5%,110%)'
  )
  cbind(old=cols, new=svgchop:::proc_color(cols))
})

unitizer_sect('Bad Styles', {
  dir <- system.file(package='svgchop', 'svg', 'test')
  chop(file.path(dir, 'bad-style-01.svg'), warn=TRUE)
  chop(file.path(dir, 'bad-style-02.svg'), warn=TRUE)
  chop(file.path(dir, 'bad-style-03.svg'), warn=TRUE)
})

unitizer_sect('Misc', {
  styles_computed()
})

