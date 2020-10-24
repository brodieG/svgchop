library(unitizer)
unitizer_sect('Color conversion to hex', {
  cols <- c(
    'none', 'transparent', '#1Bc', '#FF0011',
    'blahblah', 'pink',
    '  rgb(255 , 0, 3)', 'rgb(255 , 300, 3)',
    'rgb(255%, 300, 30%)', 'rgb(5%,5%,110%)'
  )
  cbind(old=cols, new=svgchop:::proc_color(cols))
})
