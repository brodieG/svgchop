library(svgchop)

unitizer_sect("basic shapes", {
  chop(R_logo())
  chop(svg_samples('01-arcs\\.svg$'))
  chop(svg_samples('clip-heart\\.svg$'))
  chop(svg_samples('fill-rule\\.svg$'))
  chop(svg_samples('04-gradient\\.svg$'))
  chop(svg_samples('logogram\\.svg$'))
  chop(svg_samples('pie-and-arcs\\.svg$'))
  chop(svg_samples('shapes\\.svg$'))
  chop(svg_samples('radial-gradient\\.svg$'))
})
