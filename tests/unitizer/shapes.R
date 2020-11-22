library(svgchop)

unitizer_sect("basic shapes", {
  chop(R_logo())
  chop(svg_samples('^arcs\\.svg$'))
  chop(svg_samples('^clip-heart\\.svg$'))
  chop(svg_samples('^fill-rule\\.svg$'))
  chop(svg_samples('^gradient\\.svg$'))
  chop(svg_samples('^logogram\\.svg$'))
  chop(svg_samples('^pie-and-arcs\\.svg$'))
  chop(svg_samples('^shapes\\.svg$'))
})
