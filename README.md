<!-- README.md is generated from README.Rmd. Please edit that file

library(rmarkdown)
render('README.Rmd', output_format=md_document())
# render('README.Rmd', output_format=html_vignette())
-->
svgchop - Approximate SVGs With Line Segments
=============================================

<!-- badges: start -->
[![R build
status](https://github.com/brodieG/svgchop/workflows/R-CMD-check/badge.svg)](https://github.com/brodieG/svgchop/actions)
[![](https://codecov.io/github/brodieG/svgchop/coverage.svg?branch=master)](https://codecov.io/github/brodieG/svgchop?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

Overview
--------

`svgchop` is a “close-enough” SVG interpreter for R. It parses and
converts SVG geometry into line segments, and other presentation
attributes into easily useable form. It implements a significant but
incomplete portion of the SVG 1.1 spec in an approximate manner. While
you should not expect the results to be identical to those produced by a
conforming implementation, many SVGs are approximated closely.

This package is aimed at the narrow niche of those who want to extract
SVG elements as polygons before they are drawn to canvas. It is
experimental, and follows the old-city design model (start with the
village well, add features as needed, next thing you know you have a
mess and you do a half-hearted
[*Haussmannization*](https://en.wikipedia.org/wiki/Haussmann%27s_renovation_of_Paris)).
The API is likely to change with minimal concessions to backward
compatibility. The code has not been optimized. It will not make it onto
CRAN except in the unlikely event there is substantial external interest
for that to happen.

Why?
----

I wanted to render SVGs in 3D using
[`rayrender`](https://cran.r-project.org/web/packages/rayrender/index.html)
and I needed the SVGs as polygons for that:

<img src=extra/r-3d.png alt="R Logo in 3D!" style='width: 650px;'/>

That’s it. If you find any good use-cases for this package do let me
know.

How?
----

Just point `svgchop::chop` to an SVG file and it will interpret it:

    library(svgchop)
    svg <- chop(R_logo(), steps=3)

You can then use the data to plot the polygons however you wish. Here we
do so with base plotting functionality:

    ext <- attr(svg, "extents")
    plot.new()
    plot.window(ext$x, rev(ext$y), asp=1)

    xy <- get_xy_coords(svg)
    fills <- get_fills(svg)
    polypath(xy[[1]], col=fills[[1]], border=NA)
    polypath(xy[[2]], col=fills[[2]], border=NA)

![](extra/figures/README-r-logo-1.png)

Installation
------------

`svgchop` is Github-only:

    f.dl <- tempfile()
    f.uz <- tempfile()
    github.url <- 'https://github.com/brodieG/svgchop/archive/master.zip'
    download.file(github.url, f.dl)
    unzip(f.dl, exdir=f.uz)
    install.packages(file.path(f.uz, 'svgchop-master'), repos=NULL, type='source')
    unlink(c(f.dl, f.uz))

Or if you have `remotes`:

    remotes::install_github("brodieG/svgchop")

Feature Coverage
----------------

This package implements:

-   All basic elements.
-   Paths, including all sub-commands.
-   Transforms.
-   Clipping.
-   CSS and in-line styling.
-   Gradients[1].

The implementation does not conform to the standard, but it works
reasonably well across a wide variety of SVGs:

<a href=extra/gallery.png style='text-decoration: none; color: inherit;'>
<img
  src=extra/gallery.png style='width: 650px;'
  alt="diptychs comparing SVGs to their svgchop counterparts"
/> </a>

Each diptych has a browser-rendered image on the left, and a
purposefully coarse `svgchop`ped render on the right.

`svgchop` implements a `plot` method which we used for the diptychs
above. There are a few issues in matching the browser output: gradients
and filters are not rendered correclty, and the colors aren’t quite
right (something in the bowels of my system is causing `png` to apply a
color profile that I can’t be bothered to track down). Notwithstanding,
the point of this package is not to compete with browsers in rasterizing
SVGs. The `plot` method is provided only to help verify the SVG
interpretation is working correctly.

Documentation
-------------

See `?svgchop::chop`, in particular the examples.

References
----------

-   [Rsvg](https://cran.r-project.org/package=rsvg), a wrapper around
    the [Librsvg Gnome library](https://developer.gnome.org/rsvg/), to
    rasterize SVGs.
-   [string2path](https://github.com/yutannihilation/string2path) for
    converting fonts to polygons.
-   [svgpathtools](https://github.com/mathandy/svgpathtools) or a Python
    SVG manipulation library.
-   [objjob svg](http://objjob.phrogz.net/svg/hierarchy) for
    [`getPointAtLength`](http://phrogz.net/svg/convert_path_to_polygon.xhtml).
-   [SVG Path Spec](https://www.w3.org/TR/SVG/paths.html), in particular
    the [Arc Implementation
    Notes](https://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes).
-   [normalize-svg-path](https://github.com/jkroso/normalize-svg-path)
    npm module for converting an SVG path to a pure cubic Bézier path.
-   [svg-mesh-3d](https://github.com/mattdesl/svg-mesh-3d) npm module
    for converting paths to meshes.

Acknowledgments
---------------

-   R Core for developing and maintaining such a wonderful language.
-   [W3C](https://www.w3.org/) for the SVG spec and accompanying
    documentation, and more generally for supporting open web standards.
-   Daniel Veillard etal. for [libxml2](http://www.xmlsoft.org/), and
    Jim Hester, Hadley Wickham, etal. for making it available in R with
    a nice consistent interface via
    [`xml2`](https://cran.r-project.org/package=xml2), which we use to
    easily manipulate SVG files (among other things).
-   Angus Johnson for [clipper](http://angusj.com/delphi/clipper.php),
    and Adrian Baddley etal. for making it available in R via
    [polyclip](https://cran.r-project.org/package=polyclip). We use
    `polyclip` to implement clip paths.
-   [Tyler Morgan Wall](https://github.com/tylermorganwall/) for
    [`rayrender`](https://cran.r-project.org/web/packages/rayrender/index.html)
    which I used for the 3D path-traced renderings.
-   Simon Urbanek for [`png`](https://cran.r-project.org/package=png).
-   Dom Lachowicz, Christian Persch, and Federico Mena Quintero for
    [Librsvg](https://developer.gnome.org/rsvg/), and [Jeroen
    Ooms](https://github.com/jeroen) for making it available in R via
    [rsvg](https://cran.r-project.org/package=rsvg).
-   [Jim Hester](https://github.com/jimhester) because
    [covr](https://cran.r-project.org/package=covr) rocks.
-   [MDN](https://developer.mozilla.org/en-US/) for being a fantastic
    resource for web development.
-   [Mortoray](https://twitter.com/edaqa) for pointing me to the SVG arc
    implementation appendix via his [blogpost on the
    topic](https://mortoray.com/2017/02/16/rendering-an-svg-elliptical-arc-as-bezier-curves/).
-   [Hadley Wickham](https://github.com/hadley/), [Peter
    Danenberg](https://github.com/klutometis), etal. for
    [roxygen2](https://cran.r-project.org/package=roxygen2).
-   [Yihui Xie](https://github.com/yihui) for
    [knitr](https://cran.r-project.org/package=knitr) and [J.J.
    Allaire](https://github.com/jjallaire) etal for
    [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by
    extension John MacFarlane for [pandoc](http://pandoc.org/).
-   [Github](https://github.com/), [Codecov](https://codecov.io/),
    [Vagrant](https://www.vagrantup.com/),
    [Docker](https://www.docker.com/),
    [Ubuntu](https://www.ubuntu.com/), [Brew](https://brew.sh/) for
    providing infrastructure that greatly simplifies open source
    development.
-   [Free Software Foundation](http://fsf.org/) for developing the GPL
    license and promotion of the free software movement.

[1] Gradients are fully parsed and the data required to render them is
made available to the user. The built-in plot method reduces them to a
single color, but the user is free to use better display devices (e.g.
`grid` &gt; 4.0) that natively support gradients to render them.
