<!-- README.md is generated from README.Rmd. Please edit that file

library(rmarkdown)
render('README.Rmd', output_format=md_document())
# render('README.Rmd', output_format=html_vignette())
-->
svgchop - Approximate SVG Elements With Line Segments
=====================================================

Overview
--------

Parse and convert SVG elements into line segments and supporting meta
data. A significant but incomplete portion of the SVG 1.1 spec is
implemented in an approximate manner. While you should not expect the
results to be identical to those produced by a conforming
implementation, many SVGs are approximated closely.

This package is aimed at anyone who wants to extract SVG elements as
polygons before they are drawn to the canvas.

Feature Coverage
----------------

We implement:

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
> </a>

Each diptych has a browser-rendered image on the left, and an `svgchop`
rendering on the right. We purposefully use low-resolution
approximations here for effect, but it is reasonably cheap to increase
the resolution sufficiently that the `svgchop` images become difficult
to distinguish from the real ones.

`svgchop` implements a `plot` method which we used for the diptychs
above, but its primary purpose is to verify `svgchop` is working
correctly. It also provides a blueprint for others looking to access the
`svgchop` data for their own purposes and display devices. The plot
method does not implement gradients, which is why some of the more
complex SVGs don’t look right. It would be reasonably straightforward to
use the [new `grid` &gt;
4.0.x](https://developer.r-project.org/Blog/public/2020/07/15/new-features-in-the-r-graphics-engine/index.html)
features to display the gradients correctly, but the point of this
package is to extract the SVG data, not to compete with the browsers in
rasterizing them.

Related Software
----------------

-   [Rsvg](https://cran.r-project.org/package=rsvg) to rasterize SVGs.
-   [string2path](https://github.com/yutannihilation/string2path) for
    converting fonts to polygons.
-   [svgpathtools](https://github.com/mathandy/svgpathtools) or a Python
    SVG manipulation library.
-   [objjob svg](http://objjob.phrogz.net/svg/hierarchy) for
    [`getPointAtLength`](http://phrogz.net/svg/convert_path_to_polygon.xhtml).
-   [SVG Path Spec](https://www.w3.org/TR/SVG/paths.html), in particular
    the [Arc Implementation
    Notes](https://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes).
-   [Mortoray’s
    post](https://mortoray.com/2017/02/16/rendering-an-svg-elliptical-arc-as-bezier-curves/)
    on converting arcs to Bézier curves.
-   [normalize-svg-path](https://github.com/jkroso/normalize-svg-path)
    npm module for converting an SVG path to a pure cubic Bézier path.
-   [svg-mesh-3d](https://github.com/mattdesl/svg-mesh-3d) npm module
    for converting paths to meshes.

Acknowledgments
---------------

-   R Core for developing and maintaining such a wonderful language.
-   [W3C](https://www.w3.org/) for the SVG spec and accompanying
    documentation, and more generally for supporting open web standards.
-   [MDN](https://developer.mozilla.org/en-US/) for being a fantastic
    resource for web development.
-   Jim Hester, Hadley Wickham, etal. for
    [`xml2`](https://cran.r-project.org/package=xml2), which makes it
    easy to extract properties from SVG (among other things).
-   [Mortoray](https://twitter.com/edaqa) for pointing me to the SVG arc
    implementation appendix.
-   [Hadley Wickham](https://github.com/hadley/), [Peter
    Danenberg](https://github.com/klutometis), etal. for
    [roxygen2](https://cran.r-project.org/package=roxygen2).
-   [Yihui Xie](https://github.com/yihui) for
    [knitr](https://cran.r-project.org/package=knitr) and [J.J.
    Allaire](https://github.com/jjallaire) etal for
    [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by
    extension John MacFarlane for [pandoc](http://pandoc.org/).
-   [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
    [Codecov](https://codecov.io/),
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
