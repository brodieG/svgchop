<!-- README.md is generated from README.Rmd. Please edit that file

library(rmarkdown)
render('README.Rmd', output_format=md_document())

-->
svgchop - Approximate SVG Elements With Line Segments
=====================================================

Overview
--------

Parse SVG elements and approximate them with straight line segments.
Only a small set of SVG syntax and commands are recognized. The parsing
is done with regex so don’t expect it to be robust. The code is slow and
hacky.

**This package is experimental, lightly tested, poorly documented, and
not intended for production use**.

    # retrieve and parse, and interpolate
    library(svgchop)
    paths.raw <- parse_paths('https://www.r-project.org/logo/Rlogo.svg')
    paths <- interp_paths(paths.raw, steps=3, normalize=TRUE)

    # Paths are known to be closed in this case, so abuse that to create sub-paths
    subpaths <- lapply(
      paths, function(x) {
        x$d[attr(x$d, 'starts'),] <- NA
        x$d$y <- 1 - x$d$y  # need to flip y values
        x$d
    } )
    # Plot!
    plot.new()
    invisible(
      Map(
        polypath, subpaths, col=c("#A7A8AD", "#1E64B6"),
        border=NA, rule='evenodd'
    ) )

![](extra/figures/README-r-logo-1.png)

Related
-------

-   [svgpathtools](https://github.com/mathandy/svgpathtools).
-   [objjob svg](http://objjob.phrogz.net/svg/hierarchy) for
    [`getPointAtLength`](http://phrogz.net/svg/convert_path_to_polygon.xhtml).
-   [SVG Path Spec](https://www.w3.org/TR/SVG/paths.html).
-   [svg.path](https://github.com/regebro/svg.path).

Acknowledgments
---------------

-   R Core for developing and maintaining such a wonderful language.
-   Paul Murrell for
    [`gridBezier`](https://cran.r-project.org/package=gridBezier) with
    which we convert SVG curves into piecewise linear paths.
-   Jim Hester, Hadley Wickham, etal. for
    [`xml2`](https://cran.r-project.org/package=xml2), which makes it
    easy to extract properties from SVG (among other things).
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
