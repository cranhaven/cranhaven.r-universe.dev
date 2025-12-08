[![tic](https://github.com/tgve/tgver/actions/workflows/tic.yml/badge.svg)](https://github.com/tgve/tgver/actions/workflows/tic.yml)
[![codecov](https://codecov.io/gh/tgve/tgver/branch/master/graph/badge.svg?token=WAR82Q7597)](https://app.codecov.io/gh/tgve/tgver)
[![CRAN status](https://www.r-pkg.org/badges/version/tgver)](https://CRAN.R-project.org/package=tgver)

This is the R package for the TGVE front end `npm` package
[`tgve`](https://www.npmjs.com/package/@tgve/tgvejs). The R package is
developed to facilitate interactive geospatial analysis and
visualization in R, use R’s echo-system to drive advanced data
processing, and facilitate deployment of geospatial web applications in
production.

## Install

Install `tgver` from CRAN

```r
install.packages("tgver")
```

You can use the latest from GitHub using `devtools::install_github("tgve/tgver")`

## Use

Overall, the package takes advantage of how
TGVE can be used (see
[npm package](https://github.com/tgve/tgvejs) for documentation) and
provides options to R users.

For instance, this document is a Markdown (GitHub) document generated
using an Rmarkdown (Rmd) document, if the Rmd is rendered to a HTML
output, then using `knitr::include_url` we should see an instance of the
TGVE embedded in the document. Please see the live examples in the
[vignette](https://tgve.github.io/tgver/articles/tgver.html) which is
rendered to HTML.

To do this we can go:

``` r
# this is the most basic use
# tgver::tgve()
# to embed in a html rendered Rmd
html.file = tgver::tgve(browse = FALSE)
knitr::include_url(html.file)
```

The first function `tgver::tgve(browse=FALSE)` prepares an instance of
the TGVE but does not run/open it, it returns its path (a `tempdir()`
path). The second line is `knitr` function to embed the first line’s
output.

That was the simplest way of running an instance of TGVE on the local
machine. The more advanced but similar function of this package, with
the back-end as a `plumber` API and serving the same instance, would be
like:

``` r
# start a tgve instance before embedding it
ps = tgver::tgve_server(background = TRUE)
#> Attempting to serve TGVE instance from: /tmp/Rtmpevq9TT/tgve
#> Running plumber at: http://127.0.0.1/8000
knitr::include_url("http://127.0.0.1:8000")
# kill the process returned from underlying `callr`
ps$kill()
# or use the public one
# knitr::include_url("https://tgve.github.io/app/")
```

## Explore `sf` objects

For these purposes, the package relies on the `plumber` API to serve
data. So, again within HTML outputs we can explore `sf` objects like so:

``` r
# using sf
nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
tgver::explore_sf(nc)
```

For more see the
[vignette](https://tgve.github.io/tgver/articles/tgver.html).

## Preview

![tgve-vignette](https://user-images.githubusercontent.com/408568/141796882-2cf68f6b-a6e4-4836-9efa-bf1973f5cab9.png)

