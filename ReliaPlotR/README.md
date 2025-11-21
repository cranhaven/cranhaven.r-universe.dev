
# ReliaPlotR

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/ReliaPlotR)](https://CRAN.R-project.org/package=ReliaPlotR)
[![R-CMD-check](https://github.com/paulgovan/ReliaPlotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paulgovan/ReliaPlotR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/paulgovan/ReliaPlotR/graph/badge.svg)](https://app.codecov.io/gh/paulgovan/ReliaPlotR)
[![](http://cranlogs.r-pkg.org/badges/last-month/ReliaPlotR)](https://cran.r-project.org/package=ReliaPlotR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ReliaPlotR)](https://cran.r-project.org/package=ReliaPlotR)

<!-- badges: end -->

Build interactive Reliability Probability Plots with `plotly`, an
interactive web-based graphing library.

## Getting Started

To install `ReliaPlotR` in R:

``` r
install.packages("ReliaPlotR")
```

Or install the development version:

``` r
devtools::install_github("paulgovan/ReliaPlotR")
```

## Basic Examples

To build a probability plot, first fit a `wblr` object using the
`WeibullR` package and then use `plotly_wblr` to build the plot.

``` r
library(WeibullR)
library(ReliaPlotR)
failures <- c(30, 49, 82, 90, 96)
obj <- wblr.conf(wblr.fit(wblr(failures)))
plotly_wblr(obj)
```

![](ReadMe_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

To build a contour plot, use the `plotly_contour` function. Note that
contour plots are only available where `method.fit='mle'` and
`method.conf='lrb'`.

``` r
obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = "mle"), method.conf = "lrb")
plotly_contour(obj)
```

![](ReadMe_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Customization

`ReliaPlotR` has several customization options.

``` r
plotly_wblr(obj, main = "Weibull Probability Plot", xlab = "Years", ylab = "Failure Probability", confCol = "blue", signif = 4, grid = FALSE)
```

![](ReadMe_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plotly_contour(obj, main = "Weibull Contour Plot", col = "red", signif = 4, grid = FALSE)
```

![](ReadMe_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Code of Conduct

Please note that the ReliaPlotR project is released with a [Contributor
Code of
Conduct](https://github.com/paulgovan/ReliaPlotR/blob/f919aeb72a1d4dd3a64e55221eb1ae214b3480f5/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
