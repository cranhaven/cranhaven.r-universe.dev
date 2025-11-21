---
title: "WeibullR.plotly: Interactive Weibull Probability Plots"
tags:
  - R
  - life data analysis
  - Weibull analysis
  - reliability
  - plotly
authors:
  - name: Paul B. Govan
    orcid: 0000-0002-1821-8492
    affiliation: 1
affiliations:
  - name: Senior Reliability Systems Engineer, GE Aerospace
    index: 1
date: 11 July 2024
bibliography: paper.bib
---

## Statement of Need

Life data analysis examines the behavior of systems over time. Often termed Weibull data analysis, due to the common use of the Weibull distribution, this field involves iterative data wrangling, modeling, and visualization. Interactive Weibull modeling offers numerous advantages, such as the ability to explore subsamples and uncover hidden data structures. `WeibullR.plotly` [@WeibullRplotly] is an open-source software package designed for creating interactive Weibull Probability Plots. It aims to provide more flexibility for exploratory Weibull analysis than traditional static plotting. The primary users of this project are analysts and engineers involved in life data, survival, and reliability analysis.

## Description

`WeibullR.plotly` is developed in R [@R], leveraging `WeibullR` [@WeibullR], a package dedicated to Life Data Analysis, and `plotly` [@plotly], an interactive web-based graphing library. This package serves as an add-on to `WeibullR`, enabling web-based graphics. By combining the strengths of both tools,, `WeibullR.plotly` is ideal for embedding in web-based applications, such as `learnr` [@learnr] interactive tutorials or `shiny` [@shiny] web applications. Examples include `WeibullR.learnr` [@WeibullRlearnr], a learnr module for Life Data Analysis, and `WeibullR.shiny` [@WeibullRshiny], a shiny app designed for Weibull Analysis.

### Usage

`WeibullR.plotly` is available on the Comprehensive R Archive Network (CRAN) as an R package at [https://CRAN.R-project.org/package=WeibullR.plotly](https://CRAN.R-project.org/package=WeibullR.plotly).

To install the release verion in R, use:

```r
install.packages(“WeibullR.plotly”)
```

To install the latest development version:

```r
devtools::install_github(‘paulgovan/WeibullR.plotly’)
```

#### Weibull Probability Plots 

To build a Weibull Probability Plot, fit a `wblr` object to a life data set using the `WeibullR` package, then generate plots with the `plotly_wblr` function.

``` r
library(WeibullR)
library(WeibullR.plotly)
failures<-c(30, 49, 82, 90, 96)
obj<-wblr.conf(wblr.fit(wblr(failures)))
plotly_wblr(obj, main='Weibull Probability Plot', xlab='Years', ylab='Failure Probability', confCol='blue', signif=4, grid=FALSE)
```

![](https://github.com/paulgovan/WeibullR.plotly/blob/main/ReadMe_files/figure-gfm/unnamed-chunk-5-1.png?raw=true)<!-- -->

#### Contour Plots

Create contour plots using the `plotly_contour` function with a `wblr` object.

``` r
plotly_contour(obj, main='Weibull Contour Plot', col='red', signif=4, grid=FALSE)
```

![](https://github.com/paulgovan/WeibullR.plotly/blob/main/ReadMe_files/figure-gfm/unnamed-chunk-6-1.png?raw=true)<!-- -->

#### Customization Options

Customize labels, colors, and grids to tailor the plots to specific needs. Refer to the package documentation for a comprehensive list of options.

### Documentation and Resources

Full documentation and working examples are available at: [https://paulgovan.github.io/WeibullR.plotly/](https://paulgovan.github.io/WeibullR.plotly/). The project documentation includes guidance on installing `WeibullR.plotly` and its dependencies, demonstrations of how to use the package functions. Unit tests for ensuring code reliability and performance, and further learning resources for getting started with the package.

### Contribution and Community Engagement

Engineers and analysts are encouraged to use and contribute to the project. The repository includes a Contributor Code of Conduct. Issues and feature requests can be submitted through Issues or Pull Requests here: [https://github.com/paulgovan/WeibullR.plotly/issues](https://github.com/paulgovan/WeibullR.plotly/issues).

## References
