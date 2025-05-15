# CruzPlot

<!-- badges: start -->

[![CRAN version](http://www.r-pkg.org/badges/version/CruzPlot)](https://cran.r-project.org/package=CruzPlot)
[![R-CMD-check](https://github.com/SWFSC/CruzPlot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SWFSC/CruzPlot/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

CruzPlot is an R package that contains a GUI for creating maps, plotting data, and performing basic data summaries from data files in the “DAS” format, typically produced by the SWFSC software WinCruz. However, see the [swfscDAS](https://github.com/SWFSC/swfscDAS/) for more discussion around DAS data.

## Installation

You can install the released version of CruzPlot from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("CruzPlot")
```

You can install the developmental version of CruzPlot from [GitHub](https://github.com/). To use the development version of CruzPlot, it is recommended to install the development version of [swfscDAS](https://swfsc.github.io/swfscDAS/index.html) as well:

``` r
# install.packages("remotes") # Install remotes package if needed
remotes::install_github("swfsc/swfscDAS", build_vignettes = TRUE)
remotes::install_github("swfsc/CruzPlot")
```

After updating CruzPlot, it is recommended to create and save new workspaces, rather than using workspaces saved with a previous version of CruzPlot. Please contact the developer if you have any issues.

## Running CruzPlot

If you have a stable internet connection, you can access CruzPlot online at <https://connect.fisheries.noaa.gov/CruzPlot>. You do not need to have R or RStudio installed to access CruzPlot online.

To open and run CruzPlot locally on your computer, run the following code in RStudio:

``` r
CruzPlot::cruzplot_gui()
```

If text or images overlap within the CruzPlot window, you can make the browser window full screen or adjust the text size in your browser (e.g., Ctrl - minus (‘-’) on Windows systems)

## CruzPlot manual

The CruzPlot manual can be downloaded [at this link](https://github.com/swfsc/CruzPlot/blob/master/inst/shiny/www/CruzPlot_Manual_app.pdf), or accessed when running CruzPlot by opening the “CruzPlot Manual” page. It is recommended for new users to read the first two pages of the manual for orientation purposes; the rest of the manual contains section-specific information (formatting requirements, etc.) and can be read on an as-needed basis.

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
