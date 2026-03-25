
## `PakPC`: Shiny App to Analyze Pakistan’s Population Census Data

###### Version : [0.3.0](https://myaseen208.com/PakPC/); Copyright (C) 2024: License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### *Muhammad Yaseen<sup>1,2</sup>, Muhammad Arfan Dilber<sup>3</sup>, and Zahid Asghar<sup>4</sup>*

1.  [School of Mathematical & Statistical Sciences, Clemson University,
    Clemson, South Carolina,
    USA](https://www.clemson.edu/science/academics/departments/mathstat/about/profiles/myaseen)
2.  Department of Mathematics and Statistics, University of Agriculture
    Faisalabad, Pakistan
3.  Pakistan Bureau of Statistics, Pakistan
4.  [School of Economics, Quaid-i-Azam University, Islamabad,
    Pakistan](https://zahidasghar.com/)

------------------------------------------------------------------------

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/PakPC)](https://cran.r-project.org/package=PakPC)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PakPC?color=green)](https://CRAN.R-project.org/package=PakPC)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/myaseen208/PakPC) -->

[![develVersion](https://img.shields.io/badge/devel%20version-0.4.0-orange.svg)](https://github.com/myaseen208/PakPC)

<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/myaseen208/PakPC/total.svg)] -->

[![Project Status:
WIP](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2024--12--09-yellowgreen.svg)](https://github.com/myaseen208/PakPC)
\*\*\*

## Description

Provides tools for analyzing Pakistan’s Population Censuses data via the
‘PakPC2023’ and ‘PakPC2017’ R packages. Designed for researchers,
policymakers, and professionals, the app enables in-depth numerical and
graphical analysis, including detailed cross-tabulations and insights.
With diverse statistical models and visualization options, it supports
informed decision-making in social and economic policy. This tool
enhances users’ ability to explore and interpret census data, providing
valuable insights for effective planning and analysis across various
fields.

## Installation

The package can be installed from CRAN as follows:

``` r
install.packages("PakPC", dependencies = TRUE)
```

The development version can be installed from github as follows:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("myaseen208/PakPC")
```

## What’s new

To know whats new in this version type:

``` r
news(package = "PakPC")
```

## Links

[Shiny App](https://myaseen208.shinyapps.io/PakPC/)

[CRAN page](https://cran.r-project.org/package=PakPC)

[Github page](https://github.com/myaseen208/PakPC)

[Documentation website](https://myaseen208.com/PakPC/)

## Citing `PakPC`

To cite the methods in the package use:

``` r
citation("PakPC")
Please, support this project by citing it in your publications!

  Yaseen M, Dilber MA, Asghar Z (2024). _PakPC: Shiny App to Analyze
  Pakistan's Population Census Data_.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {PakPC: Shiny App to Analyze Pakistan's Population Census Data},
    author = {Muhammad Yaseen and Muhammad Arfan Dilber and Zahid Asghar},
    year = {2024},
    journal = {The Comprehensive R Archive Network (CRAN)},
  }
```
