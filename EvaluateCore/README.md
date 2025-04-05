
<!-- 
<img src="https://raw.githubusercontent.com/aravind-j/EvaluateCore/master/inst/extdata/EvaluateCore.png" width="20%" />
-->

## `EvaluateCore`: Quality Evaluation of Core Collections <img src="https://raw.githubusercontent.com/aravind-j/EvaluateCore/master/inst/extdata/EvaluateCore.png" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">

###### Version : [0.1.3](https://aravind-j.github.io/EvaluateCore/); Copyright (C) 2018-2022: [ICAR-NBPGR](http://www.nbpgr.ernet.in/); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### *Aravind, J.<sup>1</sup>, Kaur, V.<sup>2</sup>, Wankhede, D. P.<sup>3</sup> and Nanjundan, J.<sup>4</sup>*

1.  Division of Germplasm Conservation, ICAR-National Bureau of Plant
    Genetic Resources, New Delhi.
2.  Division of Germplasm Evaluation, ICAR-National Bureau of Plant
    Genetic Resources, New Delhi.
3.  Division of Genomic Resources, ICAR-National Bureau of Plant Genetic
    Resources, New Delhi.
4.  ICAR-Indian Agricultural Research Institute, Regional Station,
    Wellington, Tamil Nadu.

------------------------------------------------------------------------

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/EvaluateCore)](https://cran.r-project.org/package=EvaluateCore)
[![Dependencies](https://tinyverse.netlify.com/badge/EvaluateCore)](https://cran.r-project.org/package=EvaluateCore)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/EvaluateCore?color=green)](https://CRAN.R-project.org/package=EvaluateCore)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.3-orange.svg)](https://github.com/aravind-j/EvaluateCore)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/aravind-j/EvaluateCore.svg)](https://github.com/aravind-j/EvaluateCore)
[![R-CMD-check](https://github.com/aravind-j/EvaluateCore/workflows/R-CMD-check/badge.svg)](https://github.com/aravind-j/EvaluateCore/actions)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2022--06--30-yellowgreen.svg)](https://github.com/aravind-j/EvaluateCore/)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3875930.svg)](https://doi.org/10.5281/zenodo.3875930)
[![Website -
pkgdown](https://img.shields.io/website-up-down-green-red/https/aravind-j.github.io/EvaluateCore.svg)](https://aravind-j.github.io/EvaluateCore/)
[![.](https://pro-pulsar-193905.appspot.com/UA-148941781-1/welcome-page)](https://github.com/aravind-j/google-analytics-beacon)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/aravind-j/EvaluateCore) -->
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/aravind-j/EvaluateCore/total.svg)] -->
<!-- [![Rdoc](http://www.rdocumentation.org/badges/version/EvaluateCore)](http://www.rdocumentation.org/packages/EvaluateCore) -->

------------------------------------------------------------------------

## Description

Implements various quality evaluation statistics to assess the value of
plant germplasm core collections using qualitative and quantitative
phenotypic trait data according to Odong et al. (2015)
[doi:10.1007/s00122-012-1971-y](https://doi.org/10.1007/s00122-012-1971-y).

<!-- ## System Requirements
The function `dist.evaluate.core` is a wrapper around the `evaluateCore` function of the `corehunter` package which implemented in Java 8. Hence you need to have [Java Runtime Environment](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html) (JRE) version 8 or higher for the package to work.-->

## Installation

The package can be installed from CRAN as follows:

``` r
# Install from CRAN
install.packages('EvaluateCore', dependencies=TRUE)
```

The development version can be installed from github as follows:

``` r
# Install development version from Github
devtools::install_github("aravind-j/EvaluateCore")
```

<!-- ## Detailed tutorial
For a detailed tutorial (vignette) on how to used this package type:


```r
browseVignettes(package = 'EvaluateCore')
```
The vignette for the latest version is also available [online](https://aravind-j.github.io/EvaluateCore/articles.html).-->

## What’s new

To know whats new in this version type:

``` r
news(package='EvaluateCore')
```

## Links

[CRAN page](https://cran.r-project.org/package=EvaluateCore)

[Github page](https://github.com/aravind-j/EvaluateCore)

[Documentation website](https://aravind-j.github.io/EvaluateCore/)

[Zenodo DOI](https://doi.org/10.5281/zenodo.3875930)

## CRAN checks

    Warning: package 'RCurl' was built under R version 4.1.3

<table class="table table-striped table-hover" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
Flavour
</th>
<th style="text-align:left;">
CRAN check
</th>
</tr>
</thead>
<tbody>
<tr grouplength="6">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![Linux](https://shields.io/badge/Linux--9cf?logo=Linux&style=social)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-debian-clang
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-debian-clang](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-debian-clang/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-debian-gcc
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-debian-gcc](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-debian-gcc/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-fedora-clang
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-fedora-clang](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-fedora-clang/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-fedora-gcc
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-fedora-gcc](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-fedora-gcc/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-patched-linux-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-patched-linux-x86\_64](https://cranchecks.info/badges/flavor/r-patched-linux-x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-release-linux-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-release-linux-x86\_64](https://cranchecks.info/badges/flavor/r-release-linux-x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr grouplength="1">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![Solaris](https://shields.io/badge/Solaris--9cf?logo=Oracle&style=social)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-patched-solaris-x86
</td>
<td style="text-align:left;">
[![CRAN check -
r-patched-solaris-x86](https://cranchecks.info/badges/flavor/r-patched-solaris-x86/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr grouplength="3">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![Windows](https://shields.io/badge/Windows--9cf?logo=Windows&style=social)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-windows-ix86+x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-windows-ix86+x86\_64](https://cranchecks.info/badges/flavor/r-devel-windows-ix86+x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-release-windows-ix86+x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-release-windows-ix86+x86\_64](https://cranchecks.info/badges/flavor/r-release-windows-ix86+x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-oldrel-windows-ix86+x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-oldrel-windows-ix86+x86\_64](https://cranchecks.info/badges/flavor/r-oldrel-windows-ix86+x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr grouplength="2">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![MacOS](https://shields.io/badge/MacOS--9cf?logo=Apple&style=social)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-release-macos-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-release-macos-x86\_64](https://cranchecks.info/badges/flavor/r-release-macos-x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-oldrel-macos-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-oldrel-macos-x86\_64](https://cranchecks.info/badges/flavor/r-oldrel-macos-x86_64/EvaluateCore)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)
</td>
</tr>
</tbody>
</table>

## Citing `EvaluateCore`

To cite the methods in the package use:

``` r
citation("EvaluateCore")
```


    To cite the R package 'EvaluateCore' in publications use:

      Aravind, J., Kaur, V., Wankhede, D. P. and Nanjundan, J. (2022).  EvaluateCore:
      Quality Evaluation of Core Collections. R package version 0.1.3,
      https://aravind-j.github.io/EvaluateCore/https://CRAN.R-project.org/package=EvaluateCore.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {EvaluateCore: Quality Evaluation of Core Collections},
        author = {J. Aravind and Vikender Kaur and Dhammaprakash Pandhari Wankhede and J. Nanjundan},
        year = {2022},
        note = {R package version 0.1.3},
        note = {https://aravind-j.github.io/EvaluateCore/},
        note = {https://CRAN.R-project.org/package=EvaluateCore},
      }

    This free and open-source software implements academic research by the authors and
    co-workers. If you use it, please support the project by citing the package.
