
<script type="application/ld+json">
      {
  "@context": "https://schema.org",
  "@graph": [
    {
      "type": "SoftwareSourceCode",
      "author": [
        {
          "id": "https://orcid.org/0000-0001-7222-8483",
          "type": "Person",
          "email": "ajaygpb@yahoo.co.in",
          "familyName": "Ajay",
          "givenName": [
            "B.",
            "C."
          ]
        },
        {
          "id": "https://orcid.org/0000-0002-4791-442X",
          "type": "Person",
          "email": "j.aravind@icar.gov.in",
          "familyName": "Aravind",
          "givenName": "J."
        },
        {
          "id": "https://orcid.org/0000-0001-6261-7071",
          "type": "Person",
          "email": "fiyaz.ra@icar.gov.in",
          "familyName": "Abdul Fiyaz",
          "givenName": "R."
        }
      ],
      "codeRepository": "https://github.com/ajaygpb/ammistability/",
      "copyrightHolder": {
        "type": "Organization",
        "name": "ICAR"
      },
      "description": "Computes various stability parameters from Additive Main Effects and Multiplicative Interaction (AMMI) analysis results such as Modified AMMI Stability Value (MASV), Sums of the Absolute Value of the Interaction Principal Component Scores (SIPC), Sum Across Environments of Genotype-Environment Interaction Modelled by AMMI (AMGE), Sum Across Environments of Absolute Value of Genotype-Environment Interaction Modelled by AMMI (AV_(AMGE)), AMMI Stability Index (ASI), Modified ASI (MASI), AMMI Based Stability Parameter (ASTAB), Annicchiarico's D Parameter (DA), Zhang's D Parameter (DZ), Averages of the Squared Eigenvector Values (EV), Stability Measure Based on Fitted AMMI Model (FA), Absolute Value of the Relative Contribution of IPCs to the Interaction (Za). Further calculates the Simultaneous Selection Index for Yield and Stability from the computed stability parameters. See the vignette for complete list of citations for the methods implemented.",
      "license": "https://spdx.org/licenses/GPL-2.0",
      "name": "ammistability: Additive Main Effects and Multiplicative Interaction Model Stability Parameters",
      "programmingLanguage": {
        "type": "ComputerLanguage",
        "name": "R",
        "url": "https://r-project.org"
      },
      "runtimePlatform": "R Under development (unstable) (2023-04-28 r84338 ucrt)",
      "version": "0.1.4"
    },
    {
      "type": "SoftwareSourceCode",
      "author": [
        {
          "id": "https://orcid.org/0000-0001-7222-8483",
          "type": "Person",
          "email": "ajaygpb@yahoo.co.in",
          "familyName": "Ajay",
          "givenName": [
            "B.",
            "C."
          ]
        },
        {
          "id": "https://orcid.org/0000-0002-4791-442X",
          "type": "Person",
          "email": "j.aravind@icar.gov.in",
          "familyName": "Aravind",
          "givenName": "J."
        },
        {
          "type": "Person",
          "email": "fiyaz.ra@icar.gov.in",
          "familyName": "Abdul Fiyaz",
          "givenName": "R."
        }
      ],
      "description": [
        "R package version 0.1.4",
        "https://ajaygpb.github.io/ammistability/",
        "https://CRAN.R-project.org/package=ammistability"
      ],
      "name": "ammistability: Additive Main Effects and Multiplicative Interaction Model Stability Parameters"
    }
  ]
}
    </script>

## `ammistability`: Additive Main Effects and Multiplicative Interaction Model Stability Parameters <img src="https://raw.githubusercontent.com/ajaygpb/ammistability/master/inst/extdata/ammistability.png" align="right" alt="logo" width="173" height = "200" style = "padding: 10px; border: none; float: right;">

###### Version : [0.1.4](https://ajaygpb.github.io/ammistability/); Copyright (C) 2017-2023: [ICAR-DGR](https://en.wikipedia.org/wiki/Directorate_of_Groundnut_Research); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### *Ajay, B. C.<sup>1</sup>, Aravind, J.<sup>2</sup> and Abdul Fiyaz, R<sup>3</sup>*

1.  RRS, ICAR-Directorate of Groundnut Research, Anantapur.
2.  ICAR-National Bureau of Plant Genetic Resources, New Delhi.
3.  ICAR-Indian Institute of Rice Research, Hyderabad.

------------------------------------------------------------------------

<!-- badges: start -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/ammistability)](https://cran.r-project.org/package=ammistability)
[![Dependencies](https://tinyverse.netlify.com/badge/ammistability)](https://cran.r-project.org/package=ammistability)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ammistability?color=green)](https://CRAN.R-project.org/package=ammistability)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.3.9000-orange.svg)](https://github.com/ajaygpb/ammistability)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/ajaygpb/ammistability.svg)](https://github.com/ajaygpb/ammistability)
[![R-CMD-check](https://github.com/ajaygpb/ammistability/workflows/R-CMD-check/badge.svg)](https://github.com/ajaygpb/ammistability/actions)
[![Project Status:
Inactive](http://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2023--05--23-yellowgreen.svg)](https://github.com/ajaygpb/ammistability/commits/master)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1344756.svg)](https://doi.org/10.5281/zenodo.1344756)
[![Pub
DOI](https://img.shields.io/badge/article-10.31742%2FIJGPB.79.2.10-blue.svg)](https://www.isgpb.org/journal/index.php/IJGPB/article/view/2848)
[![Website -
pkgdown](https://img.shields.io/website-up-down-green-red/https/ajaygpb.github.io/ammistability.svg)](https://ajaygpb.github.io/ammistability/)
[![.](https://pro-pulsar-193905.appspot.com/UA-123032895-2/welcome-page)](https://github.com/aravind-j/google-analytics-beacon)
<!-- [![Rdoc](https://www.rdocumentation.org/badges/version/ammistability)](https://www.rdocumentation.org/packages/ammistability) -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/ajaygpb/ammistability) -->
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/ajaygpb/ammistability/total.svg)] -->
<!-- badges: end -->

------------------------------------------------------------------------

## Description

Computes various stability parameters from Additive Main Effects and
Multiplicative Interaction (AMMI) analysis results such as Modified AMMI
Stability Value (MASV), Sums of the Absolute Value of the Interaction
Principal Component Scores (SIPC), Sum Across Environments of
Genotype-Environment Interaction Modelled by AMMI (AMGE), Sum Across
Environments of Absolute Value of Genotype-Environment Interaction
Modelled by AMMI (AV\_(AMGE)), AMMI Stability Index (ASI), Modified ASI
(MASI), AMMI Based Stability Parameter (ASTAB), Annicchiarico’s D
Parameter (DA), Zhang’s D Parameter (DZ), Averages of the Squared
Eigenvector Values (EV), Stability Measure Based on Fitted AMMI Model
(FA), Absolute Value of the Relative Contribution of IPCs to the
Interaction (Za). Further calculates the Simultaneous Selection Index
for Yield and Stability from the computed stability parameters. See the
vignette for complete list of citations for the methods implemented.

## Installation

The package can be installed from CRAN as follows:

``` r
# Install from CRAN
install.packages('ammistability', dependencies=TRUE)
```

The development version can be installed from github as follows:

``` r
# Install development version from Github
devtools::install_github("ajaygpb/ammistability")
```

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = 'ammistability')
```

The vignette for the latest version is also available
[online](https://ajaygpb.github.io/ammistability/articles/Introduction.html).

## What’s new

To know whats new in this version type:

``` r
news(package='ammistability')
```

## Links

[CRAN page](https://cran.r-project.org/package=ammistability)

[Github page](https://github.com/ajaygpb/ammistability)

[Documentation website](https://ajaygpb.github.io/ammistability/)

[Zenodo DOI](https://doi.org/10.5281/zenodo.1344756)

<!-- 
## CRAN checks




[![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html) 

                                                                                                                                                     
----------------------------------  -----------------------------------------------------------------------------------------------------------------
r-devel-linux-x86_64-debian-clang   [![CRAN check - r-devel-linux-x86_64-debian-clang](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-debian-clang/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-devel-linux-x86_64-debian-gcc     [![CRAN check - r-devel-linux-x86_64-debian-gcc](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-debian-gcc/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-devel-linux-x86_64-fedora-clang   [![CRAN check - r-devel-linux-x86_64-fedora-clang](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-fedora-clang/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-devel-linux-x86_64-fedora-gcc     [![CRAN check - r-devel-linux-x86_64-fedora-gcc](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-fedora-gcc/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-patched-linux-x86_64              [![CRAN check - r-patched-linux-x86_64](https://cranchecks.info/badges/flavor/r-patched-linux-x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-release-linux-x86_64              [![CRAN check - r-release-linux-x86_64](https://cranchecks.info/badges/flavor/r-release-linux-x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
----------------------------------  -----------------------------------------------------------------------------------------------------------------

[![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html) 

                                                                                                                                             
------------------------------  -------------------------------------------------------------------------------------------------------------
r-devel-windows-ix86+x86_64     [![CRAN check - r-devel-windows-ix86+x86_64](https://cranchecks.info/badges/flavor/r-devel-windows-ix86+x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-release-windows-ix86+x86_64   [![CRAN check - r-release-windows-ix86+x86_64](https://cranchecks.info/badges/flavor/r-release-windows-ix86+x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-oldrel-windows-ix86+x86_64    [![CRAN check - r-oldrel-windows-ix86+x86_64](https://cranchecks.info/badges/flavor/r-oldrel-windows-ix86+x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
------------------------------  -------------------------------------------------------------------------------------------------------------

[![MacOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=apple&logoColor=white)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html) 

                                                                                                                               
-----------------------  ------------------------------------------------------------------------------------------------------
r-release-macos-x86_64   [![CRAN check - r-release-macos-x86_64](https://cranchecks.info/badges/flavor/r-release-macos-x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-oldrel-macos-x86_64    [![CRAN check - r-oldrel-macos-x86_64](https://cranchecks.info/badges/flavor/r-oldrel-macos-x86_64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-release-macos-arm64    [![CRAN check - r-release-macos-arm64](https://cranchecks.info/badges/flavor/r-release-macos-arm64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
r-oldrel-macos-arm64     [![CRAN check - r-oldrel-macos-arm64](https://cranchecks.info/badges/flavor/r-oldrel-macos-arm64/ammistability)](https://cran.r-project.org/src/contrib/Archive/ammistability/)
-----------------------  ------------------------------------------------------------------------------------------------------

-->

## Citing `ammistability`

To cite the methods in the package use:

``` r
citation("ammistability")
```

    To cite the R package 'ammistability' in publications use:

      Ajay, B. C., Aravind, J., and Abdul Fiyaz, R. (2019). ammistability: R package for ranking genotypes based on stability
      parameters derived from AMMI model. Indian Journal of Genetics and Plant Breeding (The), 79(2), 460-466.
      https://www.isgpb.org/article/ammistability-r-package-for-ranking-genotypes-based-on-stability-parameters-derived-from-ammi-model

      Ajay, B. C., Aravind, J., and Abdul Fiyaz, R. ().  ammistability: Additive Main Effects and Multiplicative Interaction
      Model Stability Parameters. R package version 0.1.4, https://ajaygpb.github.io/ammistability/,
      https://CRAN.R-project.org/package=ammistability.

    This free and open-source software implements academic research by the authors and co-workers. If you use it, please
    support the project by citing the package.

    To see these entries in BibTeX format, use 'print(<citation>, bibtex=TRUE)', 'toBibtex(.)', or set
    'options(citation.bibtex.max=999)'.
