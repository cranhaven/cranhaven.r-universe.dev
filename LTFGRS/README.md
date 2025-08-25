# LTFGRS

LTFGRS is an R package that provides a unified interface to perform several phenotype-based family genetic risk scores (FGRS) as well as functions to perform the required data preparation and management steps.

The implemented methods are [LT-FH++](https://doi.org/10.1016/j.ajhg.2022.01.009), [PA-FGRS](https://pubmed.ncbi.nlm.nih.gov/39471805/), and [Kendler's FGRS](https://pubmed.ncbi.nlm.nih.gov/33881469/). 
The primary focus of the package is LT-FH++ and PA-FGRS. They share many similarities in their models, since they are both based on the same underlying liability threshold model with modifications. This package allows users to pick and chose which elements of those methods they want to use.
Notable properties of the package are:

- Automatic identification of family members up to n'th degree.
- personalised thresholds for each included family that can account for:
  - age of diagnosis
  - censoring
  - cohort effects
  - sex
  - family-wise censoring
- mixture distribution to handle partially observed controls.

# Installation
You can install the development version of LTFGRS from GitHub with:


```{r eval=FALSE}
remotes::install_github("EmilMiP/LTFGRS")
```

You can install the LTFGRS from CRAN with:

```{r eval=FALSE}
install.packages("LTFGRS")
```

<!-- badges: start -->
[![R-CMD-check](https://github.com/EmilMiP/LTFGRS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EmilMiP/LTFGRS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# Documentation and Tutorials

Documentation and tutorials for the package can be found in the [pkgdown site](https://emilmip.github.io/LTFGRS/).

