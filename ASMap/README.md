# ASMap

<!-- badges: start -->
[![R-CMD-check](https://github.com/DrJ001/ASMap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DrJ001/ASMap/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Linkage Map Construction using the MSTmap Algorithm 

Functions for Accurate and Speedy linkage map construction, manipulation and diagnosis of Doubled Haploid, Backcross and Recombinant Inbred 'R/qtl' objects. This includes extremely fast linkage map clustering and optimal marker ordering using 'MSTmap' (see Wu et al.,2008).


## Installing

To install the latest version from CRAN, use the following:

```r
install.packages("ASMap")
```

To install the latest development version from GitHub, use the following:

```r
if(!require("remotes")) install.packages("remotes") 
remotes::install_github("DrJ001/ASMap")
```

## Using the package

Load the package and start using it with:

```r
library(ASMap)
```

If you find this package useful, please cite it! Type `citation("ASMap")` on the R console to find out how.
