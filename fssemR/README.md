## fssemR
[![CRAN status](https://www.r-pkg.org/badges/version/fssemR)](https://cran.r-project.org/package=fssemR)
[![CRAN download](https://cranlogs.r-pkg.org/badges/fssemR)](https://cranlogs.r-pkg.org/badges/fssemR)


`fssemR` is a package that ultilizes the Proximal Alternating Linearized Maximal to solve the 
non-convex non-smooth jointly fused sparse structrual equation model. 

## Installation

`fssemR` package contains a lot of necessary scripts to analyze large dataset such as microarray and SNP data
from GEO database, so it has not been submitted to CRAN yet for these non-standard directory.
To install `fssemR`, you need a C++ compiler such as `g++` or `clang++` with C++11 feature,
and for Windows users, the [Rtools](https://cran.r-project.org/bin/windows/Rtools/index.html)
software is needed (unless you can configure the toolchain by yourself).

The installation follows the typical way of R packages on Github:

```r
library(devtools)
install_github("Ivis4ml/fssemR")
```

Now, `fssemR` package is uploaded on CRAN. So you will install it via CRAN 

```r
install.packages("fssemR")
```

## Vignette
[fssemR-introduction](https://github.com/Ivis4ml/fssemR/blob/master/inst/doc/fssemR.pdf)

## Citation
Xin Zhou, Xiaodong Cai, Inference of differential gene regulatory networks based on gene expression and genetic perturbation data, Bioinformatics, , btz529, https://doi.org/10.1093/bioinformatics/btz529

## Update
Upgrade to Eigen 0.3.4 compatible

