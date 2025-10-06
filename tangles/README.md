[![R-CMD-check](https://github.com/brendo1001/tangles/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brendo1001/tangles/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tangles)](https://cran.r-project.org/package=tangles)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tangles)](https://cran.r-project.org/package=tangles)

# Tangles

An R package for anonymisation of spatial point patterns and raster objects.

This package achieves the relatively simple, yet useful, task of spatial anonymisation. Anonymisation is needed in situations where a data owner may not wish to share the actual spatial locations of their data, but is happy to share transformed data in a way that still permits valid spatial analysis.

Anonymisation is achieved via three modes of spatial shifts:

* Vertical shifts  
* Lateral shifts  
* Rotational shifts  

The `tangles` package can entangle both non-gridded spatial point patterns and raster objects. It can also entangle data using a pre-defined entanglement sequence and can disentangle data back to their original spatial representations. Each entanglement process is given a unique hash key label to guarantee successful restoration of the original spatial structure.

## 📦 Package Installation

Install the development version of `tangles` from GitHub using the `devtools` package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install tangles
devtools::install_github("brendo1001/tangles")
```



## 📬 Contact

[brendan.malone@csiro.au](mailto:brendan.malone@csiro.au)
