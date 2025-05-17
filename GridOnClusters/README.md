---
title: The 'GridOnClusters' R package
bibliography: inst/REFERENCES.bib
---
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/GridOnClusters)](https://cran.r-project.org/package=GridOnClusters)
[![CRAN_latest_release_date](https://www.r-pkg.org/badges/last-release/GridOnClusters)](https://cran.r-project.org/package=GridOnClusters)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/GridOnClusters)](https://cran.r-project.org/package=GridOnClusters)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/GridOnClusters)](https://cran.r-project.org/package=GridOnClusters)



### Overview

The package offers a method to discretize multivariate continuous data using a grid that captures the joint distribution via preserving clusters in the original data (Wang, Kumar, and Song 2020). Joint grid discretization is applicable as a data transformation step before using other methods to infer association, function, or causality without assuming a parametric model.

### When to use the package

Most available discretization methods process one variable at a time, such as ['Ckmeans.1d.dp'](https://cran.r-project.org/package=Ckmeans.1d.dp). If discretizing each variable independently misses patterns arising from the joint distribution of multiple involved variables, one may benefit from using the joint discretization method in this package.

### To download and install the package

```{r}
install.packages("GridOnClusters")
```

### Examples

See the *Examples* vignette of the package.

### Citing the package

Wang J, Kumar S, Song M (2020). "Joint Grid
Discretization for Biological Pattern Discovery." In
_Proceedings of the 11th ACM International Conference
on Bioinformatics, Computational Biology and Health
Informatics_. Article no. 57. doi: 10.1145/3388440.3412415 (URL:
https://doi.org/10.1145/3388440.3412415).

