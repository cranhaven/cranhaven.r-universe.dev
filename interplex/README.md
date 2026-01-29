# interplex

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/interplex)](https://CRAN.R-project.org/package=interplex)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This is an R package to facilitate conversion between different structures for simplicial complex data.

## usage

### installation

{interplex} is now on CRAN and can be installed as follows:

```r
install.packages("interplex")
```

### supported structures

{interplex} includes converters between the following data structures:

* complete lists of simplices, used by the [{TDA}](https://cran.r-project.org/package=TDA) package
* simplex tree instances of class 'Rcpp_SimplexTree',
  provided by the [{simplextree}](https://github.com/peekxc/simplextree) package
* simplex trees in [Python GUDHI](https://gudhi.inria.fr/python/latest/) imported using the [{reticulate}](https://rstudio.github.io/reticulate/) package
* objects of class 'igraph', provided by the [{igraph}](https://igraph.org/r/) package
* objects of class 'network', provided by the [{network}](https://github.com/statnet/network) package

Coercion among the graph/network classes is done using methods from the [{intergraph}](https://mbojan.github.io/intergraph/) package. Simplicial complexes are only directly coerced between the 'igraph' class.

## development

Install the development version of {interplex} from GitHub as follows:

```r
remotes::install_github("tdaverse/interplex")
```

### future directions

A future release should extend coercers from (unannotated) simplicial complexes to simplicial filtrations (in which simplices are annotated with real-values) and possibly arbitrary simplicial maps (with real-valued intervals).

### contribute

Contributions in any form are more than welcome!
See the
[CONTRIBUTING](https://github.com/tdaverse/interplex/blob/main/CONTRIBUTING.md)
file for guidance, and please respect the [Code of
Conduct](https://github.com/tdaverse/interplex/blob/main/CODE_OF_CONDUCT.md).

## acknowledgments

This package was designed and developed in part through discussions with Matt Piekenbrock and Raoul Wadhwa.

### resources

Development of this package benefitted from the use of equipment and the
support of colleagues at [UF Health](https://ufhealth.org/).
