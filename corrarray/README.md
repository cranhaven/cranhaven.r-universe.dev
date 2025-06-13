
# corrarray

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/Medicine1/corrarray.svg?branch=master)](https://travis-ci.com/Medicine1/corrarray)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Medicine1/corrarray?branch=master&svg=true)](https://ci.appveyor.com/project/Medicine1/corrarray)
<!-- badges: end -->

The goal of ‘corrarray’ is to create a multi-sample correlation array by
combining the correlation matrices of a data set stratified by a
grouping variable. For two specified levels of the variable, ‘corrarray’
displays one level’s correlation matrix in the lower triangular matrix
and the other level’s in the upper triangular matrix. Such an output can
enable visualization of correlations from two samples in a single
correlation matrix or corrgram.

## Installation

You can install the released version of ‘corrarray’ from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("corrarray")
```

## Example

The following illustrates how ‘corrarray’ can be used to generate a 1-
or 2-sample correlation matrix or a k-sample correlation array:

``` r
library(corrarray)
## All observations: 1-sample correlation matrix.
corrarray(iris)
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
#> Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
#> Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
#> Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

## Stratify by the three species: 3-sample correlation array.
corrarray(iris, "Species", output = "array")
#> , , Sample = setosa
#> 
#>               
#>                Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   Sepal.Length    1.0000000   0.7425467    0.2671758   0.2780984
#>   Sepal.Width     0.7425467   1.0000000    0.1777000   0.2327520
#>   Petal.Length    0.2671758   0.1777000    1.0000000   0.3316300
#>   Petal.Width     0.2780984   0.2327520    0.3316300   1.0000000
#> 
#> , , Sample = versicolor
#> 
#>               
#>                Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   Sepal.Length    1.0000000   0.5259107    0.7540490   0.5464611
#>   Sepal.Width     0.5259107   1.0000000    0.5605221   0.6639987
#>   Petal.Length    0.7540490   0.5605221    1.0000000   0.7866681
#>   Petal.Width     0.5464611   0.6639987    0.7866681   1.0000000
#> 
#> , , Sample = virginica
#> 
#>               
#>                Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   Sepal.Length    1.0000000   0.4572278    0.8642247   0.2811077
#>   Sepal.Width     0.4572278   1.0000000    0.4010446   0.5377280
#>   Petal.Length    0.8642247   0.4010446    1.0000000   0.3221082
#>   Petal.Width     0.2811077   0.5377280    0.3221082   1.0000000

## Specify lower and upper samples: 2-sample correlation matrix.
corrarray(iris, "Species", lower = "setosa", upper = "virginica")
#> [1] "Sample1 (lower triangular matrix) is 'setosa' (n=50)."   
#> [2] "Sample2 (upper triangular matrix) is 'virginica' (n=50)."
#>               Sample2
#> Sample1        Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   Sepal.Length    1.0000000   0.4572278    0.8642247   0.2811077
#>   Sepal.Width     0.7425467   1.0000000    0.4010446   0.5377280
#>   Petal.Length    0.2671758   0.1777000    1.0000000   0.3221082
#>   Petal.Width     0.2780984   0.2327520    0.3316300   1.0000000
```
