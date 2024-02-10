
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StratifiedSampling package

In this R package, different functions are implemented for selecting
samples .

-   If the population of interest is stratified. Different functions are
    implemented, for more details see
    <https://doi.org/10.1007/s42081-021-00134-y>.
-   If two datasets are available for statistical matching. A method
    based on optimal transport is implemented, for more details see
    <https://arxiv.org/abs/2105.08379>.
-   If you are interested in the Sequential Spatially Balanced method.
    <https://arxiv.org/abs/2112.01164>

The package contains also some useful functions. Look at the manual of
the package for more information.

## Installation

### CRAN version

    install.packages("StratifiedSampling")

### Latest version

You can install the latest version of the package `StratifiedSampling`
with the following command:

``` r
# install.packages("devtools")
devtools::install_github("Rjauslin/StratifiedSampling")
```

## Optimal transport matching

The package proposes a method to do statistical matching using optimal
transport and balanced sampling. For more details see Raphaël Jauslin
and Yves Tillé (2021) <https://arxiv.org/abs/2105.08379>. A complete
example on how to use the package to make an optimal statistical
transport match can be found in the following vignette:

    vignette("ot_matching", package = "StratifiedSampling")

## Sequential spatially balanced sampling

The package proposes a method to select a well-spread sample balanced on
some auxiliary variables. For more details see Raphaël Jauslin and Yves
Tillé (2022) <https://arxiv.org/abs/2112.01164>. A complete example on
how to use the different functions to select a well-spread and balanced
sample can be found in the following vignette:

    vignette("sequential_balanced", package = "StratifiedSampling")

## Simple example on stratified population

Integrating a stratified structure in the population in a sampling
design can considerably reduce the variance of the Horvitz-Thompson
estimator. We propose in this package different methods to handle the
selection of a balanced sample in stratified population. For more
details see Raphaël Jauslin, Esther Eustache and Yves Tillé (2021)
<https://doi.org/10.1007/s42081-021-00134-y>.

This basic example shows you how to set up a stratified sampling design.
The example is done on the `swissmunicipalities` dataset from the
package `sampling`.

``` r
library(sampling)
library(StratifiedSampling)
#> Le chargement a nécessité le package : Matrix

data(swissmunicipalities)
swiss <- swissmunicipalities
X <- cbind(swiss$HApoly,
        swiss$Surfacesbois,
        swiss$P00BMTOT,
        swiss$P00BWTOT,
        swiss$POPTOT,
        swiss$Pop020,
        swiss$Pop2040,
        swiss$Pop4065,
        swiss$Pop65P,
        swiss$H00PTOT )

X <- X[order(swiss$REG),]
strata <- swiss$REG[order(swiss$REG)]
```

Strata are NUTS region of the Switzerland. Inclusion probabilities `pik`
is set up equal within strata and such that the sum of the inclusion
probabilities within strata is equal to 80.

``` r
pik <- sampling::inclusionprobastrata(strata,rep(80,7))
```

It remains to use the function `stratifiedcube()`.

``` r
s <- stratifiedcube(X,strata,pik)
```

We can check that we have correctly selected the sample. It is balanced
and have the right number of units selected in each stratum.

``` r
head(s)
#> [1] 0 1 0 0 1 0

sum(s)
#> [1] 560
t(X/pik)%*%s
#>          [,1]
#>  [1,] 4035368
#>  [2,] 1279586
#>  [3,] 3736660
#>  [4,] 3901323
#>  [5,] 7637982
#>  [6,] 1734546
#>  [7,] 2289617
#>  [8,] 2443589
#>  [9,] 1170231
#> [10,] 3301463
t(X/pik)%*%pik
#>          [,1]
#>  [1,] 3998831
#>  [2,] 1270996
#>  [3,] 3567567
#>  [4,] 3720443
#>  [5,] 7288010
#>  [6,] 1665613
#>  [7,] 2141059
#>  [8,] 2362332
#>  [9,] 1119006
#> [10,] 3115399

Xcat <- disj(strata)

t(Xcat)%*%s
#>      [,1]
#> [1,]   80
#> [2,]   80
#> [3,]   80
#> [4,]   80
#> [5,]   80
#> [6,]   80
#> [7,]   80
t(Xcat)%*%pik
#>      [,1]
#> [1,]   80
#> [2,]   80
#> [3,]   80
#> [4,]   80
#> [5,]   80
#> [6,]   80
#> [7,]   80
```
