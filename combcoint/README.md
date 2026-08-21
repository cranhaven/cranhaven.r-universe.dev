
<!-- README.md is generated from README.Rmd. Please edit that file -->

# combcoint

<!-- badges: start -->

[![R-CMD-check](https://github.com/Janine-Langerbein/combcoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Janine-Langerbein/combcoint/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`combcoint` is an `R` package which provides, among other things, the
*combined non-cointegration tests* of Bayer and Hanck. The following
cointegration tests are implemented:

-   `bayerhanck()` to perform the *Combining non-cointegration test*
    which is implemented in two different versions.
-   `boswijk()` performs the structural error correction model
    cointegration test by Boswijk
    [(1994)](https://doi.org/10.1016/0304-4076(93)01560-9)
-   `banerjee()` performs the error-correction-based cointregration test
    by Banerjee [(1998)](https://doi.org/10.1111/1467-9892.00091)
-   `englegranger()` performs the residual-based cointregration test by
    Engle and Granger [(1987)](https://doi.org/10.2307/1913236) and is a
    wrapper for the `ur.df` of the `urca` package
-   `johansen()` performs the system-based maximum likelihood
    cointegration test by Johansen
    [(1988)](https://doi.org/10.1016/0165-1889(88)90041-3) and is a
    wrapper function for the `rank.test` of the `tsDyn` package. Note
    that first the transformation to a VECM is needed.

## Installation

<!--
You can install the released version of combcoint from [CRAN](https://CRAN.R-project.org) with:


```r
install.packages("combcoint")
```
-->

The development version can be downloaded from
[GitHub](https://github.com/Janine-Langerbein/combcoint) with:

``` r
# install.packages("devtools")
devtools::install_github("Janine-Langerbein/combcoint")
```

## Usage

First install the package as described above

``` r

# example data from the MTS package 
data("mts-examples", package = "MTS")

bayerhanck(sp ~ ibm + ko, data = ibmspko, lags = 1, trend = "const", test = "all")

banerjee(sp ~ ibm + ko, data = ibmspko, lags = 1, trend = "const")

boswijk(sp ~ ibm + ko, data = ibmspko, lags = 1, trend = "const")

englegranger(sp ~ ibm + ko, data = ibmspko, lags = 1, trend = "const")

johansen(sp ~ ibm + ko, data = ibmspko, type = "eigen", lags = 1, trend = "const")
```

## Reference

Bayer, C. and Hanck, C. (2013). Combining non-cointegration tests.
*Journal of Time Series Analysis*, 34(1), 83 – 95.
<https://doi.org/10.1111/j.1467-9892.2012.00814.x>

Boswijk, H. P. (1994), Testing for an unstable root in conditional and
structural error correction models, *Journal of Econometrics* 63(1),
37-60. <https://doi.org/10.1016/0304-4076(93)01560-9>

Johansen, S. (1988), Statistical analysis of cointegration vectors,
*Journal of Economic Dynamics and Control* 12(2-3), 231-254.
<https://doi.org/10.1016/0165-1889(88)90041-3>

Banerjee, A., Dolado, J. J. and Mestre, R. (1998), Error-correction
Mechanism Tests for Cointegration in a Single-equation Framework,
*Journal of Times Series Analysis* 19(3), 267-283.
<https://doi.org/10.1111/1467-9892.00091>

Engle, R. and Granger, C. (1987), Co-integration and Error Correction:
Representation, Estimation, and Testing, *Econometrica* 55(2), 251-76.
<https://doi.org/10.2307/1913236>

------------------------------------------------------------------------

Please note that this project and package is licensed under the
[MIT](https://github.com/Janine-Langerbein/combcoint/blob/main/LICENSE.md)
license.
