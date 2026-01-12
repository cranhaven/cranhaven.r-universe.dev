
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`nlpred`

[![Travis-CI Build
Status](https://travis-ci.org/benkeser/nlpred.svg?branch=master)](https://travis-ci.org/benkeser/nlpred)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/benkeser/nlpred?branch=master&svg=true)](https://ci.appveyor.com/project/benkeser/nlpred)
[![Coverage
Status](https://img.shields.io/codecov/c/github/benkeser/nlpred/master.svg)](https://codecov.io/github/benkeser/nlpred?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![MIT
license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
<!-- [![CRAN](http://www.r-pkg.org/badges/version/nlpred)](http://www.r-pkg.org/pkg/nlpred) -->
<!-- [![CRAN downloads](https://cranlogs.r-pkg.org/badges/nlpred)](https://CRAN.R-project.org/package=nlpred) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.835868.svg)](https://doi.org/10.5281/zenodo.835868) -->

> Small-sample optimized estimators of cross-validated prediction
> metrics

[David Benkeser](https://www.benkeserstatistics.com/)

-----

## Description

`nlpred` is an R package for computing estimates of cross-validated
prediction metrics. These estimates are tailored for superior
performance in small samples. Several estimators are available including
ones based cross-validated targeted minimum loss-based estimation,
estimating equations, and one-step estimation.

-----

## Installation

For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via

``` r
install.packages("nlpred")
```

You can install the current release of `nlpred` from GitHub via
[`devtools`](https://CRAN.R-project.org/package=devtools) with:

``` r
devtools::install_github("benkeser/nlpred")
```

-----

## Usage

The main functions in the package are `cv_auc` and `cv_scrnp`, which are
used to compute, respectively, the `K`-fold [cross-validated area under
the receiver operating characteristics
curve](http://projecteuclid.org/euclid.ejs/1437742107) (CVAUC) and the
`K`-fold cross-validated [sensitivity constrained rate of negative
prediction](https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.7296).
However, rather than using standard cross-validation estimators (where
prediction algorithms are developed in a training sample and AUC/SCRNP
estimated using the validation sample), we instead use techniques from
efficiency theory to estimate these quantities. This allows us to use
the training data both to develop the prediction algorithm, *as well as*
key nuisance parameters needed to evaluate AUC/SCRNP. By reserving more
data for estimation of these key parameters, we obtain improved
performance in small samples.

``` r
# load package
library(nlpred)
#> Loading required package: data.table

# turn off messages from np package
options(np.messages=FALSE)

# simulate data
n <- 200
p <- 10
X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
Y <- rbinom(n, 1, plogis(X[,1] + X[,10]))

# get cv auc estimates for logistic regression
logistic_cv_auc_ests <- cv_auc(Y = Y, X = X, K = 5, learner = "glm_wrapper")
logistic_cv_auc_ests
#>                est         se       cil       ciu
#> cvtmle   0.7598522 0.03223410 0.6966745 0.8230299
#> onestep  0.7601000 0.03252870 0.6963449 0.8238551
#> esteq    0.7557129 0.03252870 0.6919578 0.8194680
#> standard 0.7660940 0.03348094 0.7004726 0.8317154

# get cv auc estimates for random forest using nested 
# cross-validation for nuisance parameter estimation. nested
# cross-validation is unfortunately necessary when aggressive learners 
# are used. 
rf_cv_auc_ests <- cv_auc(Y = Y, X = X, K = 5, 
                         learner = "randomforest_wrapper", 
                         nested_cv = TRUE)
rf_cv_auc_ests
#>                est         se       cil       ciu
#> cvtmle   0.7305404 0.03606462 0.6598550 0.8012257
#> onestep  0.7308869 0.03625171 0.6598349 0.8019390
#> esteq    0.7281639 0.03625171 0.6571118 0.7992159
#> standard 0.7435551 0.03553040 0.6739168 0.8131934

# same examples for scrnp
logistic_cv_scrnp_ests <- cv_scrnp(Y = Y, X = X, K = 5, learner = "glm_wrapper")
logistic_cv_scrnp_ests
#>                est         se        cil       ciu
#> cvtmle   0.1099379 0.03873987 0.03400918 0.1858667
#> onestep  0.1237150 0.03857579 0.04810785 0.1993222
#> esteq    0.1237150 0.03857579 0.04810785 0.1993222
#> standard 0.1612586 0.03851825 0.08576425 0.2367530


rf_cv_scrnp_ests <- cv_scrnp(Y = Y, X = X, K = 5, 
                         learner = "randomforest_wrapper", 
                         nested_cv = TRUE)
rf_cv_scrnp_ests
#>                 est         se         cil       ciu
#> cvtmle   0.09331934 0.02851627 0.037428470 0.1492102
#> onestep  0.09642105 0.02851279 0.040536999 0.1523051
#> esteq    0.09642105 0.02851279 0.040536999 0.1523051
#> standard 0.08475865 0.04111922 0.004166465 0.1653508
```

-----

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/benkeser/nlpred/issues).

-----

## Contributions

Interested contributors can consult our [`contribution
guidelines`](https://github.com/benkeser/nlpred/blob/master/CONTRIBUTING.md)
prior to submitting a pull request.

-----

## Citation

After using the `nlpred` package, please cite the following:

    @Manual{nlpredpackage,
      title = {nlpred: Estimators of Non-Linear Cross-Validated Risks Optimized for Small Samples},
      author = {David Benkeser},
      note = {R package version 1.0.1}
    }
    
    @article{benkeser2019improved,
      year  = {2019},
      author = {Benkeser, David C and Petersen, Maya and van der Laan, Mark J},
      title = {Improved Small-Sample Estimation of Nonlinear Cross-Validated Prediction Metrics},
      journal = {Journal of the American Statistical Association},
      doi = {10.1080/01621459.2019.1668794}
    }

## License

© 2019- David Benkeser

The contents of this repository are distributed under the MIT license.
See below for details:

    The MIT License (MIT)
    
    Copyright (c) 2019- David C. Benkeser
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
