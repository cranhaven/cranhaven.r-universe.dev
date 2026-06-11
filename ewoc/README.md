
<!-- README.md is generated from README.Rmd. Please edit that file -->
EWOC
====

[![Travis-CI Build Status](https://travis-ci.org/dnzmarcio/ewoc.svg?branch=master)](https://travis-ci.org/dnzmarcio/ewoc) [![](http://cranlogs.r-pkg.org/badges/grand-total/ewoc)](https://CRAN.R-project.org/package=ewoc)

Escalation With Overdose Control is a dose escalation design for phase I clinical trials such that the probability of overdose is controlled explicitly.

It was first introduced by Babb et al. (1998) and several modifications have been studied along of the years. This R-package has three available designs: the classical EWOC introduced by Babb et al. (1998), the proportional hazards model in discussed Tighioaurt (2014), and the extended parametrization presented by Tighioaurt et al (2017).

Installation
------------

Before installing the R-package EWOC, you may need to install [Just Another Gibbs Sampler](http://mcmc-jags.sourceforge.net/).

The R-package EWOC can be installed from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("dnzmarcio/ewoc")
```

Example
-------

A new dose using the classical EWOC can be calculated:

``` r
library(ewoc)
DLT <- 0
dose <- 30
test <- ewoc_d1classic(DLT ~ dose, type = 'discrete',
                       theta = 0.33, alpha = 0.25,
                       min_dose = 0, max_dose = 100,
                       dose_set = seq(0, 100, 20),
                       rho_prior = matrix(1, ncol = 2, nrow = 1),
                       mtd_prior = matrix(1, ncol = 2, nrow = 1),
                       rounding = "nearest")
```

``` r
summary(test)
#> Conditions
#>   Minimum Dose Maximum Dose Theta Alpha Number of patients
#> 1            0          100  0.33  0.25                  1
#> 
#> Next Dose
#>   Estimate         95% HPD
#> 1       40 (12.87 ; 98.77)
#> 
#> P(DLT| next dose)
#>   Estimate      95% HPD
#> 1      0.3 (0.07 ; 0.7)
```

In addition, simulations also can be performed to evaluate a design:

``` r
library(ewoc)
DLT <- 0
dose <- 20
step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
                            theta = 0.33, alpha = 0.25,
                            min_dose = 20, max_dose = 100,
                            dose_set = seq(0, 100, 20),
                            rho_prior = matrix(1, ncol = 2, nrow = 1),
                            mtd_prior = matrix(1, ncol = 2, nrow = 1),
                            rounding = "nearest")
response_sim <- response_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
                                   min_dose = 20, max_dose = 100)
sim <- ewoc_simulation(step_zero = step_zero,
                        n_sim = 1, sample_size = 30,
                        alpha_strategy = "conditional",
                        response_sim = response_sim,
                        ncores = 1)
pdlt <- pdlt_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
                      min_dose = 20, max_dose = 100)
results <- opc(sim_list = list(sim), pdlt_list = list(pdlt),
    mtd_list = list(60), toxicity_margin = 0.05, mtd_margin = 6)
```

References
----------

Babb, J., Rogatko, A., & Zacks, S. (1998). Cancer phase I clinical trials: efficient dose escalation with overdose control. Statistics in medicine, 17(10), 1103-1120.

Tighiouart, M., Liu, Y., & Rogatko, A. (2014). Escalation with overdose control using time to toxicity for cancer phase I clinical trials. PloS one, 9(3), e93070.

Tighiouart, M., Cook-Wiens, G., & Rogatko, A. (2018). A Bayesian adaptive design for cancer phase I trials using a flexible range of doses. Journal of biopharmaceutical statistics, 28(3), 562-574.

Diniz, M. A., Tighiouart, M., & Rogatko, A. (2019). Comparison between continuous and discrete doses for model based designs in cancer dose finding. PloS one, 14(1).
