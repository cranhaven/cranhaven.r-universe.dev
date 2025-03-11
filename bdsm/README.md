
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsm

<!-- badges: start -->
<!-- badges: end -->

The goal of bdsm is to provide tools to model panel data.

## Installation

You can install the released version of bdsm from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bdsm")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mateuszwyszynski/bdsm")
```

## Basic Usage

``` r
library(magrittr)
devtools::load_all()
#> ℹ Loading bdsm

set.seed(20)

# STEP 1
# Prepare data
#
# Features are scaled and centralized around the mean.
# Then they are centralized around the mean within cross-sections
data_prepared <- bdsm::economic_growth[,1:7] %>%
  feature_standardization(timestamp_col = year, entity_col = gdp) %>%
  feature_standardization(timestamp_col = year, entity_col = country,
                          cross_sectional = TRUE, scale = FALSE)

# If needed track computation time
library(tictoc)
tic()

# STEP 2
# Find optimal model space
#
# Parameters for each model are initialized with init_value. Then MLE for each
# model is searched numerically
model_space <-
  optimal_model_space(df = data_prepared, dep_var_col = gdp,
                      timestamp_col = year, entity_col = country,
                      init_value = 0.5)
#> initial  value 700.805202 
#> iter 100 value 63.489964
#> final  value 62.413224 
#> converged
#> initial  value 755.227924 
#> iter 100 value -7.081746
#> final  value -9.945489 
#> converged
#> initial  value 731.149545 
#> iter 100 value -5.728710
#> final  value -7.336652 
#> converged
#> initial  value 865.987070 
#> iter 100 value -75.528524
#> final  value -81.347491 
#> converged
#> initial  value 821.652572 
#> iter 100 value 12.593978
#> final  value 9.478839 
#> converged
#> initial  value 788.836005 
#> iter 100 value -61.377699
#> final  value -62.826938 
#> converged
#> initial  value 747.497621 
#> iter 100 value -56.890341
#> final  value -58.756416 
#> converged
#> initial  value 792.979857 
#> iter 100 value -127.201548
#> final  value -132.333776 
#> converged
#> initial  value 665.161630 
#> iter 100 value -242.698752
#> final  value -256.663454 
#> converged
#> initial  value 708.042561 
#> iter 100 value -326.609584
#> final  value -328.729156 
#> converged
#> initial  value 678.350819 
#> iter 100 value -315.110317
#> final  value -323.599046 
#> converged
#> initial  value 802.194508 
#> iter 100 value -392.089550
#> final  value -397.592119 
#> converged
#> initial  value 750.297273 
#> iter 100 value -296.583891
#> final  value -309.577948 
#> converged
#> initial  value 709.806113 
#> iter 100 value -375.455174
#> final  value -381.498968 
#> converged
#> initial  value 661.102743 
#> iter 100 value -365.114246
#> final  value -375.096037 
#> converged
#> initial  value 701.509591 
#> iter 100 value -436.936673
#> final  value -448.914323 
#> converged



print(paste("Computation Time:", toc()))
#> 50.095 sec elapsed
#> [1] "Computation Time: c(elapsed = 4.228)" 
#> [2] "Computation Time: c(elapsed = 54.323)"
#> [3] "Computation Time: logical(0)"         
#> [4] "Computation Time: 50.095 sec elapsed"
tic()

# STEP 3
# Compute intermediate BMA results
bma_result <- bma_summary(df = data_prepared, dep_var_col = gdp,
                          timestamp_col = year, entity_col = country,
                          model_space = model_space)
#> [1] "Prior Mean Model Size: 2"
#> [1] "Prior Inclusion Probability: 0.5"

print(paste("Computation Time:", toc()))
#> 15.783 sec elapsed
#> [1] "Computation Time: c(elapsed = 54.323)"
#> [2] "Computation Time: c(elapsed = 70.106)"
#> [3] "Computation Time: logical(0)"         
#> [4] "Computation Time: 15.783 sec elapsed"

# STEP 4
# Summary for parameters of interest
regressors <- bdsm:::regressor_names(data_prepared, year, country, gdp)

bma_params_summary <- bdsm:::parameters_summary(
  regressors = regressors, bet = bma_result$bet, pvarh = bma_result$pvarh,
  pvarr = bma_result$pvarr, fy = bma_result$fy, fyt = bma_result$fyt,
  ppmsize = bma_result$ppmsize, cout = bma_result$cout, nts = bma_result$nts,
  pts = bma_result$pts, variables_n = bma_result$variables_n
  )
#> [1] "Posterior Mean Model Size:  4.08027534311768"

bma_params_summary
#>    varname          postprob                 pmean                std
#>      alpha                 1      1.00832558392276  0.110742334576895
#> V1     ish 0.724215389851411       0.1218982030087 0.0316376449005969
#> V2     sed  0.70775503500249 -0.000673106583091735 0.0654581209209995
#> V3    pgrw 0.661624194249673   -0.0278472442338177 0.0404467740791176
#> V4     pop 0.986680724014111     0.142000377827331 0.0514636442549736
#>                  stdR             unc_pmean            unc_std
#>     0.167230503443864      1.00832558392276  0.110742334576895
#> V1 0.0781287204805418    0.0882805546141323 0.0607674354710891
#> V2  0.107215420410677 -0.000476394573276497 0.0550695754101354
#> V3  0.091135707929527   -0.0184244105282735 0.0354399302399243
#> V4 0.0682640855066145     0.140109035604948 0.0536490878147803
#>              unc_stdR
#>     0.167230503443864
#> V1 0.0859561842307829
#> V2 0.0901988993098509
#> V3 0.0752919228484835
#> V4 0.0697345816191463
```

## Troubleshooting

1.  Cannot install required packages / setup renv environment

Make sure to go through the displayed errors. The problem might be
connected to your OS environment. E.g. you might see an information like
the following:

    Configuration failed to find one of freetype2 libpng libtiff-4 libjpeg. Try installing:
     * deb: libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev (Debian, Ubuntu, etc)
     * rpm: freetype-devel libpng-devel libtiff-devel libjpeg-devel (Fedora, CentOS, RHEL)
     * csw: libfreetype_dev libpng16_dev libtiff_dev libjpeg_dev (Solaris)

In such case you should first try installing the recommended packages.
With properly configured system environment everything should work fine.

## Advanced Usage: parallel computing

``` r
# To find the optimal model space with parallel computations
# replace the STEP 2 with:
library(parallel)
cl <- makeCluster(detectCores(), 'FORK')
setDefaultCluster(cl)

model_space <-
  optimal_model_space(df = data_prepared, dep_var_col = gdp,
                      timestamp_col = year, entity_col = country,
                      init_value = 0.5,
                      run_parallel = TRUE)

# and STEP 3 with:
bma_result <- bma_summary(df = data_prepared, dep_var_col = gdp,
                          timestamp_col = year, entity_col = country,
                          model_space = model_space,
                          run_parallel = TRUE)
#> [1] "Prior Mean Model Size: 2"
#> [1] "Prior Inclusion Probability: 0.5"

stopCluster(cl = NULL)
```
