
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Colossus

The goal of Colossus is to provide an open-source means of performing
survival analysis on big data with complex risk formula. Colossus is
designed to perform Cox Proportional Hazard regressions and Poisson
regressions on datasets loaded as data.tables or data.frames. The risk
models allowed are sums or products of linear, log-linear, or several
other radiation dose response formula highlighted in the vignettes.
Additional plotting capabilities are available.

By default a fully portable version of the code is compiled, which does
not support OpenMP on every system. Please consult the GitHub for
details on libraries required for your OS if you are interested in using
OpenMP on linux. Note that Colossus requires OpenMP support to perform
parallel calculations. During the configuration stage of installation,
text starting with “CONFIG NOTE” will denote what OS/Compiler are
detected and if OpenMP support is configured. Currently OpenMP support
is not configured for linux compiling with clang and MacOS systems.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(data.table)
library(parallel)
library(Colossus)
## basic example code reproduced from the starting-description vignette

df <- data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"

names <- c('a','b','c','d')
term_n <- c(0,1,1,2)
tform <- c("loglin","lin","lin","plin")
modelform <- "M"
fir <- 0

a_n <- c(0.1, 0.1, 0.1, 0.1)

keep_constant <- c(0,0,0,0)
der_iden <- 0

control=list('lr' = 0.75,'maxiter' = 100,'halfmax' = 5,'epsilon' = 1e-9,
             'dbeta_max' = 0.5,'deriv_epsilon' = 1e-9, 'abs_max'=1.0,
             'change_all'=TRUE,'dose_abs_max'=100.0,'verbose'=FALSE,
             'ties'='breslow','double_step'=1)

e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
print(e)
#> $LogLik
#> [1] -0.6753644
#> 
#> $First_Der
#> [1]  0.000000e+00 -7.187040e-05  7.361232e-05  1.919948e-04
#> 
#> $Second_Der
#>              [,1]         [,2]          [,3]          [,4]
#> [1,] 0.000000e+00 0.000000e+00  0.000000e+00  4.965508e-19
#> [2,] 0.000000e+00 1.742209e-08  7.238366e-07  2.311365e-07
#> [3,] 0.000000e+00 7.238366e-07 -1.501037e-06 -2.356033e-07
#> [4,] 4.965508e-19 2.311365e-07 -2.356033e-07 -3.687577e-06
#> 
#> $beta_0
#> [1]  41.26157  98.72266  96.82311 101.10000
#> 
#> $Standard_Deviation
#> [1]      NaN      NaN 177.9643   0.0000
#> 
#> $AIC
#> [1] 9.350729
#> 
#> $BIC
#> [1] 8.517767
#> 
#> $Parameter_Lists
#> $Parameter_Lists$term_n
#> [1] 0 1 1 2
#> 
#> $Parameter_Lists$tforms
#> [1] "loglin" "lin"    "lin"    "plin"  
#> 
#> $Parameter_Lists$names
#> [1] "a" "b" "c" "d"
#> 
#> 
#> $Control_List
#> $Control_List$Iteration
#> [1] 100
#> 
#> $Control_List$`Maximum Step`
#> [1] 1
#> 
#> $Control_List$`Derivative Limiting`
#> [1] 0.0001919948
#> 
#> 
#> $Converged
#> [1] FALSE
```
