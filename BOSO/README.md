# Introduction to BOSO


We present *BOSO*, an R package to perform feature selection in a linear regression problem. It implements a Bilevel Optimization Selector Operator.

## Installation
BOSO can be installed from CRAN repository:

`install.packages("BOSO")`


## Introduction

The package package has been prepared to work like 'glmnet' and 'lasso', presented 
in the *BestSubset* package.

``` r
library(BOSO)

## Load the data prepared for this test
data("sim.xy", package = "BOSO")

Xtr <- sim.xy[['high-5']]$x
Ytr <- sim.xy[['high-5']]$y
Xval <- sim.xy[['high-5']]$xval
Yval <- sim.xy[['high-5']]$yval


## Perform BOSO
time <- Sys.time()
obj <- BOSO(x = Xtr, y = Ytr,
            xval = Xval, yval = Yval,
            IC = 'eBIC',
            nlambda=100,
            intercept= 0,
            standardize = 0,
            Threads=4, timeLimit = 60, verbose = 3, 
            seed = 2021)
time <- as.numeric(Sys.time() - time)

```

`obj` is a BOSO object, which have the following associated functions: 

  - `coef(obj)` returns the coefficients (betas) of the linear regression.  
  - `predict(obj, xnew)` returns the predicted outcome with a new X matrix.


``` r
betas <- coef(obj)
print(betas[betas!=0])

Ytr_predicted <- predict(obj, Xtr)
print(paste0("MSE for training set is ",  round(mean((Ytr_predicted-Ytr)^2),5)))

Yval_predicted <- predict(obj, Xval)
print(paste0("MSE for validation set is ", round(mean((Yval_predicted-Yval)^2),5)))
```








