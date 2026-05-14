[![Travis-CI Build Status](https://travis-ci.org/youjin1207/logisticRR.svg?branch=master)](https://travis-ci.org/youjin1207/logisticRR)
[![DOI](https://zenodo.org/badge/144892836.svg)](https://zenodo.org/badge/latestdoi/144892836)
![![Downloads badge](http://cranlogs.r-pkg.org/badges/logisticRR)](http://cranlogs.r-pkg.org/badges/logisticRR?color=red)
 [![](http://cranlogs.r-pkg.org/badges/grand-total/logisticRR?color=yellow)](https://CRAN.R-project.org/package=logisticRR)
![![version badge](http://www.r-pkg.org/badges/version-last-release/logisticRR)](http://www.r-pkg.org/badges/version-last-release/logisticRR?color=orange)	


# Overview

## Package information

- Version: 0.3.0
- Maintainer : Youjin Lee (<youjin.lee@pennmedicine.upenn.edu>)
- Imports : stats, nnet

## Installation

You can download the package by:

```
install.packages("logisticRR")
library(logisticRR)
```
or you can directly download the development version from author's Github
```
install.packages("devtools")
library(devtools)
install_github("youjin1207/logisticRR")
```


## Usage

[Here](https://github.com/youjin1207/logisticRR/blob/master/vignettes/logisticRR.Rmd) is a R vignettes for guidance. Or you can access to vignettes via:

```
install_github("youjin1207/logisticRR", build_vignettes = TRUE)
library(logisticRR)
vignette("logisticRR", package = "logisticRR")
```

## Example

### generate hypothetical data

```
n <- 500
set.seed(1234)
X <- rbinom(n, 1, 0.3)
W <- rbinom(n, 1, 0.3); W[sample(1:n, n/3)] = 2
Z <- rep(0, n)
Z[sample(1:n, n/2)] <- "female"; Z <- ifelse(Z == 0, "male", Z)
dummyZ <- ifelse(Z == "female", 1, 0)
Y <- rbinom(n, 1, plogis(X - W + 2*dummyZ))
dat <- as.data.frame(cbind(Y, X, W, Z))
dat$X <- as.numeric(dat$X); dat$X <- ifelse(dat$X == 2, 1, 0)
dat$Y <- as.numeric(dat$Y); dat$Y <- ifelse(dat$Y == 2, 1, 0)
dat$W <- as.factor(dat$W)
dat$Z <- as.factor(dat$Z)
```

```
simresult <- logisticRR(Y ~ X + W + Z, data = dat, boot = TRUE, n.boot = 200)
var(simresult$boot.rr)
simresult$delta.var

simresult$RR
```


```
nominalresult <- logisticRR(Y ~ W + X + Z, data = dat, boot = TRUE, n.boot = 200)
var(nominalresult$boot.rr)
nominalresult$delta.var

nominalresult$RR
```

### multivariate logistic regression

When reponse variable takes more than two values, multinomial logistic regression is widely used to reveal association between the response variable and exposure variable. In that case, relative risk of each category compared to the reference category can be considered, conditional on other fixed covariates. Other than (adjusted) relative risk, relative risks ratio (RRR) is often of interest in multinomial logistic regression.


```
dat$multiY <- ifelse(dat$X == 1, rbinom(n, 1, 0.8) + dat$Y, rbinom(n, 1, 0.2) + dat$Y)
multiresult <- multiRR(multiY ~ X + W + Z, data = dat, boot = TRUE, n.boot = 1000)
apply(multiresult$boot.rr, 2, sd)
sqrt(multiresult$delta.var)

multiresult$RRR
multiresult$RR
```

Similar to the binary reponse, in multinomial logistic regression model, categorical exposure variable can be introduced; in this case, baseline value and comparative value of exposure variable should be specified. 

```
multinresult <- multinRR(multiY ~ W + X + Z, data = dat, basecov = 0, comparecov = 1, boot = TRUE, n.boot = 1000)
apply(multinresult$boot.rr, 2, sd)
sqrt(multinresult$delta.var)

multinresult$RRR
multinresult$RR
```
