# MTAFT: Data-driven Estimation for Multi-Threshold Accelerated Failure Time Model

This repository contains the code for the article titled "Data-driven Estimation for Multi-Threshold Accelerated Failure Time Model" (MTAFT). The MTAFT model is a statistical method used for estimating failure times in the presence of multiple thresholds. The code provided here implements the MTAFT model and its associated functions.

## Functions

The repository includes the following R functions:

### `MTAFT_CV`

This function implements the cross-validation (CV) method proposed in the MTAFT article. It combines the Wild Binary Segmentation (WBS) or Dynamic Programming (DP) algorithm to determine the optimal number of thresholds.

### `MTAFT_IC`

This function implements the information criterion (IC) method proposed in the MTAFT article. It also combines the WBS or DP algorithm to determine the optimal number of thresholds.

### `TSMCP`

This function implements Li and Jin's (2018) method, which transforms the multi-threshold estimation problem into a variable selection problem.

### `MTAFT_simdata`

This function provides a simple data generation process for simulating data to evaluate the performance of the MTAFT model. It generates a dataset consisting of the response variable (Y), deletion indicator function (delta), threshold variable (Tq), and covariate matrix (X).

### `MTAFT_test`

This function implements a hypothesis test for the existence of thresholds using a score-type statistic. It is applicable to scenarios with multiple thresholds.

## Usage

### Install

``` r
devtools::install_github("zenghao-stat/MTAFT")
library(MTAFT)
```

Here are examples of how to use the provided R functions:

### Generating simulated data

``` r
n <- 500
err <- "normal" # or "t3"
dataset <- MTAFT_simdata(n, err)
Y <- dataset[, 1]
delta <- dataset[, 2]
Tq <- dataset[, 3]
X <- dataset[, -c(1:3)]
```

### Testing threshold effects

``` r
nboots <- 500
pval <- MTAFT_test(Y, X, Tq, delta, nboots)
```

### Detecting the number and positions of breakpoints

``` r
library(grpreg)

n1 <- sum(delta)
c <- seq(0.5, 1.5, 0.1)
m <- ceiling(c * sqrt(n1))
bicy <- rep(NA, length(c))
tsmc <- NULL
p <- ncol(X)
for (i in 1:length(c)) {
  tsm <- try(TSMCP(Y, X, delta, c[i], penalty = "scad"), silent = TRUE)
  if (is(tsm, "try-error")) next()
  bicy[i] <- log(n) * ((length(tsm[[1]]) + 1) * (p + 1)) + n * log(tsm[[3]])
  tsmc[[i]] <- tsm
}

if (any(!is.na(bicy))) {
  tsmcp <- tsmc[[which(bicy == min(bicy))[1]]]
  thre.LJ <- Tq[tsmcp[[1]]]
  thre.num.Lj <- length(thre.LJ)
}
```

### Applying the information criterion method

``` r
c0 <- 0.299
delta0 <- 2.01
algorithm <- "WBS"
dist_min <- 50
ncps_max <- 4

result <- MTAFT_IC(Y, X, delta, Tq, c0, delta0, algorithm, dist_min, ncps_max)
```

### Applying the cross-validation method

``` r
algorithm <- "WBS"
dist_min <- 50
ncps_max <- 4

result <- MTAFT_CV(Y, X, delta, Tq, algorithm, dist_min, ncps_max)
```

## Reference

> Wan, C., Zeng, H., Zhong, W., et al. (2023) Data-driven estimation for multi-threshold accelerated failure time model. Working paper.
