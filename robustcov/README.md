![](https://github.com/YunyiShen/RobustOmega/workflows/R-CMD-check/badge.svg)

# robustcov
Robust covariance and precision matrix estimators. Based on the review of P.-L. Loh and X. L. Tan. (2018)


To install:

```r
devtools::install_github("YunyiShen/robustcov")
```

There are in total 4 robust covariance and 3 correlation estimation implemented, namely:

- `corSpearman`: Spearman correlation
- `corKendall`: Kendall's tau
- `corQuadrant`: Quadrant correlation coefficients
- `covGKmat`: Gnanadesikan-Kettenring estimator by Tarr et al. (2015) and Oellerer and Croux (2015)
- `covSpearmanU`: SpearmanU covariance estimator by P.-L. Loh and X. L. Tan. (2018), The pairwise covariance matrix estimator proposed in Oellerer
and Croux (2015), where the MAD estimator is combined with Spearman’s
rho
- `covOGK`: Orthogonalized Gnanadesikan-Kettenring (OGK) estimator by Maronna, R. A. and Zamar, R. H. (2002)
- `covNPD`: Nearest Positive (semi)-Definite projection of the pairwise covariance matrix estimator considered in Tarr et al. (2015). 

P.-L. Loh and X. L. Tan. (2018) then used these robust estimates in Graphical Lasso (package `glasso`) or Quadratic Approximation (package `QUIC`) to obtain sparse solutions to precision matrix

With `glasso`, a function `robglasso` stand for robust graphical LASSO is implemented. It has build in cross validation described in P.-L. Loh and X. L. Tan. (2018), for instance, to use the method with cross validation:

```r
robglasso(data=matrix(rnorm(100),20,5), covest = cov,CV=TRUE)
```

Where `data` should be a matrix and `covest` should be a function that estimate the covariance e.g. anyone mentioned above. The result list contains everything from `glasso` output with the optimal tuning parameter found by cross validation. One can also decide fold by setting `fold` in `robglasso`. For more details see `?robglasso`.  
