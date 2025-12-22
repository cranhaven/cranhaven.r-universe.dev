
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rofanova

<!-- badges: start -->

[![R build
status](https://github.com/unina-sfere/rofanova/workflows/R-CMD-check/badge.svg)](https://github.com/unina-sfere/rofanova/actions)
<!-- badges: end -->

The package **rofanova** implements the robust nonparametric functional
ANOVA method (RoFANOVA) proposed by Centofanti et al. (2021). RoFANOVA
addresses the functional analysis of variance (FANOVA) problem, which
aims to identify the presence of significant differences, in terms of
functional mean, among groups of a functional data, by being robust
against the presence of possible outliers. It is a permutation test
whose test statistics rely on the functional equivariant M-estimator,
the functional extension of the classical robust M-estimator, which is
based on the functional normalized median absolute deviation (FuNMAD)
estimator.

The main function is `rofanova` which implements the RoFANOVA method
both for univariate and bi-variate functional data by using several
families of loss functions. The functions `fusem` and `funmad` implement
the functional equivariant M-estimator and the FuNMAD estimator,
respectively.

## Installation

You can install the development version of **rofanova** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("unina-sfere/rofanova")
```

<!-- You can install the released version of rofanova from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("rofanova") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->
<!-- ``` r -->
<!-- # install.packages("devtools") -->
<!-- devtools::install_github("unina-sfere/rofanova") -->
<!-- ``` -->

## Example

This is a basic example which shows you how to apply the main function
`rofanova` to perform both one-way and two-way FANOVA when data are
univariate functional data. The data are generated as described in the
first scenario of the simulation study in Centofanti et al. (2021).

We start by loading and attaching the **rofanova** package.

``` r
library(rofanova)
```

Then, we generate the data and, just as an example, we fix the number of
permutations *B* to 20.

``` r
data_out<-simulate_data(scenario="one-way")
label_1=data_out$label_1
X_fdata<-data_out$X_fdata
B=20
```

We compute the p-values corresponding to the RoFANOVA test with the
median, the Huber, the bisquare, the Hampel, and the optimal loss
functions.

``` r
per_list_median<-rofanova(X_fdata,label_1,B = B,family="median")
pvalue_median<-per_list_median$pval_vec
per_list_huber<-rofanova(X_fdata,label_1,B = B,family="huber")
pvalue_huber<-per_list_huber$pval_vec
per_list_bisquare<-rofanova(X_fdata,label_1,B = B,family="bisquare")
pvalue_bisquare<-per_list_bisquare$pval_vec
per_list_hampel<-rofanova(X_fdata,label_1,B = B,family="hampel")
pvalue_hampel<-per_list_hampel$pval_vec
per_list_optimal<-rofanova(X_fdata,label_1,B = B,family="optimal")
pvalue_optimal<-per_list_optimal$pval
pvalues<-c(pvalue_median,pvalue_huber,pvalue_bisquare,pvalue_hampel,pvalue_optimal)
names(pvalues)=c("median", "Huber", "bisquare", "Hampel", "optimal")
```

The p-values for the significance of the main factor are

``` r
print(pvalues)
#>   median    Huber bisquare   Hampel  optimal 
#>     0.70     0.80     0.65     0.65     0.70
```

Similarly, two-way FANOVA can be performed as follows.

``` r
data_out<-simulate_data(scenario="two-way")
label_1=data_out$label_1
label_2=data_out$label_2
X_fdata<-data_out$X_fdata
B=20
per_list_median<-rofanova(X_fdata,label_1,label_2,B = B,family="median")
pvalue_median<-per_list_median$pval_vec
per_list_huber<-rofanova(X_fdata,label_1,label_2,B = B,family="huber")
pvalue_huber<-per_list_huber$pval_vec
per_list_bisquare<-rofanova(X_fdata,label_1,label_2,B = B,family="bisquare")
pvalue_bisquare<-per_list_bisquare$pval_vec
per_list_hampel<-rofanova(X_fdata,label_1,label_2,B = B,family="hampel")
pvalue_hampel<-per_list_hampel$pval_vec
per_list_optimal<-rofanova(X_fdata,label_1,label_2,B = B,family="optimal")
pvalue_optimal<-per_list_optimal$pval
pvalues<-cbind(pvalue_median,pvalue_huber,pvalue_bisquare,pvalue_hampel,pvalue_optimal)
colnames(pvalues)=c("median", "Huber", "bisquare", "Hampel", "optimal") 
```

The p-values for the significance of the whole model, the two main
factors and the interaction are

``` r
print(pvalues)
#>     median Huber bisquare Hampel optimal
#> MOD   0.45  0.40     0.45   0.40    0.15
#> F1    0.65  0.55     0.75   0.55    0.50
#> F2    0.40  0.80     0.70   0.65    0.40
#> INT   0.40  0.30     0.15   0.30    0.15
```

# References

-   Centofanti, F., Colosimo, B.M., Grasso, M., Menafoglio, A., Palumbo,
    B., Vantini, S. (2021). Robust Functional ANOVA with Application to
    Additive Manufacturing. *arXiv preprint arXiv:2112.10643*.
