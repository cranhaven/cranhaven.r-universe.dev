
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TensorClustering

<!-- badges: start -->

<!-- badges: end -->

Performs model-based tensor clustering methods including the Tensor
Envelope Mixture Model (**TEMM**), Doubly-Enhanced EM (**DEEM**)
algorithm and Tensor Gaussian Mixture Model (**TGMM**). TEMM is proposed
in the paper “Tensor Envelope Mixture Model For Simultaneous Clustering
and Multiway Dimension Reduction” published in Biometrics by Deng and
Zhang (2021). DEEM is proposed in the paper “A Doubly-Enhanced EM
Algorithm for Model-Based Tensor Clustering” published in the Journal of
the American Statistical Association by Mai, Zhang, Pan and Deng (2021).

## Installation

The **TensorClustering** package can be installed, directly from
**CRAN**:

``` r
install.packages("TensorClustering")
library(TensorClustering)
```

It can also be installed from **GitHub**, using the **devtools**
library:

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("azuryee/TensorClustering")
library(TensorClustering)
```
