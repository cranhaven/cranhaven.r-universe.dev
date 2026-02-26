## `survivalMPLdc`: Penalised Likelihood for Survival Analysis with Dependent Censoring

## survivalMPLdc
The `survivalMPLdc` package provides the estimating and plotting function for the Cox hazard models under dependent right censoring using maximum penalised likelihood method [Xu et al (2018)](https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.7651).

The `survivalMPLdc` currently only supports proportional hazard models. The future work can extend the proposed method to other semi-parametric hazard model, i.e. the accelerate failure or additive hazard models. Other type of censoring, i.e. interval censoring and even time-dependent covariates can be considered. 

## Installation

### Stable release on CRAN

The `survivalMPLdc` package can be installed from `CRAN`.

```s
install.packages("survivalMPLdc")
library("survivalMPLdc")
```

### Development version on Github

You can use the devtools package to install the development version of `survivalMPLdc` from GitHub:
```s
# install.packages("devtools")
devtools::install_github("Kenny-Jing-Xu/survivalMPLdc")
library(survivalMPLdc)
```

## Usage
A reference manual is available at [kenny-jing-xu.github.io/survivalMPLdc](https://kenny-jing-xu.github.io/survivalMPLdc/).

## Citation
Xu, J., Ma, J. and Fung, T. (2020). survivalMPLdc: Survival Analysis under Dependent Right Censoring using Maximum Penalised Likelihood. R package version 0.1.1.
