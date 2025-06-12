# SurrogateRsq 0.2.1

## Minor improvements and fixes 

* The `data` argument is removed from all three main functions `surr_rsq()`, `surr_rsq_ci()`, and `surr_rsq_rank()`. The dataset will be directly pulled our from the `full_model` object. 

* The bug related to the bootstrapped data and the model updating steps in `surr_rsq_ci()` have been fixed (#2, #3). 

* `rsq()` function is removed since it is no needed. 

# SurrogateRsq 0.2.0.9000

# SurrogateRsq 0.2.0

# Changes in version 0.1.1

## Major features

1. The package supports the ordinal logistic regression models.

## Features needed

1. It has "print()" for surr_rsq, but the print() or summary() functions are needed for surr_rsq_ci and surr_rsq_rank.

2. ...

# SurrogateRsq 0.1.0

## Major features

1. This package has four main functions: rsq, surr_rsq, surr_rsq_ci, and surr_rsq_rank.

2. surr_rsq() is a function for producing a point estimate of the surrogate R-squared for a user-specified model.

2. surr_rsq_ci() is a function for generating an interval measure of the surrogate R-squared with the designated confidence level.

2. surr_rsq_rank() is a function to produce a ranking of explanatory variables based on
their contributions to the overall surrogate R-squared.

2. rsq() is a function to produce other pseudo R-squared measures in the literature.


