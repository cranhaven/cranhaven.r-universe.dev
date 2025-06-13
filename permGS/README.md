# permGS
permGS is an R package implementing permutational group-sequential tests for time-to-event data based on
the two-sample (weighted) log-rank test statistic. It supports exact permutation test when the censoring distributions 
are equal in the treatment and the control group and approximate imputation-permutation methods when the 
censoring distributions are different. One- and two-sided testing is possible.

## Installation

Get the released version from CRAN:

```R
install.packages("permGS")
```

Or the development version from github:

```R
# install.packages("devtools")
devtools::install_github("mbrueckner/permGS")
```
