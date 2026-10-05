
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ISCA <img src="man/figures/logo.png" align="right" height="120" alt="" />

## Overview

The Inductive Subgroup Comparison Approach (‘ISCA’) offers a way to
compare groups that are internally differentiated and heterogeneous. It
starts by identifying the social structure of a reference group against
which a minority or another group is to be compared, yielding empirical
subgroups to which minority members are then matched based on how
similar they are. The modelling of specific outcomes then occurs within
specific subgroups in which majority and minority members are matched.
ISCA is characterized by its data-driven, probabilistic, and iterative
approach and combines fuzzy clustering, Monte Carlo simulation, and
regression Analysis. ISCA_random_assignments() assigns subjects
probabilistically to subgroups. ISCA_clustertable() provides summary
statistics of each cluster across iterations. ISCA_modeling provides OLS
regression results for each cluster across iterations.

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the package from this [GitHub
repository](https://github.com/ldrouhot/ISCA). Be sure to first install
the [remotes](https://CRAN.R-project.org/package=remotes) package.

``` r
install.packages("remotes")
```

Then install ISCA using the `install_github` function in the
[remotes](https://CRAN.R-project.org/package=remotes) package.

``` r
remotes::install_github("ldrouhot/ISCA")
```

## Examples

The functions are demonstrated using a fictitious dataset containing
1,000 observations, which is included in the package. The first function
`ISCA_random_assignments()` produces a dataset that has one new column
for each probabilistic draw/iteration. The values indicate the
respective cluster assignments. The output is the foundation of the
other two functions in the ISCA-package.

``` r
library(ISCA)
data(sim_data)

ISCA_step1 <- ISCA::ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), fuzzifier = 1.5, n_clusters=4, draws=5, cluster_vars= c("female", "age", "education", "income"))
```

``` r
head(ISCA_step1)
#>   female age education income religiosity discrimination native A1 A2 A3 A4 A5
#> 1      1  74         6   1608           2              2      0  1  1  1  1  1
#> 2      1  69         9   1227          10              4      0  4  4  4  4  4
#> 3      0  21         6      0           2              5      0  3  3  3  3  3
#> 4      1  64         6   2132          10              3      0  2  2  2  2  2
#> 5      1  20         7   1487           2              3      0  1  4  1  1  1
#> 6      1  25         5   1761           9              3      0  1  1  1  1  1
```

The second function `result_ISCA_clustertable()` gives an overview of
the distribution of the variables of interest per cluster and across
iterations.

``` r
result_ISCA_clustertable <- ISCA::ISCA_clustertable(data = ISCA_step1, cluster_vars = c("native", "education", "age", "female", "discrimination", "religiosity"), draws = 5)
print(result_ISCA_clustertable)
#>                        variable        1        2        3        4
#> 1                    assignment   1.0000   2.0000   3.0000   4.0000
#> 2             grand_mean_native   0.5014   0.4214   0.4684   0.4576
#> 3          cluster_error_native   0.0175   0.0122   0.0213   0.0103
#> 4          grand_mean_education   5.0306   5.0152   5.0688   5.0278
#> 5       cluster_error_education   0.0443   0.0426   0.0410   0.0448
#> 6            grand_sd_education   1.3002   1.3807   1.2700   1.3782
#> 7                grand_mean_age  47.4305  46.2335  47.5678  49.5852
#> 8             cluster_error_age   0.3652   0.5450   0.3787   0.2581
#> 9                  grand_sd_age  18.9128  18.2450  17.5107  17.9565
#> 10            grand_mean_female   0.4802   0.4777   0.4976   0.4795
#> 11         cluster_error_female   0.0072   0.0112   0.0033   0.0091
#> 12    grand_mean_discrimination   3.9587   4.0172   3.9800   3.9644
#> 13 cluster_error_discrimination   0.0401   0.0392   0.0141   0.0231
#> 14      grand_sd_discrimination   1.1245   1.2439   1.2637   1.2664
#> 15       grand_mean_religiosity   4.8027   4.8783   4.7865   4.8623
#> 16    cluster_error_religiosity   0.0914   0.0761   0.0860   0.0705
#> 17         grand_sd_religiosity   2.9023   2.8570   2.8655   2.8037
#> 18             grand_mean_count 232.4000 188.4000 252.8000 326.4000
#> 19          cluster_error_count   8.0808   3.4351  13.6638  10.5024
```

The third function `ISCA_clustertable()` runs OLS regressions for each
cluster across iterations. The output is a list storing the regression
estimates and adjusted R Squared values.

``` r
ISCA_modeling_res <- ISCA::ISCA_modeling(data= ISCA_step1, model_spec="religiosity ~ native + female + age + education + discrimination", draws = 5, n_clusters = 4)
#> `mutate_if()` ignored the following grouping variables:
#> • Column `cluster`
print(ISCA_modeling_res[1])
#> [[1]]
#> # A tibble: 30 × 5
#> # Groups:   cluster [5]
#>    cluster term           mean_coefficients mean_std.error mean_p_value
#>    <fct>   <chr>                      <dbl>          <dbl>        <dbl>
#>  1 1       (Intercept)               4.09           0.482        0.0012
#>  2 1       age                       0.0007         0.0036       0.784 
#>  3 1       discrimination            0.206          0.0873       0.267 
#>  4 1       education                 0.0283         0.047        0.765 
#>  5 1       female                    0.623          0.191        0.130 
#>  6 1       native                   -1.16           0.182        0.0048
#>  7 2       (Intercept)               4.23           0.537        0.0009
#>  8 2       age                      -0.0067         0.0066       0.504 
#>  9 2       discrimination            0.190          0.0649       0.285 
#> 10 2       education                 0.131          0.0692       0.417 
#> # ℹ 20 more rows
print(ISCA_modeling_res[2])
#> [[1]]
#>   mean.fit.cluster1 mean.fit.cluster2 mean.fit.cluster3 mean.fit.cluster4
#> 1        0.03939277        0.03813129        0.05497081        0.09119341
#>   pooled.model
#> 1   0.06104137
```

## Vignettes

Check out the ISCA vignette for more in-depth explanations.

``` r
# browseVignettes("ISCA")
```

## Citation

Please cite the package as follows:

> Drouhot, Lucas G., and Marion Späth. 2024. ISCA: Compare Heterogeneous
> Social Groups. R package version 0.1.0.
