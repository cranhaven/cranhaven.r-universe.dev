
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ipf <a href="http://christophertkenny.com/ipf/"><img src="man/figures/logo.png" align="right" height="138" alt="ipf website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/christopherkenny/ipf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christopherkenny/ipf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`ipf` provides fast iterative proportional fitting (raking) for survey
weighting. The computational core is written in Rust for speed and
numerical stability. It supports multiple raking variables, automatic
variable selection, weight bounding, and comprehensive diagnostics.

## Installation

You can install the development version of ipf from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("christopherkenny/ipf")
```

## Example

Rake a survey sample to match known population targets:

``` r
library(ipf)
data(anes24)

# Define population targets as named proportions
targets <- list(
  sex = c(Male = 0.48, Female = 0.52),
  race = c(
    White = 0.59,
    Black = 0.14,
    Hispanic = 0.19,
    Asian = 0.06,
    Other = 0.02
  )
)

# Rake
result <- rake(anes24, targets)
result
#> 
#> ── Raking result (ipf)
#> Converged: Yes (3 iterations, max prop err = 8.02e-08)
#> Variables raked: "sex" and "race"
#> Missing handling: "exclude"
#> Design effect: 1.115 | Effective n: 867 / 966
#> Weight range: [0.301, 2.056] | Mean: 1 | SD: 0.339
```

Examine per-variable diagnostics with `summary()`:

``` r
summary(result)
#> 
#> ── Raking Summary (ipf)
#> ────────────────────────────────────────────────────────────────────────────────
#> ✔ Converged in 3 iterations (max prop err = 8.02e-08)
#> ℹ No base weights (uniform)
#> ℹ Selection: type = "nolim", method = "total"
#> ℹ Missing handling: "exclude"
#> ℹ Variables raked: "sex" and "race"
#> ── Weight Summary ──────────────────────────────────────────────────────────────
#> Min: 0.301 Q1: 0.8853 Median: 0.8982 Mean: 1 Q3: 1.1263 Max: 2.056
#> SD: 0.3386 CV: 0.3386
#> ── Design Effect ───────────────────────────────────────────────────────────────
#> Deff: 1.1146 | Effective n: 867 / 966
#> ── Per-Variable Assessment ─────────────────────────────────────────────────────
#> 
#> ── sex
#> # A tibble: 3 × 9
#>   level  target unweighted_n unweighted_pct weighted_n weighted_pct change_pct
#>   <chr>   <dbl>        <dbl>          <dbl>      <dbl>        <dbl>      <dbl>
#> 1 Male     0.48          458          0.477       461.        0.480    0.00341
#> 2 Female   0.52          503          0.523       500.        0.520   -0.00341
#> 3 Total    1             961          1           961.        1.000    0.00683
#> # ℹ 2 more variables: residual_disc <dbl>, original_disc <dbl>
#> 
#> ── race
#> # A tibble: 6 × 9
#>   level    target unweighted_n unweighted_pct weighted_n weighted_pct change_pct
#>   <chr>     <dbl>        <dbl>          <dbl>      <dbl>        <dbl>      <dbl>
#> 1 White      0.59          632         0.662       563.        0.590     -0.0718
#> 2 Black      0.14          118         0.124       134.        0.140      0.0164
#> 3 Hispanic   0.19          114         0.119       181.        0.190      0.0706
#> 4 Asian      0.06           28         0.0293       57.3       0.0600     0.0307
#> 5 Other      0.02           63         0.0660       19.1       0.0200    -0.0460
#> 6 Total      1             955         1           955.        1.000      0.235 
#> # ℹ 2 more variables: residual_disc <dbl>, original_disc <dbl>
```

Extract weights for downstream analysis:

``` r
augmented <- augment(result)
augmented[, c("sex", "race", ".weight")]
#> # A tibble: 966 × 3
#>    sex    race     .weight
#>    <chr>  <chr>      <dbl>
#>  1 Female White      0.885
#>  2 Male   White      0.898
#>  3 Female White      0.885
#>  4 Male   <NA>       1.01 
#>  5 Female Black      1.13 
#>  6 Male   White      0.898
#>  7 Male   Black      1.14 
#>  8 Male   Hispanic   1.60 
#>  9 Male   White      0.898
#> 10 Female White      0.885
#> # ℹ 956 more rows
```

Get a tidy one-row-per-level view:

``` r
tidy(result)
#> # A tibble: 7 × 5
#>   variable level    target weighted_pct discrepancy
#>   <chr>    <chr>     <dbl>        <dbl>       <dbl>
#> 1 sex      Male       0.48       0.480     3.85e- 8
#> 2 sex      Female     0.52       0.520    -3.85e- 8
#> 3 race     White      0.59       0.590     2.89e-15
#> 4 race     Black      0.14       0.140     5.00e-16
#> 5 race     Hispanic   0.19       0.190     5.27e-16
#> 6 race     Asian      0.06       0.0600    3.05e-16
#> 7 race     Other      0.02       0.0200    5.90e-17
```

Or a single-row model summary:

``` r
glance(result)
#> # A tibble: 1 × 7
#>   converged iterations max_prop_err  deff n_eff n_obs n_vars
#>   <lgl>          <int>        <dbl> <dbl> <dbl> <int>  <int>
#> 1 TRUE               3 0.0000000802  1.11  867.   966      2
```
