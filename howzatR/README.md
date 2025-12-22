
<!-- README.md is generated from README.Rmd. Please edit that file -->

# howzatR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/howzatR)](https://CRAN.R-project.org/package=howzatR)
<!-- badges: end -->

The goal of howzatR is to provide useful functions for cricket analysis
& exploratory.

## Installation

You can install a stable version of howzatR using R/Rstudio with:

``` r
install.packages("howzatR")
```

You can install the development version of howzatR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lukelockley/howzatR")
```

## Example - Batters Analysis

This is a basic example how to use the batting functionality:

``` r
library(howzatR)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## Basic Batting dataset
bat_raw_df
#>     Player Inns NO Runs_Scored Balls_Faced
#> 1 A. Green    7  1         140         220
#> 2 B. Brown    8  3         156         100
#> 3  C. Blue    6  0         111          76


## Analysis
bat_df <- bat_raw_df %>%
  mutate(
    Outs = Inns - NO,
    Average = bat_avg(runs_scored = Runs_Scored, no_dismissals = Outs),
    Strike_Rate = bat_sr(runs_scored = Runs_Scored, balls_faced = Balls_Faced)
  )

## Results
bat_df
#>     Player Inns NO Runs_Scored Balls_Faced Outs  Average Strike_Rate
#> 1 A. Green    7  1         140         220    6 23.33333    63.63636
#> 2 B. Brown    8  3         156         100    5 31.20000   156.00000
#> 3  C. Blue    6  0         111          76    6 18.50000   146.05263
```

## Example - Bowling Analysis

This is a basic example how to use the bowling functionality

``` r
library(howzatR)
library(dplyr)

## Basic Bowling dataset
bowl_raw_df
#>     Player Balls_Bowled Runs_Conceded Wickets
#> 1 E. Apple          560           235      15
#> 2  F. Pear          754           567      21
#> 3 G. Grape          234           270       7


## Analysis
bowl_df <- bowl_raw_df %>%
  mutate(
    Economy_overs = bowl_econ(balls_bowled = Balls_Bowled, runs_conceded = Runs_Conceded, type = "overs"),
    Economy_sets = bowl_econ(balls_bowled = Balls_Bowled, runs_conceded = Runs_Conceded, type = "sets"),
    Economy_hundred = bowl_econ(balls_bowled = Balls_Bowled, runs_conceded = Runs_Conceded, type = "per_100"),
    Average = bowl_avg(runs_conceded = Runs_Conceded, wickets_taken = Wickets),
    Strike_Rate = bowl_sr(balls_bowled = Balls_Bowled, wickets_taken = Wickets),
    Overs = balls_to_overs(balls = Balls_Bowled)
  ) %>%
  select(
    Player, Balls_Bowled, Overs, Runs_Conceded,
    Wickets, Economy_overs, Economy_sets, Economy_hundred,
    Average, Strike_Rate
  )

## Results
bowl_df
#>     Player Balls_Bowled Overs Runs_Conceded Wickets Economy_overs Economy_sets
#> 1 E. Apple          560  93.2           235      15      2.517857     2.098214
#> 2  F. Pear          754 125.4           567      21      4.511936     3.759947
#> 3 G. Grape          234  39.0           270       7      6.923077     5.769231
#>   Economy_hundred  Average Strike_Rate
#> 1        41.96429 15.66667    37.33333
#> 2        75.19894 27.00000    35.90476
#> 3       115.38462 38.57143    33.42857
```
