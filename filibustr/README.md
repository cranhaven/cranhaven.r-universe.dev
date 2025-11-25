
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filibustr

<!-- badges: start -->

[![R-CMD-check](https://github.com/feinleib/filibustr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/feinleib/filibustr/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/feinleib/filibustr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/feinleib/filibustr/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/feinleib/filibustr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/feinleib/filibustr?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/filibustr)](https://CRAN.R-project.org/package=filibustr)
<!-- badges: end -->

The [`filibustr`](https://feinleib.github.io/filibustr/) package
provides data utilities for research on the U.S. Congress. This package
provides a uniform interface for accessing data from sources such as
Voteview, the Legislative Effectiveness Scores, and more. Accessing your
data using these functions removes many of the manual steps involved
with importing data. This has two primary benefits:

- **Speeding up your workflow** and enabling you to quickly experiment
  with a variety of data choices.
- Ensuring you always have the **most up-to-date data**.

`filibustr` is inspired by the
[`baseballr`](https://github.com/BillPetti/baseballr) package, which
provides similar conveniences for baseball analytics data.

## Installation

You can install the stable version of filibustr from CRAN with:

``` r
install.packages("filibustr")
```

You can install the development version of filibustr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("feinleib/filibustr")
```

## Functions

### Voteview

There are four functions that retrieve data from
[Voteview](https://voteview.com/data):

- `get_voteview_members()`: data on members (Presidents, Senators, and
  Representatives).
- `get_voteview_parties()`: data on parties (size and ideology)
- `get_voteview_rollcall_votes()`: results of recorded votes (overall
  results, not positions of individual members)
- `get_voteview_member_votes()`: individual members’ votes on recorded
  votes

These functions share a common interface and arguments.

For demonstration, here is the table returned by
`get_voteview_parties()`.

``` r
library(filibustr)

get_voteview_parties()
#> # A tibble: 845 × 9
#>    congress chamber   party_code party_name       n_members nominate_dim1_median
#>       <int> <fct>          <int> <fct>                <int>                <dbl>
#>  1        1 President       5000 Pro-Administrat…         1               NA    
#>  2        1 House           4000 Anti-Administra…        29                0.018
#>  3        1 House           5000 Pro-Administrat…        31                0.576
#>  4        1 Senate          4000 Anti-Administra…         9               -0.238
#>  5        1 Senate          5000 Pro-Administrat…        20                0.427
#>  6        2 President       5000 Pro-Administrat…         1               NA    
#>  7        2 House           4000 Anti-Administra…        32               -0.022
#>  8        2 House           5000 Pro-Administrat…        40                0.533
#>  9        2 Senate          4000 Anti-Administra…        14               -0.392
#> 10        2 Senate          5000 Pro-Administrat…        17                0.446
#> # ℹ 835 more rows
#> # ℹ 3 more variables: nominate_dim2_median <dbl>, nominate_dim1_mean <dbl>,
#> #   nominate_dim2_mean <dbl>
```

**Note:** Especially when working with large datasets, reading data from
Voteview can take a long time. Here are two strategies to speed up your
data import:

- If you are repeatedly loading the same static dataset (i.e., not
  including information from the current Congress), it may be useful to
  download the dataset as a CSV/DTA file so you can read that local file
  using `local_path` instead of having to download data from online.
- You may use `mirai` to download Voteview data in parallel. See
  `vignette("parallel-downloads", package = "filibustr")` for more info
  on parallel data downloads.

### Legislative Effectiveness Scores

The function `get_les()` retrieves Legislative Effectiveness Scores data
from the [Center for Effective Lawmaking](https://thelawmakers.org).

Here is an example table returned by `get_les()`.

``` r
get_les(chamber = "senate")
#> # A tibble: 2,635 × 88
#>    last     first state congress cgnum icpsr  year dem   majority elected female
#>    <chr>    <chr> <fct>    <int> <int> <int> <int> <lgl> <lgl>      <int> <lgl> 
#>  1 Abourezk James SD          93     1 13000  1972 TRUE  TRUE        1972 FALSE 
#>  2 Allen    James AL          93     3 12100  1972 TRUE  TRUE        1968 FALSE 
#>  3 Bayh     Birch IN          93     6 10800  1972 TRUE  TRUE        1962 FALSE 
#>  4 Bentsen  Lloyd TX          93    10   660  1972 TRUE  TRUE        1970 FALSE 
#>  5 Bible    Alan  NV          93    11   688  1972 TRUE  TRUE        1954 FALSE 
#>  6 Biden    Jose… DE          93    12 14101  1972 TRUE  TRUE        1972 FALSE 
#>  7 Burdick  Quen… ND          93    16  1252  1972 TRUE  TRUE        1960 FALSE 
#>  8 Byrd     Robe… WV          93    18  1366  1972 TRUE  TRUE        1958 FALSE 
#>  9 Cannon   Howa… NV          93    19  1482  1972 TRUE  TRUE        1958 FALSE 
#> 10 Chiles   Lawt… FL          93    21 13101  1972 TRUE  TRUE        1970 FALSE 
#> # ℹ 2,625 more rows
#> # ℹ 77 more variables: afam <lgl>, latino <lgl>, votepct <dbl>, chair <lgl>,
#> #   subchr <lgl>, seniority <int>, state_leg <lgl>, state_leg_prof <dbl>,
#> #   maj_leader <lgl>, min_leader <lgl>, votepct_sq <dbl>, power <lgl>,
#> #   freshman <lgl>, sensq <int>, deleg_size <int>, party_code <int>,
#> #   bioname <chr>, bioguide_id <chr>, born <int>, died <int>, dwnom1 <dbl>,
#> #   dwnom2 <dbl>, meddist <dbl>, majdist <dbl>, cbill1 <int>, caic1 <int>, …
```

There are non-trivial differences between the House and Senate datasets,
so take care when joining House and Senate data.

### Harbridge-Yong, Volden, and Wiseman (2023)

The function `get_hvw_data()` retrives replication data for
[Harbridge-Yong, Volden, and Wiseman
(2023)](https://doi.org/10.1086/723805).

Here are the tables returned by `get_hvw_data()`:

``` r
get_hvw_data("house")
#> # A tibble: 9,825 × 109
#>    thomas_num thomas_name     icpsr congress  year st_name    cd dem   elected
#>         <int> <chr>           <int>    <int> <int> <fct>   <int> <lgl>   <int>
#>  1          1 Abdnor, James   14000       93  1973 SD          2 FALSE    1972
#>  2          2 Abzug, Bella    13001       93  1973 NY         20 TRUE     1970
#>  3          3 Adams, Brock    10700       93  1973 WA          7 TRUE     1964
#>  4          4 Addabbo, Joseph 10500       93  1973 NY          7 TRUE     1960
#>  5          5 Albert, Carl       NA       93  1973 OK          3 NA       1946
#>  6          6 Alexander, Bill 12000       93  1973 AR          1 TRUE     1968
#>  7          7 Anderson, John  10501       93  1973 IL         16 FALSE    1960
#>  8          8 Anderson, Glenn 12001       93  1973 CA         35 TRUE     1968
#>  9          9 Andrews, Ike    14001       93  1973 NC          4 TRUE     1972
#> 10         10 Andrews, Mark   10569       93  1973 ND          1 FALSE    1963
#> # ℹ 9,815 more rows
#> # ℹ 100 more variables: female <lgl>, votepct <dbl>, dwnom1 <dbl>,
#> #   deleg_size <int>, speaker <lgl>, subchr <lgl>, ss_bills <int>,
#> #   ss_aic <int>, ss_abc <int>, ss_pass <int>, ss_law <int>, s_bills <int>,
#> #   s_aic <int>, s_abc <int>, s_pass <int>, s_law <int>, c_bills <int>,
#> #   c_aic <int>, c_abc <int>, c_pass <int>, c_law <int>, afam <lgl>,
#> #   latino <lgl>, power <lgl>, budget <lgl>, chair <lgl>, state_leg <lgl>, …
get_hvw_data("senate")
#> # A tibble: 2,228 × 104
#>    last  first state  cabc  caic cbill  claw cpass  sabc  saic sbill  slaw spass
#>    <chr> <chr> <fct> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
#>  1 Grav… Mike  AK        0     0    17     0     0     2     0    48     0     1
#>  2 Stev… Ted   AK        0     0     9     0     0     6     0    71     3     6
#>  3 Allen James AL        0     0     5     0     0     2     0    14     0     1
#>  4 Spar… John  AL        1     0    23     0     1     7     0    62     0     7
#>  5 Fulb… James AR        0     0     0     0     0     9     0    31     3     8
#>  6 McCl… John  AR        0     0     3     0     0     3     0    20     1     2
#>  7 Fann… Paul  AZ        0     0     4     0     0     1     0    32     1     1
#>  8 Gold… Barry AZ        0     0     6     0     0     0     0    13     0     0
#>  9 Cran… Alan  CA        7     0    17     1     7     5     0    64     2     4
#> 10 Tunn… John  CA        0     0     1     0     0     4     0    35     0     1
#> # ℹ 2,218 more rows
#> # ℹ 91 more variables: ssabc <int>, ssaic <int>, ssbill <int>, sslaw <int>,
#> #   sspass <int>, congress <int>, cgnum <int>, icpsr <int>, year <int>,
#> #   dem <lgl>, majority <lgl>, elected <int>, female <lgl>, afam <lgl>,
#> #   latino <lgl>, votepct <dbl>, dwnom1 <dbl>, chair <lgl>, subchr <lgl>,
#> #   seniority <int>, state_leg <lgl>, state_leg_prof <dbl>, maj_leader <lgl>,
#> #   min_leader <lgl>, allbill <int>, allaic <int>, allabc <int>, …
```

The House and Senate data do not have the same number of variables, or
the same variable names, so it is not trivial to join the two tables.

### Senate.gov

The following functions retrieve data tables from
[Senate.gov](https://www.senate.gov).

- `get_senate_sessions()`: The start and end dates of each legislative
  session of the Senate. ([table
  link](https://www.senate.gov/legislative/DatesofSessionsofCongress.htm))
- `get_senate_cloture_votes()`: Senate actions on cloture motions and
  cloture votes. ([table
  link](https://www.senate.gov/legislative/cloture/clotureCounts.htm))

These functions take no arguments, and they always return the full data
table from the Senate website.

### Small utilities

This package also provides some smaller utility functions for working
with congressional data.

- 3 functions dealing with years and Congress numbers:
  - `year_of_congress()` returns the starting year for a given Congress.
  - `congress_in_year()` returns the Congress number for a given year.
  - `current_congress()` returns the number of the current Congress,
    which is currently 119. `current_congress()` is equivalent to
    `congress_in_year(Sys.Date())`.
- `get_voteview_cast_codes()` returns a key to the `cast_code` column in
  `get_voteview_member_votes()`.
- `read_html_table()` is a general-use function for reading HTML tables
  from online. It’s a nice shortcut for a common `rvest` workflow that
  otherwise takes 3 functions. (It’s what powers the Senate.gov
  functions!)

## Feedback and contributions

If you notice any bugs, or have suggestions for new features, please
submit an issue on the [Issues
page](https://github.com/feinleib/filibustr/issues) of this package’s
GitHub repository!

## Data sources

This package uses data from the following websites and research:

- Harbridge-Yong, L., Volden, C., & Wiseman, A. E. (2023). The
  Bipartisan Path to Effective Lawmaking. *The Journal of Politics*,
  *85*(3), 1048–1063. <https://doi.org/10.1086/723805>
- Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron
  Rudkin, and Luke Sonnet (2025). *Voteview: Congressional Roll-Call
  Votes Database.* <https://voteview.com/>
- U.S. Senate. <https://www.senate.gov/>
- Volden, C., & Wiseman, A. E. (2025). *Legislative Effectiveness
  Scores* \[dataset\]. Center for Effective Lawmaking.
  <https://thelawmakers.org/>
