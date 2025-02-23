
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WashEx

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/washex)](https://CRAN.R-project.org/package=washex)
[![R-CMD-check](https://github.com/rwrandles/washex-r/workflows/R-CMD-check/badge.svg)](https://github.com/rwrandles/washex-r/actions)
[![Travis build
status](https://travis-ci.com/rwrandles/washex-r.svg?branch=main)](https://api.travis-ci.com/rwrandles/washex-r.svg?branch=main)
<!-- badges: end -->

The **Wash**ington **Ex**plorer (WashEx) package creates an interface
between R and the [Washington State Legislative Web
Services](https://wslwebservices.leg.wa.gov). It serves as a companion
to [Legislative Explorer: Washington](http://legex.org/wa/process), a
project developed by John Wilkerson and Rohnin Randles at the University
of Washington. The package contains functions to retrieve data regarding
the following:

-   Bill summaries
-   Roll calls
-   Amendments
-   Sponsors
-   Committees
-   Committee membership
-   Hearings
-   Revised Code of Washington (RCW)

## Installation

You can install the released version of `washex` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("washex")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rwrandles/washex-r")
```

## Usage

Some possible implementations of the `washex` package include

``` r
library(washex)
library(tidyverse)

sponsors_2007 <- getSponsors("2007-08")
sponsors_2007 %>% 
  group_by(District) %>%
  summarize(prop_repub = round(mean(Party == "R"),3))
#> # A tibble: 49 x 2
#>    District prop_repub
#>    <chr>         <dbl>
#>  1 1             0    
#>  2 10            0.75 
#>  3 11            0    
#>  4 12            1    
#>  5 13            1    
#>  6 14            1    
#>  7 15            1    
#>  8 16            0.667
#>  9 17            0.667
#> 10 18            1    
#> # ... with 39 more rows

head(getCommittees("2007-08")[,"Name"])
#> [1] "Agriculture & Natural Resources"                                 
#> [2] "Appropriations"                                                  
#> [3] "Appropriations Subcommittee on Education"                        
#> [4] "Appropriations Subcommittee on General Government & Audit Review"
#> [5] "Appropriations Subcommittee on Health & Human Services"          
#> [6] "Capital Budget"

head(getStatusChanges("2007-08", "1001") %>%
  select("HistoryLine", "ActionDate"))
#>                                                     HistoryLine
#> 1        First reading, referred to Public Safety & Emerg Prep.
#> 2                   PSEP - Executive action taken by committee.
#> 3 PSEP - Majority; 1st substitute bill be substituted, do pass.
#> 4                                   Referred to Human Services.
#> 5                     HS - Executive action taken by committee.
#> 6   HS - Majority; 2nd substitute bill be substituted, do pass.
#>            ActionDate
#> 1 2007-01-08T00:00:00
#> 2 2007-02-01T00:00:00
#> 3 2007-02-01T00:00:00
#> 4 2007-02-06T00:00:00
#> 5 2007-02-22T00:00:00
#> 6 2007-02-22T00:00:00

ag_bills <- getRCWBills("2007-08", c("15","16"))
getRollCalls.votes("2007-08", ag_bills$BillNumber) %>% 
  filter(str_detect("Final Passage", Motion)) %>%
  mutate(Party = sponsors_2007$Party[match(MemberId, sponsors_2007$Id)]) %>%
  group_by(BillId) %>%
  summarize(repub_support = sum(Vote == "Yea" & Party == "R") / sum(Party == "R"),
            dem_support = sum(Vote == "Yea" & Party == "D") / sum(Party == "D"))
#> # A tibble: 16 x 3
#>    BillId     repub_support dem_support
#>    <chr>              <dbl>       <dbl>
#>  1 E2SHB 1303        0.457        1    
#>  2 E2SHB 2798        0.943        0.984
#>  3 EHB 3381          0.0571       0.841
#>  4 ESB 5204          1            0.984
#>  5 ESHB 1151         1            1    
#>  6 HB 1311           0.971        0.873
#>  7 HB 1418           0.114        0.937
#>  8 HB 1775           0.957        0.968
#>  9 HB 1888           0.971        1    
#> 10 HB 2467           0.943        0.984
#> 11 HB 3106           0.886        0.952
#> 12 SB 6283           0.943        0.984
#> 13 SB 6284           0.943        0.921
#> 14 SHB 1128          0.0286       0.968
#> 15 SHB 1312          1            0.984
#> 16 SHB 1338          0.943        0.889
```

## Issues

If you encounter any bugs or issues while using the package, please file
an issue with a minimum reproducible example on
[Github](https://github.com/rwrandles/washex-r/issues)
