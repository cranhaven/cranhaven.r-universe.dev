
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scipub

<!-- badges: start -->

[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/scipub?color=blue)](https://r-pkg.org/pkg/scipub)
![CRAN RStudio
version](https://www.r-pkg.org/badges/version-last-release/scipub)
<!-- badges: end -->

<https://dpagliaccio.github.io/scipub/>

This package contains functions for summarizing data for scientific
publication. This includes making a “Table 1” to summarize demographics
across groups, correlation tables with significance indicated by stars,
and extracting formatted statistical summarizes from simple tests for
in-text notation. The package also includes functions for Winsorizing
data based on a Z-statistic cutoff.

Functions:  
apastat - Format simple statistic test results for scientific
publication  
correltable - Create correlation table (with stars for significance) for
scientific publication  
FullTable1 - Create Table1 of group summary with stats for scientific
publication  
gg\_groupplot - Create ggplot to display group differences
(box+point+hist) winsorZ\_find - Identify outliers based on z-score
cutoff that are Winsorized by the `winsorZ` function  
winsorZ - Winsorize outliers based on z-score cutoff to next most
extreme non-outlier value

## Installation

You can install the released version of scipub from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("scipub")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dpagliaccio/scipub")
```

## Example

This is a basic example:

``` r
library(scipub)
correltable(data=psydat)
#> Warning: Converting non-numeric columns to factor: Sex,Income
#> $table
#>          Age Sex     Income  Height iq          depressT   anxT      
#> Age          t=-1.86 F=4.17* .41*** .09***      .02        -.01      
#> Sex                  χ2=0.72 t=0.83 t=1.25      t=-4.87*** t=-5.76***
#> Income                       F=1.15 F=364.33*** F=31.18*** F=16.26***
#> Height                              .04*        -.01       -.01      
#> iq                                              -.08***    -.06***   
#> depressT                                                   .61***    
#> anxT                                                                 
#> 
#> $caption
#> [1] "Note. This table presents Pearson correlation coefficients with pairwise deletion. N=4 missing Sex. N=404 missing Income. N=7 missing Height. N=179 missing iq. N=8 missing depressT. N=8 missing anxT.  Group differences for continuous and categorical\n             variables are indicated by t-statistic/ANOVA F\n             and chi-squared, respectively. * p<.05, ** p<.01, *** p<.001"
```
