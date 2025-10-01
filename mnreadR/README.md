
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mnreadR

<!-- badges: start -->
<!-- badges: end -->

The goal of mnreadR is to analyze MNREAD data using R.

The MNREAD Acuity Charts are continuous-text reading acuity charts for
normal and low vision.

The charts are used to assess how reading performance depends on print
size. Four measures of reading performance are obtained: Reading acuity
(the smallest print that can be read); Maximum reading speed (the
reading speed when performance is not limited by print size); Critical
print size (the smallest print that supports the maximum reading speed)
and Reading accessibility index (a single measure that represents one’s
visual access to printed material)

The four MNREAD parameters are usually estimated by hand by plotting
reading speed as a function of print size. The present package will
provide all necessary functions to perform this estimation automatically
in R.

The MNREAD Acuity Charts have a wide range of applications in testing
normal and low vision: prescribing optical corrections for reading, or
other near tasks in the eye clinic, in low vision assessment,
prescribing magnifiers or other reading aids, applications in pediatrics
and special education, and research. The MNREAD test is widely used as
an outcome measure in clinical trials of treatements for vision
disorders. By providing standardized and automated methods to analyze
the reading test results, this package will be useful for both the
research and medical communities.

## Installation

You can install the released version of mnreadR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mnreadR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mnreadR)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: tidyr
#> Loading required package: ggplot2
#> Welcome to the MNREAD R package!

# inspect the structure of the dataframe
head(data_low_vision, 10)
#>    subject polarity treatment vd  ps    rt err
#> 1       s1  regular         A 20 1.3  6.66   0
#> 2       s1  regular         A 20 1.2  6.53   0
#> 3       s1  regular         A 20 1.1  7.46   0
#> 4       s1  regular         A 20 1.0 11.69   0
#> 5       s1  regular         A 20 0.9 11.16   0
#> 6       s1  regular         A 20 0.8 12.75   0
#> 7       s1  regular         A 20 0.7 23.09   0
#> 8       s1  regular         A 20 0.6 34.62   1
#> 9       s1  regular         A 20 0.5    NA  10
#> 10      s1  regular         A 20 0.4    NA  NA

# run the parameters estimation
data_low_vision_param <- mnreadParam(data_low_vision, ps, vd, rt, err,
                                      subject, polarity)
#> Remember to check the accuracy of MRS and CPS estimates by inspecting the MNREAD curve with mnreadCurve()
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Warning: There was 1 warning in `reframe()`.
#> ℹ In argument: `max(rs)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced

#> Warning in log(omax): NaNs produced
#> Joining with `by = join_by(subject, polarity)`
#> Joining with `by = join_by(subject, polarity)`
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
