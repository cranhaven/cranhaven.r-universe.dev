
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comparer

<!-- badges: start -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/CollinErickson/comparer.svg?branch=master)](https://app.travis-ci.org/CollinErickson/comparer) -->
<!-- [![Coverage Status](https://img.shields.io/codecov/c/github/CollinErickson/comparer/master.svg)](https://codecov.io/github/CollinErickson/comparer?branch=master) -->
<!-- [![Coverage Status](https://img.shields.io/coveralls/CollinErickson/comparer.svg)](https://coveralls.io/r/CollinErickson/comparer?branch=master) -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/comparer)](https://cran.r-project.org/package=comparer)
[![Coverage
Status](https://codecov.io/gh/CollinErickson/comparer/branch/master/graph/badge.svg)](https://app.codecov.io/github/CollinErickson/comparer?branch=master)
[![Coverage
Status](https://coveralls.io/repos/github/CollinErickson/comparer/badge.svg?branch=master)](https://coveralls.io/github/CollinErickson/comparer?branch=master)
[![R-CMD-check](https://github.com/CollinErickson/comparer/workflows/R-CMD-check/badge.svg)](https://github.com/CollinErickson/comparer/actions)
<!-- badges: end -->

The goal of comparer is to make it easy to compare the results of
different code chunks that are trying to do the same thing. The R
package `microbenchmark` is great for comparing the speed of code, but
there’s no way to compare their output to see which is more accurate.

## Installation

You can install comparer from GitHub with:

``` r
# install.packages("devtools")
# devtools::install_github("CollinErickson/comparer")
```

## `mbc`

One of the two main functions of this package is `mbc`, for “model
benchmark compare.” It is designed to be similar to the package
`microbenchmark`, allow for fast comparisons except including the
output/accuracy of the code evaluated instead of just timing.

Suppose you want to see how the mean and median of a sample of 100
randomly generated data points from an exponential distribution compare.
Then, as demonstrated below, you can use the function `mbc`, with the
functions mean and median, and then `input=rexp(100)`. The value of
`input` will be stored as `x`, so `mean(x)` will find the mean of that
data. It outputs the run times of each, and then the results from the
five trials, where five is the default setting for `times`. The run
times aren’t useful because they are all fast. For more precise timing
(\<0.01 seconds), you should use `microbenchmark`. The trials all have
the same output since there is no randomness, the same data is used for
each trial. The “Output summary” shows that the mean is near 1, while
the median is near 0.6.

``` r
## basic example code
library(comparer)
#> Loading required package: GauPro
#> Loading required package: mixopt
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: ggplot2
#> Loading required package: splitfngr
#> Loading required package: numDeriv
#> Loading required package: rmarkdown
#> Loading required package: tidyr
#> Loading required package: reshape
#> 
#> Attaching package: 'reshape'
#> The following objects are masked from 'package:tidyr':
#> 
#>     expand, smiths
#> The following object is masked from 'package:dplyr':
#> 
#>     rename
#> Loading required package: plyr
#> ------------------------------------------------------------------------------
#> You have loaded plyr after dplyr - this is likely to cause problems.
#> If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
#> library(plyr); library(dplyr)
#> ------------------------------------------------------------------------------
#> 
#> Attaching package: 'plyr'
#> The following objects are masked from 'package:reshape':
#> 
#>     rename, round_any
#> The following objects are masked from 'package:dplyr':
#> 
#>     arrange, count, desc, failwith, id, mutate, rename, summarise,
#>     summarize
#> Loading required package: progress
mbc(mean(x), median(x), input=rexp(100))
#> Run times (sec)
#>    Function        Sort1        Sort2        Sort3        Sort4        Sort5
#> 1   mean(x) 5.006790e-06 5.960464e-06 7.152557e-06 8.106232e-06 5.602837e-05
#> 2 median(x) 2.098083e-05 2.312660e-05 2.479553e-05 4.196167e-05 8.988380e-05
#>           mean           sd neval
#> 1 1.645088e-05 2.215562e-05     5
#> 2 4.014969e-05 2.902476e-05     5
#> 
#> Output summary
#>        Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5      mean sd
#> 1   mean(x)    1 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470  0
#> 2 median(x)    1 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696  0
```

To get the data to be generated for each trial, use the `inputi`
argument to set a variable that the functions call. The arguments
`mean(x)` and `median(x)` are captured as expressions. `rexp(100)` will
be stored as `x` by default. You can see that the values are now
different for each trial.

``` r
## Regenerate the data each time
mbc(mean(x), median(x), inputi=rexp(100))
#> Run times (sec)
#>    Function        Sort1        Sort2        Sort3        Sort4        Sort5
#> 1   mean(x) 5.960464e-06 5.960464e-06 5.960464e-06 6.914139e-06 1.692772e-05
#> 2 median(x) 2.288818e-05 2.503395e-05 2.503395e-05 2.980232e-05 4.911423e-05
#>           mean           sd neval
#> 1 8.344650e-06 4.815819e-06     5
#> 2 3.037453e-05 1.077721e-05     5
#> 
#> Output summary
#>        Func Stat        V1        V2        V3        V4        V5     mean
#> 1   mean(x)    1 0.9890381 0.9069863 0.8813966 1.2063718 1.0568761 1.008134
#> 2 median(x)    1 0.6836623 0.6488801 0.6404516 0.7901115 0.7825493 0.709131
#>          sd
#> 1 0.1307018
#> 2 0.0723598
```

The variable name, or multiple variables, can be set in `inputi` by
using braces `{}` In the example below, values are set for `a` and `b`,
which can then be called by the expressions to be compared.

``` r
mbc(mean(a+b), mean(a-b), inputi={a=rexp(100);b=runif(100)})
#> Run times (sec)
#>      Function        Sort1        Sort2        Sort3        Sort4        Sort5
#> 1 mean(a + b) 5.960464e-06 5.960464e-06 6.198883e-06 6.914139e-06 1.788139e-05
#> 2 mean(a - b) 5.960464e-06 5.960464e-06 6.198883e-06 8.106232e-06 1.001358e-05
#>           mean           sd neval
#> 1 8.583069e-06 5.212596e-06     5
#> 2 7.247925e-06 1.788934e-06     5
#> 
#> Output summary
#>          Func Stat        V1        V2        V3       V4        V5      mean
#> 1 mean(a + b)    1 1.4851116 1.5601898 1.3481168 1.600197 1.4810187 1.4949268
#> 2 mean(a - b)    1 0.5518472 0.5843345 0.4168324 0.628536 0.4586843 0.5280469
#>           sd
#> 1 0.09641584
#> 2 0.08805201
```

## `ffexp`

The other main function of the package is `ffexp`, an abbreviation for
full-factorial experiment. It will run a function using all possible
combinations of input parameters given. It is useful for running
experiments that take a long time to complete.

The first arguments given to `ffexp$new` should give the possible values
for each input parameter. In the example below, `a` can be 1, 2, or 3,
and `b` can “a”, “b”, or “c”. Then `eval_func` should be given that can
operate on these parameters. For example, using `eval_func = paste` will
paste together the value of `a` with the value of `b`.

``` r
f1 <- ffexp$new(
  a=1:3,
  b=c("a","b","c"),
  eval_func=paste
)
```

After creating the `ffexp` object, we can call `f1$run_all` to run
`eval_func` on every combination of `a` and `b`.

``` r
f1$run_all()
```

Now to see the results in a clean format, look at `f1$outcleandf`.

``` r
f1$outcleandf
#>   a b  V1 runtime          start_time            end_time run_number
#> 1 1 a 1 a       0 2024-09-28 11:17:53 2024-09-28 11:17:53          1
#> 2 2 a 2 a       0 2024-09-28 11:17:53 2024-09-28 11:17:53          2
#> 3 3 a 3 a       0 2024-09-28 11:17:53 2024-09-28 11:17:53          3
#> 4 1 b 1 b       0 2024-09-28 11:17:53 2024-09-28 11:17:53          4
#> 5 2 b 2 b       0 2024-09-28 11:17:53 2024-09-28 11:17:53          5
#> 6 3 b 3 b       0 2024-09-28 11:17:53 2024-09-28 11:17:53          6
#> 7 1 c 1 c       0 2024-09-28 11:17:53 2024-09-28 11:17:53          7
#> 8 2 c 2 c       0 2024-09-28 11:17:53 2024-09-28 11:17:53          8
#> 9 3 c 3 c       0 2024-09-28 11:17:53 2024-09-28 11:17:53          9
```

## `hype`: Hyperparameter Optimization

`hype` uses Bayesian optimization to find the best parameters/inputs for
a function that is slow to evaluate. (If the function can be evaluated
quickly, then you can use standard optimization methods.) A common use
case is for hyperparameter tuning: when fitting a model that has
multiple hyperparameters, you want to find the best values to set the
hyperparameters to but can only evaluate a small number of settings
since each is slow.
