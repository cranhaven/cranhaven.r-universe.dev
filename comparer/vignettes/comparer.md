---
title: "Introduction to the comparer R package"
author: "Collin Erickson"
date: "2020-03-17"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to the comparer R package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
  
references:
- id: gneiting
  title: Strictly proper scoring rules, prediction, and estimation
  author:
  - family: Gneiting
    given: Tilmann
  - family: Raftery
    given: Adrian E.
  container-title: Journal of the American Statistical Association
  publisher: Taylor \& Francis
  page: 359-378
  type: article-journal
  issued:
    year: 2007
---




When coding, especially for data science, there are multiple
ways to solve each problem.
When presented with two options, you want to pick the one
that is faster and/or more accurate.
Comparing different code chunks on the same task can be tedious.
It often requires creating data, writing a for loop
(or using `sapply`), then comparing.

The comparer package makes this comparison quick and simple:

* The same data can be given in to each model.

* Various metrics can be used to judge results,
including using the predicted errors from the code.

* The results are displayed in a table that allows you
to quickly judge the results.

This document introduces the main function of the `comparer` package, `mbc`.


## Motivation from `microbenchmark`

The R package `microbenchmark` provides the fantastic eponymous function.
It makes it simple to run different segments of code and see which is faster.
Borrowing an example from http://adv-r.had.co.nz/Performance.html,
the following shows how it gives a summary of how fast each ran.


```r
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  x <- runif(100)
  microbenchmark::microbenchmark(sqrt(x), x ^ .5)
} else {
  "microbenchmark not available on your computer"
}
```

```
## [1] "microbenchmark not available on your computer"
```

However it gives no summary of the output.
For this example it is fine since the output is deterministic,
but when working with randomness or model predictions we want
to have some sort of summary or evaluation metric to see which
has better accuracy, or to just see how the outputs differ.


## `mbc` to the rescue

The function `mbc` in the `comparer` package was created to solve this
problem, where a comparison of the output is desired in addition to
the run time.

For example, we may wish to see how the sample size affects
an estimate of the mean of a random sample.
The following shows the results of finding the mean of 10 and 100
samples from a normal distribution.































