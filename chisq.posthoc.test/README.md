[![Build Status](https://travis-ci.com/ebbertd/chisq.posthoc.test.svg?branch=master)](https://travis-ci.com/ebbertd/chisq.posthoc.test)

# Overview

chisq.posthoc.test is a R package that is designed to run a post hoc analysis for Pearson's Chi-squared Test for Count Data. The residuals as well as p-values will be returned.

## Installation

The easiest way to get chisq.posthoc.test is to install it using the devtools:

```R
install.packages("devtools")
devtools::install_github("ebbertd/chisq.posthoc.test")
```

## Usage

```R
chisq.posthoc.test(x, y, method = "bonferroni", ...)
```

x corresponds to x of the chisq.test function and are passed on to it. The method indicates the p adjustment method and defaults to the Bonferroni method. Additional options can be given which are passed on to the chisq.test function.

## Notes

This work was inspired by the Youtube videos on how to do the [Chi-Square Post-Hoc Testing in SPSS](https://www.youtube.com/watch?v=cOu9rv83G-I).

## References

 T. Mark Beasley & Randall E. Schumacker (1995) Multiple Regression Approach to Analyzing Contingency Tables: Post Hoc and Planned Comparison Procedures, The Journal of Experimental Education, 64:1, 79-93, DOI: [10.1080/00220973.1995.9943797](https://doi.org/10.1080/00220973.1995.9943797)
