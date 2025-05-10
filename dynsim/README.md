dynsim
======

### Dynamic Simulations of Autoregressive Relationships

**Version 1.2.3**
[![CRAN Version](http://www.r-pkg.org/badges/version/dynsim)](https://CRAN.R-project.org/package=dynsim) ![CRAN Downloads](http://cranlogs.r-pkg.org/badges/last-month/dynsim)
![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/dynsim)
[![R-CMD-check](https://github.com/christophergandrud/dynsim/workflows/R-CMD-check/badge.svg)](https://github.com/christophergandrud/dynsim/actions)

**Christopher Gandrud, Laron K Williams, and Guy D Whitten**

## About

The **dynsim** package implements Williams and Whitten's
([2011](https://www.stata-journal.com/article.html?article=st0242), [2012](http://web.missouri.edu/~williamslaro/Williams%20and%20Whitten%202012.pdf)) method for dynamic simulations of autoregressive relationships in R.

## Process

There are four basic steps to use **dynsim** to create dynamic simulations of
autoregressive relationships:

1. *Estimate* your linear model using `lm` or similar functions.

2. *Set up starting values* for simulation scenarios and (optionally) shock
values at particular iterations (e.g. points in forecasted time).

3. *Simulate* these scenarios based on the estimated model using the `dynsim`
function.

4. *Plot* the simulation results with the `dynsimGG` function.

## Examples

For examples please visit
[https://christophergandrud.github.io/dynsim/](https://christophergandrud.github.io/dynsim/).

## Install

**dynsim** is available on
[CRAN](https://cran.r-project.org/package=dynsim)

You can also easily install the latest development version with the
*devtools* package:

```{S}
devtools::install_github("christophergandrud/dynsim")
```
