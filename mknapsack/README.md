[![Build Status](https://travis-ci.org/madedotcom/mknapsack.svg?branch=master)](https://travis-ci.org/madedotcom/mknapsack)
[![codecov.io](https://codecov.io/github/madedotcom/mknapsack/coverage.svg?branch=master)](https://codecov.io/github/madedotcom/mknapsack?branch=master)


# mknapsack

This package solves [multiple knapsack problem](https://en.wikipedia.org/wiki/List_of_knapsack_problems) by assigning items optimally to knapsacks using Mixed Integer Linear Programming (MILP) solver of choice.


## Definition of the mknapsack problem
We start with a list of items that we want to order with each assigned a:
  
*  sku - this is an id of the product / item that we want to order. 
*  profit - expected profit from sales of this item
*  volume - this can be m3 of the box for example 
*  moq - mininum order quanity (MOQ)
*  sold - flag that defines if this item must be added as highest priority prior to othe items

Those items should be optimally packed into multiple containers of the a given size (cap). 
Items should be aded to containers in the way that each container is more profitable than the following one.

## Supported solvers

Package implements interface to several solvers which can be set via `mknapsack.solver` option.

Currently you can choose from those options:

* lpsolve - [lp_solve](http://web.mit.edu/lpsolve/doc/)
* cbc - [CBC COIN-OR](https://projects.coin-or.org/Cbc)
* glpk - [GLPK (GNU Linear Programming Kit)](https://www.gnu.org/software/glpk/)

`lpsolve` is default option.

## Example

Solve problem with [CBC COIN-OR](https://projects.coin-or.org/Cbc) solver:

```R
set.seed(100)
devtools::install_github("dirkschumacher/rcbc")
devtools::install_github("dirkschumacher/ROI.plugin.cbc")
devtools::install_github("madedotcom/mknapsack")
library(rcbc)
library(ROI)
library(ROI.plugin.cbc)
library(data.table)
library(mknapsack)
options(mknapsack.solver = "cbc")

items <- data.table(
  volume = pmin(rlnorm(100, log(2), log(3)), 15),
  profit = rgamma(100, shape = 1, scale = 100) - 25
)

items[, knapsack := 
  mknapsack(
    profit = profit, 
    volume = volume, 
    cap = 65
  )]

#Aggregate solution to knapsacks
knapsacks <- items[order(knapsack), 
                    .(volume = sum(volume), profit = sum(profit)), 
                    by = knapsack]
knapsacks

#    knapsack volume   profit
# 1:        1 64.89659 5000.27608
# 2:        2 64.40358 1540.40302
# 3:        3 64.97235  340.92516
# 4:        4 53.33824   88.02793
# 5:       NA 91.13399 -272.54349
# 

```
