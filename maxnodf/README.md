
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/christophhoeppke/maxnodf.svg?branch=master)](https://travis-ci.org/christophhoeppke/maxnodf)
[![codecov](https://codecov.io/gh/christophhoeppke/maxnodf/branch/master/graph/badge.svg)](https://codecov.io/gh/christophhoeppke/maxnodf)

## Overview

`maxnodf` calculates the maximum NODF value that can be achieved in a
bipartite network with a given number of rows, columns and links.

## Installation

You can install `maxnodf` from github with:

``` r
install.packages("devtools") # install devtools if needed
devtools::install_github("christophhoeppke/maxnodf")
```

## Use

`maxnodf` has three functions:

### `maxnodf()`

For a given network, `maxnodf` calculates the maximum nestedness that
can be achieved in a network with a given number of rows, columns and
links, subject to the constraint that all rows and columns must have at
least one link (i.e. row and column totals must always be \>= 1). As
input, `maxnodf()` takes either a numeric matrix describing a bipartite
network (a bipartite incidence matrix where elements are positive
numbers if nodes interact, and 0 otherwise) or a numeric vector of
length three of the form `c(#Rows, #Columns, #Links)`.

This allows nestedness values to be normalised as NODF/max(NODF)
following Song et al (2017), where NODF is the raw NODF of the network
and max(NODF) is the maximum nestedness that can be achieved in a
network with the same number of rows, columns and links as web, subject
to the constraint that all rows and columns must have at least one link
(as calculated by maxnodf())

`maxnodf` has three algorithms for finding the maximum nestedness of a
bipartite network. These can be set using the `quality` argument. Lower
quality settings are faster, but find worse optima. Higher quality
settings are slower, but find better optima.

  - `quality = 0`, uses a greedy algorithm.
  - `quality = 1`, uses a greedy algorithm plus hillclimbing.
  - `quality = 2`, uses a simulated annealing algorithm, with the greedy
    algorithm output as the start point. Best results, but requires the
    most computation time.

### `NODFc()`

To control for maximum nestedness, connectance and network size, Song et
al. (2017) propose the NODF\_c metric: (NODF/max(NODF))/(C \* log(S))
where C is the network connectance, S is the geometric mean of the
number of plants and pollinators in the network, NODF is the raw NODF of
the network and max(NODF) is the maximum nestedness that can be achieved
in a network with the same number of rows, columns and links as web,
subject to the constraint that all rows and columns must have at least
one link (as calculated by `maxnodf()`). `NODFc` calculates this NODF\_c
metric. As input, `NODFc` takes a numeric matrix describing a bipartite
network, as well as the same `quality` argument values described above.

### `nodf_cpp()`

A simple function that takes a bipartite matrix as input, and returns
the raw NODF value of the matrix. Used internally by `maxnodf`.
Calculation is quick, because it is implemented in C++.

## Example

``` r
m <- matrix(0,10,10) # initialise an empty network
m[1,] <- 1 # ensure all row species have at least one link
m[,1] <- 1 # ensure all column species have at least one link
m[2:10,2:10] <- sample(0:1, 9 * 9, replace = TRUE) # randomise the rest of the matrix
maxnodf(web = m, quality = 2) # calculate the maximum nestedness
```

## License

The code is released under the MIT license (see `LICENSE` file).

## References

Song, C., Rohr, R.P. and Saavedra, S., 2017. Why are some
plant–pollinator networks more nested than others? Journal of Animal
Ecology, 86(6), pp.1417-1424.
