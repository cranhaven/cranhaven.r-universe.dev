
# spacejamr: Simulate Spatial Bernoulli Networks

<!-- badges: start -->
[![R-CMD-check](https://github.com/dscolby/spacejamr/workflows/R-CMD-check/badge.svg)](https://github.com/dscolby/spacejamr/actions)
[![codecov.io](https://codecove.io/github/dscolby/spacejamr/coverage.svg?branch=master)](https://github.com/dscolby/spacejamr/actions)
<!-- badges: end -->

The goal of spacejamr is to enable social network analysis where conventional
collection of social network data would be impossible. It does this by providing
tools to prepare shapefiles, simulate spatial point processes, generate networks 
from those point processes using a spatial interaction function. It also 
contains plot methods that return 'ggplot2' objects that can be further refined.

## Installation

You can install the released version of spacejamr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("spacejamr")
```

## Simulate a point process or sequence

``` r
library(spacejamr)

# Load Rhode Island dataset
data(RI)

# Spatial Poisson point process
ri_points <- PointProcess(points = 5000, window = RI, seed = 88)

# Halton sequence
ri_seq <- haltonSeq(points = 5000, window = RI, seed = 9)

```

## Generate networks from spatial interaction functions

``` r
# Standard power law SIF
rinet_standard <- NetSim(point_process = ri_points, base_prob = 0.95, 
                         scale = 100, threshold = 0.5, power = -2.3)

# Attenuated power law SIF
rinet_apl <- NetSim(point_process = ri_points, type = attenuated,
                    base_prob = 0.93, scale = 100, threshold = 0.5, 
                    power = -1.9)

# Arctangent probability law SIF
rinet_arctan <- NetSim(point_process = ri_points, type = arctan,
                       base_prob = 0.93, scale = 100, threshold = 0.5, 
                       power = -1.9)
                    
# Exponential decay law SIF
rinet_arctan <- NetSim(point_process = ri_points, type = decay,
                       base_prob = 0.93, scale = 100, threshold = 0.5, 
                       power = -1.9)
                    
# Logistic probability law SIF
rinet_arctan <- NetSim(point_process = ri_points, type = logistic,
                       base_prob = 0.93, scale = 100, threshold = 0.5, 
                       power = -1.9)

```

## Plot methods

``` r
# Boundaries
plot(RI)

# Point process or sequence realization
plot(ri_points)
plot(ri_seq)

# Network generated from SIF
plot(rinet_standard)
plot(rinet_apl)

```

## Compare two simulated networks

``` r
compare_networks(rinet_standard, rinet_apl)

```

## About
Creator: Darren Colby\
Creater ORCID: 0000-0001-8468-2755\
Maintainer: Darren Colby\
Maintainer email: dscolby17@gmail.com\
Current version: 0.2\
License: MIT
