
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trafficCAR

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Development
status](https://img.shields.io/badge/devel-active-brightgreen.svg)](#)
[![R \>=
3.5](https://img.shields.io/badge/R-%3E%3D%203.5-blue.svg)](https://www.r-project.org/)
<!-- badges: end -->

## Overview

`trafficCAR` is an R package for constructing conditional autoregressive
(CAR) precision matrices on graph and road network data. It is built for
users seeking principled spatial dependence structures for linear
networks (streets, paths, and segmented roadways) and end-to-end
utilities for turning raw road geometries into model-ready
adjacency/weight matrices.

The package is designed to support methodological, simulation-based, and
applied modeling workflows with intrinsic and proper CAR models defined
on network structures. It includes helpers for turning
LINESTRING/MULTILINESTRING features into stable segments, for building
graph objects and sparse matrices, and for scaling or constraining CAR
precision matrices to align with common spatial statistics conventions.

Core functionality includes:

- Building graph representations from spatial road geometries

- Constructing adjacency and weight matrices

- Generating ICAR and proper CAR precision matrices

In addition, `trafficCAR` provides convenient wrappers and plotting
utilities aimed at traffic and speed modeling, so you can fit CAR/ICAR
models and immediately map latent effects back to a road network.

## What the package offers

- **Road and graph preparation**: Convert LINESTRING/MULTILINESTRING
  road data to stable segment IDs with lengths (`roads_to_segments()`),
  construct segment adjacency (`build_adjacency()`), and build a full
  igraph-based road network (`build_network()`).

- **Spatial weights and precision matrices**: Create binary or
  row-standardized weight matrices (`weights_from_adjacency()`), build
  intrinsic and proper CAR precision matrices (`car_precision()`), apply
  Besag scaling (`intrinsic_car_precision()`), and impose sum-to-zero
  constraints for ICAR components (`icar_sum_to_zero()`).

- **Simulation utilities**: Sample from proper CAR latent Gaussian
  models with Gibbs updates (`sample_proper_car()`) or draw from
  multivariate normals with sparse precision matrices for custom
  workflows (`rmvnorm_prec()`).

- **Model fitting for traffic outcomes**: Fit Gaussian CAR/ICAR
  regression models with optional covariates (`fit_car()`), or use
  traffic-oriented wrappers that prepare speed and travel-time outcomes,
  apply transformations, and return augmented fit objects
  (`fit_traffic()`).

- **Augmentation and visualization**: Attach fitted latent effects back
  to road geometries for mapping (`augment_fit()` and
  `augment_traffic_fit()`), create static plots
  (`plot_traffic_static()`), and generate interactive leaflet maps
  (`plot_traffic_interactive()`).

- **Helper utilities**: Simplify road geometries while preserving
  topology (`simplify_roads()`), compute connected components for ICAR
  centering (`components_from_adjacency()`), and derive degree or
  row-standardized matrices used across the CAR constructors.

## Typical workflow

1.  **Prepare road geometries**: Clean and optionally simplify input
    road data. Convert LINESTRING/MULTILINESTRING features into stable,
    length-aware segments with `roads_to_segments()`.

2.  **Build network structure**: Use `build_adjacency()` or
    `build_network()` to produce adjacency/graph objects that encode
    which road segments touch or intersect.

3.  **Create weights/precision matrices**: Convert adjacency into binary
    or row-standardized weights (`weights_from_adjacency()`), then build
    ICAR or proper CAR precision matrices with `car_precision()` or
    `intrinsic_car_precision()`.

4.  **Fit models or simulate**: Fit Gaussian CAR/ICAR regression models
    with `fit_car()` or `fit_traffic()`, or run simulations with
    `sample_proper_car()` and `rmvnorm_prec()` for benchmarking and
    model checking.

5.  **Augment and visualize**: Attach fitted spatial effects back to the
    road geometries (`augment_fit()` or `augment_traffic_fit()`) and
    visualize results via static or interactive plotting helpers.

## Data expectations

- Road geometries should be LINESTRING or MULTILINESTRING features,
  typically stored in `sf` objects.
- Segment-level outcomes or covariates should align with the segments
  produced by `roads_to_segments()` or with the graph indices used to
  build adjacency/weight matrices.
- For ICAR models, connected components are handled via
  `components_from_adjacency()` and sum-to-zero constraints can be
  applied with `icar_sum_to_zero()`.

## Installation

You can install the released version of `trafficCAR` from CRAN:

``` r
install.packages("trafficCAR")
```

To install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("mell00/trafficCAR")
```
