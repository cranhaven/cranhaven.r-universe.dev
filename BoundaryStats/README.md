
# BoundaryStats

BoundaryStats was designed to test for the presence of geographic
boundaries in ecological variables and overlap between such boundaries.
Users can calculate boundary and boundary overlap statistics with raster
data. BoundaryStats can create null distributions for the statistics
based on various neutral landscape models that are parameterized on the
empirical data. The primary functions are statistical tests for the
presence of spatial boundaries of a variable and significant overlap
between the spatial boundaries of two variables.

[![DOI](https://zenodo.org/badge/534683960.svg)](https://zenodo.org/badge/latestdoi/534683960)

## Installation

You can install BoundaryStats with either:

``` r
install.packages('BoundaryStats')
remotes::install_github("aluo734/BoundaryStats")
```

## Statistical Tests

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Function
</th>
<th style="text-align:left;">
Category
</th>
<th style="text-align:left;">
Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
n_subgraph
</td>
<td style="text-align:left;">
Boundary
</td>
<td style="text-align:left;">
The number of subgraphs, or sets of contiguous boundary elements, in the
data.
</td>
</tr>
<tr>
<td style="text-align:left;">
max_subgraph
</td>
<td style="text-align:left;">
Boundary
</td>
<td style="text-align:left;">
The length of the longest subgraph.
</td>
</tr>
<tr>
<td style="text-align:left;">
Odirect
</td>
<td style="text-align:left;">
Boundary Overlap
</td>
<td style="text-align:left;">
The number of directly overlapping boundary elements, or raster cells
labelled as part of a boundary, of two traits.
</td>
</tr>
<tr>
<td style="text-align:left;">
Ox
</td>
<td style="text-align:left;">
Boundary Overlap
</td>
<td style="text-align:left;">
The average minimum distance between each boundary element in raster x
and the nearest boundary element in raster y. Uses Euclidean distance.
The boundaries of trait x depend on the boundaries of trait y.
</td>
</tr>
<tr>
<td style="text-align:left;">
Oxy
</td>
<td style="text-align:left;">
Boundary Overlap
</td>
<td style="text-align:left;">
The average minimum distance between boundary elements in two raster
layers. Uses Euclidean distance. Boundaries for each trait affect one
another reciprocally (x affects y and y affects x).
</td>
</tr>
</tbody>
</table>

## Example

``` r
library(BoundaryStats)
library(tidyverse)

data(T.cristatus)
T.cristatus <- terra::rast(T.cristatus_matrix, crs = T.cristatus_crs)
ext(T.cristatus) <- T.cristatus_ext

data(grassland)
grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
ext(grassland) <- grassland_ext

Tcrist_boundaries <- categorical_boundary(T.cristatus)
grassland_boundaries <- define_boundary(grassland, threshold = 0.1)
plot_boundary(Tcrist_boundaries, grassland_boundaries)

Tcrist_ovlp_null <- overlap_null_distrib(T.cristatus, grassland, rand_both = FALSE, x_cat = T, n_iterations = 100, x_model = 'random_cluster')

Odirect(Tcrist_boundaries, grassland_boundaries, Tcrist_ovlp_null)
Ox(Tcrist_boundaries, grassland_boundaries, Tcrist_ovlp_null)
Oxy(Tcrist_boundaries, grassland_boundaries, Tcrist_ovlp_null)
```

Data source: Cox, Karen; Schepers, Robbert; Van Breusegem, An;
Speybroeck, Jeroen (2023), The common ground in landscape effects on
gene flow in two newt species in an agroecosystem, Dryad, Dataset,
<https://doi.org/10.5061/dryad.bk3j9kdhz>.
