The `nodiv` R package

 [![Downloads](http://cranlogs.r-pkg.org/badges/nodiv?color=brightgreen)](https://cran.r-project.org/package=nodiv) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/nodiv)](https://cran.r-project.org/package=nodiv) [![codecov](https://codecov.io/github/mkborregaard/nodiv/branch/master/graph/badge.svg?token=7mDtrqzWJG)](https://app.codecov.io/github/mkborregaard/nodiv)

## nodiv - Node-based analysis of species distributions

The package implements Borregaard et al. (2014) method for identifying nodes in a phylogeny associated with divergent distributions. The main algorithm goes through each node in the phylogeny and relates node overlap to a null model. The package also provides functions for preparing the data sets, for exploratory plots and further analysis.

The workflow starts by creating `nodiv_data` data object, which makes sure that the phylogeny, the species distributions and the spatial coordinates of sites are matched correctly. The function takes a number of data types. There are several functions for exploratory data analysis, including `plot`, `summary`, `richness`, `Node_occupancy`, `Node_size` etc. The core of the package are the functions `Nodesig`, which compares the distributions of the two clades descending from a node, and `Node_analysis`, which applies this function to all nodes in the phylogeny and summarizes the results as a `nodiv_result` object. There is a set of functions for interpretation of the results, including `plot`, `plotSOS`, and `summary`. The package also provides basic functions for plotting and manipulating data sets that combine spatial distributions with phylogenies, e.g. `subsample`, `plot_grid` and `plot_points`.  
