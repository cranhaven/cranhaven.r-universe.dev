![C++](https://img.shields.io/badge/C++-RcppCGAL-blue?style=for-the-badge&logo=cplusplus)
![R](https://img.shields.io/badge/-%2764?style=for-the-badge&logo=r&logoColor=grey)
![Topology](https://img.shields.io/badge/Topology-Alpha%20Hull%20-blue?style=for-the-badge)

# ahull3D: Fast 3D Alpha Hull with Label Propagation

Ultra-fast 3D alpha shape computation with label propagation from input points to hull vertices and faces. Optimized for tree segmentation workflows.

## System Requirements

This package uses **RcppCGAL** for geometric computations. RcppCGAL is automatically installed from CRAN as a dependency. No external CGAL installation is required.

## Dependencies

- **Required**: `Rcpp`, `RcppCGAL`, `rgl`, `Rvcg`
- **Suggested**: `lidR`, `RANN`, `tictoc` (for examples)

## Installation

```r
# Install from CRAN (once available)
install.packages("ahull3D")

# Development version from GitHub
remotes::install_github("DijoG/ahull3D")
```

## Usage example

```r
library(lidR)
library(dplyr)
library(ahull3D)

# Read tree point cloud
laz_file <- system.file("extdata", "12trees.laz", package = "ahull3D")
trees <- lidR::readLAS(laz_file)

# Prepare data
lasdff <- treesd@data[,c("X", "Y", "Z", "pid")] %>%
  as.data.frame() %>%
  distinct(across(1:3), .keep_all = TRUE) %>%
  as.matrix()

# Compute alpha hull with tree labels ("pid" propagation)
tictoc::tic()
a <- 
  ahull3D::ahull3D(
  points = lasdff[,1:3],
  input_truth = lasdff[,4],   # "pid" column
  alpha = 0.1
  )
tictoc::toc()  # ~25 seconds 

# Access and check results
input_pid <- attr(a, "input_truth")    # a vector of n vertices
face_pid <- attr(a, "face_truth")      # a matrix of 3*n faces 


length(input_pid) == ncol(a$vb)        # should be TRUE
dim(face_pid) == dim(a$it)             # should be TRUE TRUE
```

## Feautures

  - 🚀 Fast: 25 seconds for 1214385 points
  - 🏷️ Label propagation: Input labels → hull vertices → faces
  - 📏 Correct dimensions: input_truth matches vertices, face_truth is 3×faces
  - 🔧 RcppCGAL powered: Robust geometric computations

  