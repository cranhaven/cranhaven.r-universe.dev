# BigDataStatMeth <!-- <img src="man/figures/logo.svg" align="right" height="139" alt="" /> -->

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/BigDataStatMeth)](https://CRAN.R-project.org/package=BigDataStatMeth)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/BigDataStatMeth)](https://CRAN.R-project.org/package=BigDataStatMeth)
[![Documentation](https://img.shields.io/badge/docs-online-blue.svg)](https://isglobal-brge.github.io/BigDataStatMeth/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->
<!--[![R-CMD-check](https://github.com/isglobal-brge/BigDataStatMeth/workflows/R-CMD-check/badge.svg)](https://github.com/isglobal-brge/BigDataStatMeth/actions) -->

## Overview

**BigDataStatMeth** provides efficient statistical methods and linear algebra operations for large-scale data analysis using block-wise algorithms and HDF5 storage. Designed for genomic, transcriptomic, and multi-omic data analysis, it enables processing datasets that exceed available RAM through intelligent data partitioning and disk-based computation.

The package offers both **R** and **C++** APIs, allowing flexible integration into existing workflows while maintaining high performance for computationally intensive operations.

### Key Features

- **Block-wise algorithms**: Process data larger than memory through intelligent partitioning
- **HDF5 integration**: Seamless storage and computation with hierarchical data format
- **Parallel processing**: Multi-threaded operations for enhanced performance
- **Dual API**: Complete R interface with underlying C++ implementation for performance
- **Statistical methods**: PCA, SVD, CCA, regression models, and more
- **Production-ready**: Extensively tested on genomic datasets with millions of features

## Installation

### From CRAN (Stable Release)

```r
install.packages("BigDataStatMeth")
```

### From GitHub (Development Version)

```r
# Install devtools if needed
install.packages("devtools")

# Install BigDataStatMeth
devtools::install_github("isglobal-brge/BigDataStatMeth")
```

### System Requirements

**R packages:**
- Matrix
- rhdf5 (Bioconductor)
- RcppEigen
- RSpectra

**System dependencies:**
- HDF5 library (>= 1.8)
- C++11 compatible compiler
- For Windows: [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

Install Bioconductor dependencies:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install(c("rhdf5", "HDF5Array"))
```

## Quick Start

### Basic Workflow: PCA on Large Genomic Data

```r
library(BigDataStatMeth)
library(rhdf5)

# Create HDF5 file from matrix
genotype_matrix <- matrix(rnorm(5000 * 10000), 5000, 10000)
bdCreate_hdf5_matrix(
  filename = "genomics.hdf5",
  object = genotype_matrix,
  group = "data",
  dataset = "genotypes"
)

# Perform block-wise PCA
pca_result <- bdPCA_hdf5(
  filename = "genomics.hdf5",
  group = "data",
  dataset = "genotypes",
  k = 4,              # Number of blocks
  bcenter = TRUE,     # Center data
  bscale = FALSE,     # Don't scale
  threads = 4         # Use 4 threads
)

# Access results
components <- pca_result$components
variance_explained <- pca_result$variance_prop
```

### Working with HDF5 Files

```r
# Matrix operations directly on HDF5
result <- bdblockmult_hdf5(
  filename = "data.hdf5",
  group = "matrices",
  A = "matrix_A",
  B = "matrix_B"
)

# Cross-product
crossp <- bdCrossprod_hdf5(
  filename = "data.hdf5",
  group = "matrices",
  A = "matrix_A"
)

# SVD decomposition
svd_result <- bdSVD_hdf5(
  filename = "data.hdf5",
  group = "matrices",
  dataset = "matrix_A",
  k = 8,
  threads = 4
)
```

## Core Functionality

### Linear Algebra Operations

| Operation | R Function | Features |
|-----------|------------|----------|
| Matrix multiplication | `bdblockmult_hdf5()` | Block-wise, parallel, HDF5 |
| Cross-product | `bdCrossprod_hdf5()` | t(A) %*% A, t(A) %*% B |
| Transposed cross-product | `bdtCrossprod_hdf5()` | A %*% t(A), A %*% t(B) |
| SVD | `bdSVD_hdf5()` | Block-wise, hierarchical |
| QR decomposition | `bdQR_hdf5()` | Block-wise |
| Cholesky | `bdCholesky_hdf5()` | For positive-definite matrices |
| Matrix inversion | `bdInvCholesky_hdf5()` | Via Cholesky decomposition |

### Statistical Methods

| Method | R Function | Description |
|--------|------------|-------------|
| Principal Component Analysis | `bdPCA_hdf5()` | Block-wise PCA with centering/scaling |
| Singular Value Decomposition | `bdSVD_hdf5()` | Hierarchical block-wise SVD |
| Canonical Correlation Analysis | `bdCCA_hdf5()` | Multi-omic data integration |
| Linear Regression | `bdlm_hdf5()` | Large-scale regression models |

### Data Management

| Operation | R Function | Purpose |
|-----------|------------|---------|
| Create HDF5 dataset | `bdCreate_hdf5_matrix()` | Initialize HDF5 files |
| Normalize data | `bdNormalize_hdf5()` | Center and/or scale |
| Remove low-quality data | `bdRemovelowdata_hdf5()` | Filter by missing values |
| Impute missing values | `bdImputeSNPs_hdf5()` | Mean/median imputation |
| Split datasets | `bdSplit_matrix_hdf5()` | Partition into blocks |
| Merge datasets | `bdBind_hdf5_datasets()` | Combine by rows/columns |

### Utility Functions

| Function | Purpose |
|----------|---------|
| `bdgetDim_hdf5()` | Get dataset dimensions |
| `bdExists_hdf5_element()` | Check if dataset exists |
| `bdgetDatasetsList_hdf5()` | List all datasets in group |
| `bdRemove_hdf5_element()` | Delete dataset or group |
| `bdImportTextFile_hdf5()` | Import text files to HDF5 |

## Documentation

Comprehensive documentation is available at **[https://isglobal-brge.github.io/BigDataStatMeth/](https://isglobal-brge.github.io/BigDataStatMeth/)**

### Sections

- **[Getting Started](https://isglobal-brge.github.io/BigDataStatMeth/tutorials/getting-started.html)**: Installation and first steps
- **[Fundamentals](https://isglobal-brge.github.io/BigDataStatMeth/fundamentals/)**: HDF5 storage and block-wise computing concepts
- **[Workflows](https://isglobal-brge.github.io/BigDataStatMeth/workflows/)**: Complete analysis examples (PCA, CCA, cross-platform integration)
- **[Developing Methods](https://isglobal-brge.github.io/BigDataStatMeth/developing-methods/)**: Building new statistical methods with BigDataStatMeth
- **[API Reference](https://isglobal-brge.github.io/BigDataStatMeth/api-reference/)**: Complete function documentation (R and C++)
- **[Technical Guide](https://isglobal-brge.github.io/BigDataStatMeth/technical/performance.html)**: Performance optimization and benchmarking

### Vignettes

```r
# List available vignettes
vignette(package = "BigDataStatMeth")

# View specific vignette
vignette("getting-started", package = "BigDataStatMeth")
vignette("pca-genomics", package = "BigDataStatMeth")
```

## Performance

BigDataStatMeth is designed for efficiency:

- **Block-wise computation**: Process 100+ GB datasets with 8-16 GB RAM
- **Parallel algorithms**: Multi-core support for matrix operations
- **Optimized I/O**: Efficient HDF5 chunking and access patterns
- **Memory management**: Controlled memory usage through block size tuning

## Use Cases

BigDataStatMeth is particularly suited for:

- **Genomics**: GWAS, eQTL analysis, population genetics
- **Transcriptomics**: RNA-seq analysis, differential expression
- **Multi-omics**: Data integration (CCA, MOFA-style analyses)
- **Large-scale statistics**: Any analysis requiring matrix operations on big data
- **Method development**: Building new statistical methods for big data

## Examples

### Example 1: Genomic PCA with Quality Control

```r
library(BigDataStatMeth)

# Load genomic data
bdCreate_hdf5_matrix("gwas.hdf5", genotypes, "data", "snps")

# Quality control
bdRemovelowdata_hdf5("gwas.hdf5", "data", "snps", 
                     pcent = 0.05, bycols = TRUE)  # Remove SNPs >5% missing

# Impute remaining missing values
bdImputeSNPs_hdf5("gwas.hdf5", "data", "snps_filtered")

# Perform PCA
pca <- bdPCA_hdf5("gwas.hdf5", "data", "snps_filtered", 
                  k = 8, bcenter = TRUE, threads = 4)

# Plot results
plot(pca$components[,1], pca$components[,2],
     xlab = "PC1", ylab = "PC2",
     main = "Population Structure")
```

### Example 2: Multi-Omic CCA

```r
# Prepare data
bdCreate_hdf5_matrix("multi_omic.hdf5", gene_expression, "data", "genes")
bdCreate_hdf5_matrix("multi_omic.hdf5", methylation, "data", "cpgs")

# Normalize
bdNormalize_hdf5("multi_omic.hdf5", "data", "genes", 
                 bcenter = TRUE, bscale = TRUE)
bdNormalize_hdf5("multi_omic.hdf5", "data", "cpgs",
                 bcenter = TRUE, bscale = TRUE)

# Canonical Correlation Analysis
cca <- bdCCA_hdf5(
  filename = "multi_omic.hdf5",
  X = "NORMALIZED/data/genes",
  Y = "NORMALIZED/data/cpgs",
  m = 10  # Number of blocks
)

# Extract canonical correlations
correlations <- h5read("multi_omic.hdf5", "Results/cor")
```

### Example 3: Custom Method Development (C++ API)

```cpp
#include <Rcpp.h>
#include "BigDataStatMeth.hpp"

using namespace BigDataStatMeth;

// [[Rcpp::export]]
void custom_analysis(std::string filename, std::string dataset) {
  
  hdf5Dataset* ds = new hdf5Dataset(filename, dataset, false);
  ds->openDataset();
  
  // Your custom algorithm using BigDataStatMeth functions
  // Block-wise processing, matrix operations, etc.
  
  delete ds;
}
```

See [Developing Methods](https://isglobal-brge.github.io/BigDataStatMeth/developing-methods/) for complete examples.

## Citation

If you use BigDataStatMeth in your research, please cite:

```
Pelegri-Siso D, Gonzalez JR (2024). BigDataStatMeth: Statistical Methods 
for Big Data Using Block-wise Algorithms and HDF5 Storage. 
R package version X.X.X, https://github.com/isglobal-brge/BigDataStatMeth
```

BibTeX entry:

```bibtex
@Manual{bigdatastatmeth,
  title = {BigDataStatMeth: Statistical Methods for Big Data},
  author = {Dolors Pelegri-Siso and Juan R. Gonzalez},
  year = {2024},
  note = {R package version X.X.X},
  url = {https://github.com/isglobal-brge/BigDataStatMeth},
}
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines

- Follow existing code style (Rcpp coding standards)
- Add tests for new functionality
- Update documentation (Roxygen2 for R, Doxygen for C++)
- Run `R CMD check` before submitting

## Getting Help

- **Documentation**: [https://isglobal-brge.github.io/BigDataStatMeth/](https://isglobal-brge.github.io/BigDataStatMeth/)
- **Issues**: [GitHub Issues](https://github.com/isglobal-brge/BigDataStatMeth/issues)

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Authors

**Dolors Pelegri-Siso**  
Bioinformatics Research Group in Epidemiology (BRGE)  
ISGlobal - Barcelona Institute for Global Health

**Juan R. Gonzalez**  
Bioinformatics Research Group in Epidemiology (BRGE)  
ISGlobal - Barcelona Institute for Global Health

## Acknowledgments

Development of BigDataStatMeth was supported by ISGlobal and the Bioinformatics Research Group in Epidemiology (BRGE).
