# rgm: Random Graphical Models for data from multiple environments

`rgm` is an R package that implements state-of-the-art Random Graphical Models (RGMs) for the analysis of complex multivariate data. It is able to handle heterogeneous data across various environments, offering a powerful tool for exploring intricate network interactions and structural relationships.

## Key Features

- **Joint Inference Across Multiple Environments**: `rgm` enables simultaneous analysis of multivariate data from diverse environments, providing a comprehensive understanding of complex network interactions.
- **Random Graphical Modeling**: The package includes a generative model of graphs across environments to handle heterogeneity and quantify structural relationships across environments.
- **Integration of External Covariates**: Users can incorporate external covariates at both node and interaction levels, allowing for a more complete analysis of network data.
- **Bayesian Framework**: `rgm` uses a Bayesian approach to quantify parameter uncertainty, including uncertainty on the inferred graphs.

## Installation

Install the latest version of `rgm` from GitHub using the following commands in R:

```R
install.packages("devtools")
devtools::install_github("franciscorichter/rgm", build_vignette=TRUE)
```

## Usage

For detailed instructions on using `rgm` for data analysis, refer to the package vignette and documentation:

```R
library(rgm)
vignette("rgm")
```

**Note**: While initially designed for microbiome analysis, `rgm` is broadly applicable across various fields requiring advanced graphical modeling of multivariate data from multiple environments.

## Principal Reference

The methodologies implemented in the rgm package are principally derived from the work described in Vinciotti, V., Wit, E., & Richter, F. (2023). "Random Graphical Model of Microbiome Interactions in Related Environments." arXiv preprint arXiv:2304.01956.


