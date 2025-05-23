# CAESAR.Suite

=========================================================================
<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version-ago/CAESAR.Suite)](https://cran.r-project.org/package=CAESAR.Suite)
[![](https://cranlogs.r-pkg.org/badges/CAESAR.Suite?color=orange)](https://cran.r-project.org/package=CAESAR.Suite)
[![](https://cranlogs.r-pkg.org/badges/grand-total/CAESAR.Suite?color=orange)](https://cran.r-project.org/package=CAESAR.Suite)
<!-- badges: end -->

CAESAR suite is an open-source software package that provides image-based spatial co-embedding of locations and genomic features. The 'CAESAR.Suite' package is specifically developed by the Jin Liu's lab for annotation and enrichment analysis of spatially resolved transcriptomics (SRT) dataset. It uniquely transfers labels from scRNA-seq reference, enabling the annotation of spatial omics datasets across different technologies, resolutions, species, and modalities, based on the conserved relationship between signature genes and cells/locations at an appropriate level of granularity. Notably, CAESAR enriches location-level pathways, allowing for the detection of gradual biological pathway activation within spatially defined domain types. 

Check out  our [Package Website](https://XiaoZhangryy.github.io/CAESAR.Suite/index.html) for a more complete description of the methods and analyses. 

CAESAR provides image-based spatial aware co-embedding of locations and genomic features.

By assuming a conserved relationship between genomic features and cells/locations within each cell/domain type at an appropriate level of granularity, the CAESAR suite flexibly annotates spatial omics datasets in a variety of contexts, for instance:

* Using multiple reference datasets.
* Across species.
* Across resolutions.
* Across modalities.
* Across technologies.

The CAESAR suite includes functions for hypothesis testing to identify pathways enriched in each cell/location or cell/domain type. For instance:

* Test whether pathways are enriched in dataset.
* Calculate pathway enrichment score for each cell/location.
* Detect cell/domain type differentially enriched pathway. 

In addition, once the co-embeddings of (multiple) dataset are estimated by CAESAR, the package provides functionality for further data exploration, analysis, and visualization. Users can:

* Detect the signature genes .
* Determine appropriate marker gene sets based on signature gene lists obtained from reference datasets
* Recover comparable gene expression matrices among datasets .
* Integrate signature gene lists from multiple datasets.
* Visualize the co-embeddings on UMAP space.
* Visualize the signature genes on UMAP space.

# Installation
"CAESAR.Suite" depends on the `Rcpp` and `RcppArmadillo` package, which requires appropriate setup of computer. For the users that have set up system properly for compiling C++ files, the following installation command will work.
```{Rmd}
# Method 1: Install CAESAR.Suite from CRAN
install.packages('CAESAR.Suite')

# For the newest version of CAESAR.Suite, users can use method 2 for installation.
# Method 2: Install CAESAR.Suite from Github
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("XiaoZhangryy/CAESAR.Suite")

# If some dependent packages (such as `scater`) on Bioconductor cannot be installed normally, use the following commands, then run the above command.
if (!require("BiocManager", quietly = TRUE)) ## install BiocManager
    install.packages("BiocManager")
# Install the package on Bioconductor
BiocManager::install(c("scater"))
```

## Usage
For usage examples and guided walkthroughs, check the `vignettes` directory of the repo. 

Tutorials for CAESAR suite:

* [Analysis for Xenium breast cancer data](https://XiaoZhangryy.github.io/CAESAR.Suite/articles/XeniumBC.html)
* [Extract image features for Xenium breast cancer data](https://XiaoZhangryy.github.io/CAESAR.Suite/articles/XeniumBCEIF.html)
* [Analysis for ST mouse olfactory bulb data](https://XiaoZhangryy.github.io/CAESAR.Suite/articles/STMOB.html)

For the users that don't have set up system properly, the following setup on different systems can be referred.
## Setup on Windows system
First, download [Rtools](https://cran.r-project.org/bin/windows/Rtools/); second, add the Rtools directory to the environment variable.

## Setup on MacOS system
First, install Xcode. Installation about Xcode can be referred [here](https://stackoverflow.com/questions/8291146/xcode-installation-on-mac).

Second, install "gfortran" for compiling C++ and Fortran at [here](https://github.com/fxcoudert/gfortran-for-macOS).

## Setup on Linux  system
If you use conda environment on Linux system and some dependent packages (such as `scater`) can not normally installed, you can search R package at anaconda.org website. We take the `scater` package as example, and its search result is https://anaconda.org/bioconda/bioconductor-scater. Then you can install it in conda environment by following command.
```{Linux}
conda install -c bioconda bioconductor-scater
```
For the user not using conda environment, if  dependent packages (such as `scater`) not normally installed are in Bioconductor, then use the following command to install the dependent packages.
```{Linux}
# install BiocManager
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
# install the package on Bioconducter
BiocManager::install(c("scater"))
```
If dependent packages (such as `ProFAST`) not normally installed are in CRAN, then use the following command to install the dependent packages.
```{Linux}
# install the package on CRAN
install.packages("ProFAST")
```

# Demonstration

For an example of typical CAESAR.Suite usage, please see our [Package Website](https://XiaoZhangryy.github.io/CAESAR.Suite/index.html) for a demonstration and overview of the functions included in CAESAR.Suite.

# NEWs
* CAESAR.Suite version 0.2 (2025-03-02)
* CAESAR.Suite version 0.1 (2024-09-06)


