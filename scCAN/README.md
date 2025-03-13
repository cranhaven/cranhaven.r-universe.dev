# scCAN
scCAN is a R software package that can perform unsupervised clustering for single-cell RNA sequencing (scRNA-seq) data. scCAN overcomes the excessive noise level from scRNA-seq data by using autoencoders and accurately cluster the cells into correct cell types.

# How to install:
- The package can be installed from this repository.
- Install devtools: `utils::install.packages('devtools')`
- Install the package using: `devtools::install_github('bangtran365/scCAN')`
  Or, install with manual and vignette: `devtools::install_github('bangtran365/scCAN', build_manual = T, build_vignettes = T)`

# To run the sample example:
- Load the package: `library(scCAN)`
- Load SCE dataset: `data('SCE'); data <- t(SCE$data); label <- as.character(SCE$cell_type1)`
- Add log transformation: `if(max(data)>100) data <- log2(data + 1)`
- Generate clustering result, the input matrix has rows as samples and columns as genes: `result <- scCAN(data, r.seed = 1)`
- The clustering result can be found here: `cluster <- result$cluster`
- Calculating adjusted Rand Index: `ari <- round(scCAN::adjustedRandIndex(cluster,label), 2)`
- Print out ARI value:  `print(paste0("ARI = ", ari))`
