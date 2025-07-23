# SCIntRuler: Single-Cell Integration Ruler <a href='https://yuelyu21.github.io/SCIntRuler/'><img src="vignettes/SCIntRuler2.png" align="right" height="139"/></a>

The accumulation of single-cell RNA-seq (scRNA-seq) studies highlights the potential benefits of integrating multiple datasets. By augmenting sample sizes and enhancing analytical robustness, integration can lead to more insightful biological conclusions. However, challenges arise due to the inherent diversity and batch discrepancies within and across studies. `SCIntRuler`, a novel R package, addresses these challenges by guiding the integration of multiple scRNA-seq datasets.

## Why SCIntRuler?
Integrating scRNA-seq datasets can be complex due to various factors, including batch effects and sample diversity. Key decisions – whether to integrate datasets, which method to choose for integration, and how to best handle inherent data discrepancies – are crucial. `SCIntRuler` offers a statistical metric to aid in these decisions, ensuring more robust and accurate analyses.

## Features
- **Informed Decision Making**: Helps researchers decide on the necessity of data integration and the most suitable method.
- **Flexibility**: Suitable for various scenarios, accommodating different levels of data heterogeneity.
- **Robustness**: Enhances analytical robustness in joint analyses of merged or integrated scRNA-seq datasets.
- **User-Friendly**: Streamlines decision-making processes, simplifying the complexities involved in scRNA-seq data integration.


## Installation

First, install the `batchelor` package from Bioconductor:

```R
## Installation
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("batchelor")

# To install `SCIntRuler`, use the following command:
devtools::install_github("yuelyu21/SCIntRuler")
# Load SCIntRuler
library(SCIntRuler)
```
## Run an Example 

To try our new method, please refer to our [getting started with SCIntRuler](https://yuelyu21.github.io/SCIntRuler/articles/SCIntRuler.html) article for user instructions.


