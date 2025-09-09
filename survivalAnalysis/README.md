# Purpose

The goal of survivalAnalysis is to provide a high-level, elegant interface for the most frequent tasks of 
survival analysis, with a focus on the needs of clinical oncology.
The syntax follows the tidyverse philosophy and is made to be used in a pipeline.

# Installation

Install from CRAN:
```r
install.packages("survivalAnalysis")
```

Or install the latest git version from bitbucket:
```r
devtools::install_bitbucket("mwiesweg/survivalAnalysis")
```

# Components

The following areas are covered:

* Univariate analysis
    * Descriptive statistics
    * Log-rank test
    * Pair-wise comparisons
* Multivariate analysis / Cox regression
* Kaplan-Meier plots based on [survminer](https://github.com/kassambara/survminer)
    * Ready-to-use plot by passing the univariate analysis result
    * High-level support for grids of KM plots
* Forest plots based on ggplot2
    * Easy creation by passing the multivariate analysis result, or multiple univariate analysis results
    * Highly customizable

# Getting Started

This package comes with two vignettes, which guide you through all core functionality and are recommended to get started.
