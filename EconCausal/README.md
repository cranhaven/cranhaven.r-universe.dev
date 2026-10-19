# EconCausal

An R package for econometric causal inference with emphasis on temporal stability, implementing three complementary methodologies:

1. Bayesian GLM with AR(1) errors (BGLM-AR1)
2. Error Correction Models with MARS (ECM-MARS)
3. Bayesian Structural Time Series (BSTS)

## Installation

```r
# From CRAN
install.packages("EconCausal")

# From GitHub
remotes::install_github("IsadoreNabi/EconCausal")

# If vignette building fails on Windows:
remotes::install_github("IsadoreNabi/EconCausal", build_vignettes = FALSE)
```
