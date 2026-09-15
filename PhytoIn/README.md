PhytoIn
================

- [PhytoIn
  <img src="man/figures/logo.png" align="right" height="110" />](#phytoin-)
  - [Installation](#installation)
  - [Overview](#overview)
  - [Quick start](#quick-start)
  - [Citation](#citation)
  - [Contributing](#contributing)
  - [License](#license)
  - [Development notes](#development-notes)

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/PhytoIn)](https://CRAN.R-project.org/package=PhytoIn)
[![R-CMD-check](https://github.com/PhytoIn/PhytoIn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PhytoIn/PhytoIn/actions)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
<!-- badges: end -->

# PhytoIn <img src="man/figures/logo.png" align="right" height="110" />

Tools for vegetation analysis and forest inventory in R.

**PhytoIn** provides functions and example datasets for
phytosociological analysis, forest inventory, biomass and carbon
estimation, and visualization of vegetation data.

------------------------------------------------------------------------

## Installation

### From CRAN (when available)

``` r
install.packages("PhytoIn")
```

### Development version (GitHub)

``` r
# install.packages("remotes")
remotes::install_github("PhytoIn/PhytoIn")
```

------------------------------------------------------------------------

## Overview

Main features:

- **Structural parameters and diversity**  
  `phytoparam()`, `summary.param()`, `plot.param()`

- **Biomass and carbon**  
  `AGB()` — wrapper around **BIOMASS** for above-ground biomass (AGB),
  C, and CO₂e

- **Volume stratification**  
  `stratvol()` — wood volume by DBH classes

- **Species accumulation and rarefaction**  
  `collector.curve()`, `rarefaction()`

- **Basal area visualization**  
  `BAplot()` — basal areas on quadrat maps (supports rectangular plots
  and individual coords)

Example datasets included: `quadrat.df`, `point.df`, `quadrat2_plot.df`,
`quadrat2_tree.df`, `quadrat3_rect.df`.

------------------------------------------------------------------------

## Quick start

> Example chunks are **not executed** when building this README. Run
> them interactively.

### Phytosociological parameters

``` r
library(PhytoIn)

res <- phytoparam(
  x = quadrat.df, measure.label = "CBH", taxon = "Species",
  family = "Family", su = "Plot", su.size = 25
)

summary(res)      # S3 summary
plot(res)         # S3 plot
head(res$param)   # Taxon-level table
res$global        # Global metrics
```

### Rarefaction curve

``` r
rarefaction(formula = Species ~ Plot - Morta, data = quadrat.df, plot = TRUE)
```

### Collector’s curve

``` r
collector.curve(formula = Species ~ Plot - Morta, data = quadrat.df,
                times = 1000, plot = TRUE, theme = "theme_classic")
```

### Biomass and carbon (AGB)

``` r
out <- AGB(
  x = quadrat.df, measure.label = "CBH", h = "h", taxon = "Species",
  dead = "Morta", circumference = TRUE, su = "Plot", area = 0.0625,
  rm.dead = TRUE, check.spelling = FALSE, correct.taxon = TRUE, long = TRUE
)

head(out$tree)
out$taxon
out$total
out$WD.level
```

> Note: `AGB()` uses **BIOMASS** internally. Internet access is not
> required;  
> taxon standardization via TNRS is used only if `httr2` is available.

### Basal areas on quadrat maps

``` r
BAplot(
  formula = CBH ~ x + y, data = quadrat2_plot.df, taxon = "Species",
  circumference = TRUE, quadrat.size = 5, dead = "Morta",
  rm.dead = FALSE, alpha = 0.4, cex.radius = 2,
  legend = TRUE, long = FALSE, ind.coord = FALSE
)
```

------------------------------------------------------------------------

## Citation

If you use **PhytoIn** in scientific work, please cite this package and
the underlying methods (e.g., Hurlbert 1971; Heck et al. 1975; Chave et
al. 2014).

``` r
citation("PhytoIn")
```

------------------------------------------------------------------------

## Contributing

Issues and pull requests are welcome:

- Issue tracker: <https://github.com/PhytoIn/PhytoIn/issues>

Please follow a minimal reproducible example (reprex) when reporting
bugs.

------------------------------------------------------------------------

## License

GPL-3 © Rodrigo Augusto Santinelo Pereira

------------------------------------------------------------------------

## Development notes

This README is generated from `README.Rmd`. To build:

``` r
rmarkdown::render("README.Rmd")
```

Add to `.Rbuildignore`:

    ^README\.Rmd$
    ^\.github$
