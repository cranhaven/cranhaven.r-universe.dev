# ggsmc

Functions for plotting, and animating, the output of importance samplers, sequential Monte Carlo samplers (SMC) and ensemble-based methods. The package can be used to plot and animate histograms, densities, scatter plots and time series, and to plot the genealogy of an SMC or ensemble-based algorithm. These functions all rely on algorithm output to be supplied in tidy format. A function is provided to transform algorithm output from matrix format (one Monte Carlo point per row) to the tidy format required by the plotting and animating functions.

The package can be installed using:

```
devtools::install_github("richardgeveritt/ggsmc")
```

Examples of how to use the package can be found in [this vignette](https://richardgeveritt.github.io/ggsmc/articles/Visualising.html).

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/richardgeveritt/ggsmc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/richardgeveritt/ggsmc/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
