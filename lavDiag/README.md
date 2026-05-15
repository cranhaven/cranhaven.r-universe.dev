# lavDiag <img src="man/figures/logo.png" align="right" width="120"/>

**Diagnostics and Visualization for Latent Variable Models**

---

## Overview

**lavDiag** extends the [`lavaan`](https://lavaan.ugent.be/) ecosystem with a suite of
diagnostic, visualization, and empirical-fit tools for latent variable models
(CFA, SEM, and related frameworks). It provides fast, parallel-safe computation
of factor scores, model-based predictions, and empirical versus model fit curves
for both continuous and ordinal indicators.

All functions are designed to work seamlessly with single-group and multi-group
models, returning tidy `tibble` outputs ready for plotting or downstream analysis.

---

## Key Features

* 🔹 **Fast parallel factor scores**
  `lavPredict_parallel()` — a robust, ordinal-aware replacement for `lavaan::lavPredict()`.

* 🔹 **Augmentation and diagnostics**
  `augment()` — attaches model predictions, residuals, SEs, and CIs to observed data.

* 🔹 **Model-based grids**
  `prepare()` — generates smooth latent grids and model-based item curves for continuous,
  ordinal, and mixed indicators.

* 🔹 **Empirical vs. model item curves**
  `item_data()` + `item_plot()` — compare model-implied and empirical (GAM-based) curves
  with metrics like R², RMSE, and MAE.

* 🔹 **Residual diagnostics**

  * `resid_cor()` — extract residual correlations
  * `resid_corrplot()` — visualize residual correlation matrices
  * `resid_qq()` — Q–Q plots of residual z-statistics
  * `hopper_plot()` — visualize residual “hopper” patterns

* 🔹 **Model summaries and estimates**
  `model_info()` and `parameter_estimates()` — consistent schema for metadata and estimates.

* 🔹 **CFA visualization**
  `plot_cfa()` — clean diagrams using `visNetwork`.

---

## Installation

### From GitHub

```r
# install.packages("remotes")
remotes::install_github("reckak/lavDiag")
```

### From CRAN (once accepted)

```r
install.packages("lavDiag")
```

---

## Quick Example

```r
library(lavaan)
library(lavDiag)

# Example CFA
HS.model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

fit <- cfa(HS.model, data = HolzingerSwineford1939, meanstructure = TRUE)

# Augment observed data with model predictions and residuals
aug <- augment(fit)

# Compute and visualize item-level empirical fit
it <- item_data(fit)
item_plot(it)

# Residual correlation plot
resid_corrplot(fit, type = "cor.bentler")
```

---

## Parallelization

Most computationally intensive functions (e.g., `lavPredict_parallel()`,
`item_data()`, `prepare()`) use [`future`](https://cran.r-project.org/package=future)
and [`furrr`](https://cran.r-project.org/package=furrr) backends for safe parallelism.
Backends are configurable via `.set_future_plan()`.

---

## Dependencies

Core dependencies include:

* `lavaan`, `dplyr`, `tidyr`, `purrr`, `tibble`
* `future`, `furrr`, `mgcv`, `corrplot`, `ggplot2`, `visNetwork`

All functions use consistent tidy-style output and `rlang`-safe programming.

---

## Contributing

Issues, pull requests, and feedback are welcome!

* 📬 **Report bugs:** [GitHub Issues](https://github.com/reckak/lavDiag/issues)
* 💡 **Feature requests:** open an issue or start a discussion

---

## Citation

If you use **lavDiag** in your research, please cite it as:

> Rečka, K. (2025). *lavDiag: Diagnostics and Visualization for Latent Variable Models*.
> GitHub: [https://github.com/reckak/lavDiag](https://github.com/reckak/lavDiag)

---

## License

MIT License © 2025 [Karel Rečka](mailto:reckak@muni.cz)
