# lavDiag 0.1.0

* **Initial CRAN release.**
* Provides diagnostics and visualization tools for latent variable models fitted with `lavaan` or `blavaan`.

### Core functionality
* `augment()` — augments fitted models with predicted values, residuals, SEs, and CIs (continuous, ordinal, or mixed indicators).
* `prepare()` — constructs model-based item curves and latent grids for continuous/ordinal indicators.
* `lavPredict_parallel()` — robust, parallel wrapper for `lavaan::lavPredict()` with automatic handling of ordinal data.

### Item-level diagnostics
* `item_data()` — extracts model-based and empirical item-level information for plotting or inspection.
* `item_plot()` — visualizes empirical vs. model-implied item curves, including confidence intervals.

### Residual diagnostics
* `resid_cor()` — computes residual (co)relations and z-statistics.
* `resid_corrplot()` — visualizes residual correlations using `corrplot`.
* `resid_qq()` — draws Q–Q plots of residual correlation z-statistics.

### Model summaries and utilities
* `parameter_estimates()` — enhanced extraction of standardized parameter estimates with robust SEs.
* `model_info()` — extracts compact model metadata (grouping, variables, parameterization, etc.).
* `hopper_plot()` — summarizes fit indices and modification indices across multiple models.
