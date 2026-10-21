# evbsreg 1.2.0

## Bug fixes

* `plot_aggregate_contributions()` labelled flagged observations with a fixed
  offset above each point, so labels for observations close together on the
  index axis overlapped and became unreadable. Labels are now staggered
  vertically for points that fall within a closeness threshold of each other,
  leaving isolated points unaffected.
* `envelope_qq()` saved and restored the caller's *entire* `par()` state
  (`par(no.readonly = TRUE)`), which includes the multi-panel layout position
  (`mfg`). When called as one step of a multi-panel figure set up by the
  caller (`par(mfrow = c(1, 2))` followed by `envelope_qq()` then a second
  `plot()`), restoring the saved state on exit rewound the layout position,
  so the second plot overwrote the first panel instead of advancing to the
  second, leaving one panel blank. The function now saves and restores only
  `pty`, the one parameter it actually changes.

## New features

* `evbs_monitor()` computes the prospective endpoint-identifiability index
  $F_t$, combining the conformal normal curvature with the finite upper endpoint,
  and `plot.evbs_monitor()` draws the corresponding control chart. Values below
  one indicate that the endpoint is determined by a single observation and should
  not be quoted as a design value.

# evbsreg 1.1.0

## New features

* `logEVBS()` provides a `gamlss.family` implementation of the log-EVBS
  distribution, allowing every parameter, including the tail-shape parameter,
  to depend on covariates or smooth terms. With a constant predictor it
  reproduces the fixed-parameter fit of `evbsreg.fit()`. Companion functions
  `dlogEVBS()`, `plogEVBS()` and `qlogEVBS()` are also provided.
* `devbs()`, `pevbs()` and `qevbs()` complete the distribution family. The
  package previously exported only `revbs()`, so users had no way to compute
  quantiles or return levels.
* `evbs_endpoint()` returns the finite upper endpoint of the fitted model when
  `gama < 0` (Weibull max-domain of attraction).
* `evbs_return_level()` computes return levels and expected shortfall from the
  exact EVBS quantile function.
* `gevreg.fit()`, `gev_scores()` and `cnc_diagnostics_gev()` extend the local
  influence framework to the generalized extreme-value regression model.
* `evbs_block_boot()` provides moving-block bootstrap standard errors for
  series with residual serial dependence.

## Important notes

* The extreme-value index of the EVBS *response* is `2 * gama`, not `gama`. The
  transformation preserves the max-domain of attraction but doubles the tail
  index. Documentation updated accordingly.
* The GEV return-level formula does **not** apply to the EVBS response. Use
  `qevbs()` or `evbs_return_level()`.
* `gevreg.fit()` centres are strongly recommended: the GEV likelihood is poorly
  conditioned for uncentred covariates and may silently fail to converge.

# evbsreg 1.0.0

## New Features

* **Initial CRAN Release** - Complete implementation of local influence diagnostics for Extreme-Value Birnbaum-Saunders (EVBS) regression models

* **Estimation** - `evbsreg.fit()` function for joint maximum likelihood estimation of EVBS regression models with flexible parameter specification

* **Diagnostics** - Conformal normal curvature-based local influence diagnostics under three perturbation schemes:
  - Case-weight perturbation
  - Response variable perturbation
  - Explanatory variable perturbation

* **Residuals** - Randomized quantile residuals (`rcoxsnell()`, `rqrandomized()`) with simulation envelopes for model validation

* **Visualization** - Publication-quality diagnostic and density plots:
  - `plot_cnc()` for local influence plots
  - `envelope_qq()` for quantile-quantile plots with envelopes
  - `plot_evbs_alpha()` and `plot_evbs_gama()` for parameter density visualization
  - `plot_aggregate_contributions()` for influence aggregation
  - `plot_normalized_eigenvalues()` for eigenvalue analysis

* **Monte Carlo Utilities** - `generate_evbs_data()` and `generate_logevbs_data()` for simulation studies

* **Random Number Generation** - `revbs()` for generating random variates from EVBS distributions with flexible GEV parent distributions

## Methodology

The methods implemented in this package are described in:
- Ospina, Lima, Barros, and Macedo (2026, submitted)

Application to real-world data:
- Monthly maximum wind gust data from Itajai, Brazil (included in `itajai` dataset)

## Documentation

* Complete function reference with examples
* Comprehensive vignette demonstrating workflow on real data
* CITATION file with proper attribution

## Dependencies

* **Imports**: stats, graphics, SpatialExtremes, ggplot2
* **Suggests**: gamlss, grDevices, knitr, rmarkdown, testthat

---

For more information, visit: https://raydonal.github.io/evbsreg/
