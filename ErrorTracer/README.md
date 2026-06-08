# ErrorTracer: Bayesian Error Propagation and Forecast Uncertainty Decomposition

## Overview

**ErrorTracer** provides a complete pipeline for ecological and genomic forecasting with climate or environmental covariates. It bridges the gap between regularized regression and fully Bayesian workflows in three steps:

1. **Extract informed priors** from a fitted regularized or standard model (`glmnet`, `lm`, `glm`, `ranger`) and carry those estimates forward as prior means into a Bayesian model — rather than discarding them.
2. **Decompose forecast uncertainty** into three independent sources — parameter uncertainty, environmental/covariate measurement uncertainty, and residual process variance — plus a fourth temporal-autocorrelation component whenever the model formula carries an `ar()`, `ma()`, `arma()`, `cosy()`, `unstr()`, `sar()`, or `car()` term.
3. **Quantify forecast shelf life** — the time horizon at which a forecast's credible interval becomes wider than the biologically plausible response range, rendering it uninformative. This concept has no equivalent in any existing R package.

The package is designed for ecological and genomic time-series forecasting in which a regularized (or plain) regression model is refit as a Bayesian model and predictions must be accompanied by a principled uncertainty budget. The API is fully general — response, predictors, and grouping are formula-driven.

---

## Key Features

| Feature | Description |
|---|---|
| **Regularized → Bayesian prior pipeline** | `extract_priors()` converts `glmnet`, `lm`, `glm`, or `ranger` coefficients into `brms`-compatible priors |
| **Uncertainty decomposition** | Partitions forecast variance into parameter / environmental / residual components, with a fourth temporal-autocorrelation component added automatically when the model formula contains `ar()`, `ma()`, `arma()`, `cosy()`, `unstr()`, `sar()`, or `car()` |
| **Forecast shelf life** | Quantifies the exact time point at which a forecast becomes uninformative |
| **Group-level fitting** | One function call fits and predicts across all groups (e.g., SNP clusters, species, sites) |
| **Calibration assessment** | Observed vs. nominal coverage probability at multiple CI levels |
| **Rich visualizations** | Six `ggplot2`-based plotting functions, all returning customizable objects |
| **Full S3 class system** | Consistent `print`, `summary`, and `plot`-family methods across all objects |

---

## Installation

Install the development version from GitHub using `remotes`:

```r
# install.packages("remotes")
remotes::install_github("madrigalrocalj/ErrorTracer")
```

ErrorTracer requires a Stan backend. Install either `cmdstanr` (recommended) or `rstan` separately:

```r
# Option A: cmdstanr (recommended)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()

# Option B: rstan
install.packages("rstan")
```

---

## Core Workflow

### Step 1 — Extract informed priors from a regularized model

`extract_priors()` is an S3 generic that dispatches on the class of the fitted model. Coefficient estimates (or importance weights for `ranger`) are used as prior means; prior SDs are proportional to coefficient magnitude or standard error.

```r
library(ErrorTracer)
library(glmnet)

# --- Elastic net (cv.glmnet) ---
cv_fit <- cv.glmnet(x_train, y_train, alpha = 0.5)

prior_spec <- extract_priors(
  model              = cv_fit,
  lambda             = "lambda.min",  # or "lambda.1se"
  multiplier         = 2.0,           # prior SD = multiplier * |coef|
  min_sd             = 0.1,           # floor for prior SD
  intercept_prior_sd = 2.5,           # intercept prior SD
  sigma_prior_scale  = 1.0            # half-Cauchy scale for sigma
)

print(prior_spec)
# ErrorTracer prior specification
#   Method      : glmnet
#   Predictors  : 5
#   Multiplier  : 2
#   Min SD      : 0.1
#   Coefficients:
#     Tmean                 mean =   0.3241  sd =   0.6482
#     PPT                   mean =  -0.1087  sd =   0.2174
#     ...
```

**Supported model classes and their prior construction:**

| Class | Prior mean | Prior SD |
|---|---|---|
| `cv.glmnet` / `glmnet` | Non-zero coefficients | `multiplier × |coef|` (floor: `min_sd`) |
| `lm` | Coefficient estimates | `multiplier × SE` (floor: `min_sd`) |
| `glm` | Coefficient estimates | `multiplier × SE` (floor: `min_sd`) |
| `ranger` | Zero (direction unknown) | `multiplier × normalised_importance` |

For `ranger`, only variables with positive permutation importance are included. Because random forests do not produce signed coefficients, priors are centered at zero and scaled by importance — encoding "this predictor matters" without a directional claim.

> **Note on centering:** The glmnet intercept is intentionally excluded from the prior — it reflects the centering of training data, which may not apply to new data. Center predictors upstream and document this in your analysis.

---

### Step 2 — Fit the Bayesian model

`et_fit()` wraps `brms::brm()` and attaches the prior specification and training data for downstream use.

```r
fit <- et_fit(
  formula       = z_diff ~ Tmean + PPT + SWE,
  data          = train_df,
  priors        = prior_spec,       # from extract_priors(); or NULL for brms defaults
  chains        = 4L,
  iter          = 2000L,
  warmup        = 1000L,
  cores         = 4L,
  seed          = 42L,
  adapt_delta   = 0.95,
  max_treedepth = 12L
)

print(fit)
# ErrorTracer model (et_model)
#   Formula : z_diff ~ Tmean + PPT + SWE
#   n obs   : 48
#   Chains  : 4  Iter: 2000  Warmup: 1000
#   Priors  : informed (glmnet, 3 predictors)
#   Rhat max: 1.002

summary(fit)
```

#### Group-level fitting

Pass a `grouping` column name to fit one model per group and receive an `et_model_list`:

```r
# Fits one brms model per SNP cluster
fit_list <- et_fit(
  formula  = z_diff ~ Tmean + PPT + SWE,
  data     = all_clusters_df,
  priors   = prior_spec,        # single spec applied to all groups
  grouping = "cluster_id",      # or pass a named list of et_prior_spec for per-group priors
  chains   = 4L,
  iter     = 2000L
)

print(fit_list)
# ErrorTracer grouped model list (et_model_list)
#   Grouping : cluster_id
#   Formula  : z_diff ~ Tmean + PPT + SWE
#   Groups   : 12
#   Fitted   : 12 / 12
```

---

### Step 3 — Predict with uncertainty propagation

`et_predict()` draws from the posterior predictive distribution, propagates environmental measurement noise through the model, and computes credible intervals.

```r
pred <- et_predict(
  model     = fit,
  newdata   = forecast_df,
  env_noise = list(Tmean = 0.5, PPT = 10.0, SWE = 5.0),
  # env_noise can also be a single fraction of each predictor's SD:
  # env_noise = 0.10   →  10% noise applied uniformly
  n_draws   = 2000L,
  ci_levels = c(0.5, 0.8, 0.90, 0.95),
  n_perturb = 500L    # draws used in the perturbation step; reduce for speed
)

print(pred)
# ErrorTracer prediction (et_prediction)
#   Observations : 24
#   Draws        : 2000
#   CI levels    : 0.5, 0.8, 0.9, 0.95
#   Mean var decomposition (across observations):
#     Parameter    : 0.0412
#     Environmental: 0.0083
#     Residual     : 0.1204
#     Total        : 0.1619
```

The returned `et_prediction` object contains:

| Slot | Description |
|---|---|
| `posterior_predict` | `[n_draws × n_obs]` matrix — full posterior predictive draws |
| `posterior_linpred` | `[n_draws × n_obs]` matrix — linear predictor draws (parameter uncertainty only) |
| `lp_perturbed` | `[n_perturb × n_obs]` matrix — linear predictor under environmentally perturbed inputs |
| `sigma_draws` | Numeric vector of posterior sigma draws |
| `credible_intervals` | `data.frame` with columns `row_id, ci_level, lower, median, upper, width` |
| `decomposition` | `data.frame` with `obs_id, param_var, env_var, residual_var, total_var` (plus `temporal_var` when the model carries an autocorrelation term) |
| `newdata` | The input forecast data |
| `model` | Reference to the source `et_model` |

---

### Step 4 — Decompose uncertainty

`decompose_uncertainty()` extracts the variance decomposition table from a prediction object:

```r
decomp <- decompose_uncertainty(pred)
head(decomp)
#   obs_id param_var  env_var residual_var total_var
# 1      1    0.0389  0.0071       0.1204    0.1601
# 2      2    0.0441  0.0098       0.1204    0.1637
# ...
```

**Decomposition method:**

| Component | Estimator |
|---|---|
| `param_var` | `Var[posterior_linpred]` across draws — coefficient uncertainty |
| `env_var` | `Var[lp_perturbed] − Var[lp_unperturbed]` — additional variance from predictor noise |
| `residual_var` | `E[σ²]` across posterior draws — biological process noise |
| `total_var` | `Var[posterior_predict]` — full predictive variance |

All components are guaranteed non-negative. Using the posterior mean of σ² (not the median) ensures `param_var + residual_var ≈ total_var` by the law of total variance.

For models whose formula carries an autocorrelation term (`ar()`, `ma()`, `arma()`, `cosy()`, `unstr()`, `sar()`, `car()`), `decompose_uncertainty()` adds a fourth column `temporal_var`, defined as `pmax(0, total_var − (param_var + env_var + residual_var))`. It captures the autocorrelation-induced spread that `brms::posterior_predict()` accumulates iteratively beyond a single innovation, and `residual_var` is then to be read as the *innovation* variance rather than the stationary marginal variance. The four components reconstruct `total_var` modulo Monte Carlo error.

---

### Step 5 — Forecast shelf life

`shelf_life()` computes the ratio of credible interval width to the plausible response range at each forecast time point, and flags when the forecast becomes uninformative:

```r
# For arcsin-sqrt transformed responses, derive range from training data:
#   plausible_range = range(train_df$z_diff)
# For bounded responses, supply explicit bounds:
#   plausible_range = c(lower, upper)
sl <- shelf_life(
  predictions              = pred,
  plausible_range          = range(train_df$z_diff),
  ci_level                 = 0.90,   # must be present in the et_prediction object
  threshold                = 1.0,    # CI/range ratio above which forecast is uninformative
  time_col                 = "year", # column in newdata to use as time axis
  max_extrapolation_factor = 10      # cap on linear projection beyond observed window
)

print(sl)
# ErrorTracer shelf life analysis
#   Observations    : 24
#   Plausible range : 2
#   Informative     : 18 / 24
#   Mean CI/range   : 0.847
#   Max CI/range    : 1.231

as.data.frame(sl)
#   obs_id  time ci_width plausible_range  ratio informative
# 1      1  2024    0.841               2  0.421        TRUE
# 2      2  2025    1.103               2  0.552        TRUE
# 3      3  2026    2.461               2  1.231       FALSE
```

A `threshold` of 1.0 (default) means the forecast is considered uninformative when the CI spans the entire plausible response range. Lower thresholds (e.g., 0.8) impose stricter requirements.

---

### Step 6 — Calibrate against validation data

`et_calibrate()` computes observed coverage probability at each nominal CI level. A well-calibrated model produces coverage that matches the nominal level:

```r
cal <- et_calibrate(
  predictions  = pred,
  observed     = validation_df,  # same n_rows as newdata; matched positionally
  response_col = "z_diff",       # inferred from formula if omitted
  ci_levels    = c(0.5, 0.8, 0.90, 0.95)
)

print(cal)
#   ci_level nominal observed_coverage n_obs calibration_error
# 1     0.50    0.50             0.542    24             0.042
# 2     0.80    0.80             0.792    24             0.008
# 3     0.90    0.90             0.875    24             0.025
# 4     0.95    0.95             0.958    24             0.008
```

---

### Step 7 — Diagnose convergence

`et_diagnose()` reports Rhat, effective sample size ratios, divergent transitions, and LOO-CV in a single call:

```r
diag <- et_diagnose(fit, loo = TRUE)

# Convergence
diag$convergence$rhat_max     # should be < 1.05
diag$convergence$neff_all_ok  # all Neff ratios > 0.1
diag$convergence$n_divergences

# LOO-CV
diag$loo$elpd_loo
diag$loo$n_bad_pareto_k  # Pareto k > 0.7 indicate influential observations
```

For grouped models, `et_diagnose()` returns a `per_group` list and a `summary` data.frame with one row per group.

---

## Visualization

All plotting functions return `ggplot2` objects that can be further customized with standard `ggplot2` additions (`+ theme(...)`, `+ labs(...)`, etc.).

```r
# Stacked bar: proportional contribution of each variance component
et_plot_decomposition(decomp, proportional = TRUE)

# Line chart: CI width / plausible range over time, with uninformative threshold
et_plot_shelf_life(sl, show_ratio = TRUE)

# Calibration: observed vs. nominal coverage (points on the 1:1 diagonal = well-calibrated)
et_plot_calibration(cal)

# Fan chart: nested CI ribbons with optional observed values overlaid
et_plot_forecast(pred, observed = validation_df,
                 response_col = "z_diff", time_col = "year")

# Density overlay: prior vs. posterior for each regression coefficient
et_plot_prior_posterior(fit, max_preds = 8L)

# Forest plot: Bayesian 95% CI (blue) vs. regularized coefficient (red ×)
et_plot_coefficients(fit)
```

---

## S3 Class System

| Class | Created by | Methods |
|---|---|---|
| `et_prior_spec` | `extract_priors()` | `print` |
| `et_model` | `et_fit()` | `print`, `summary` |
| `et_model_list` | `et_fit(..., grouping = ...)` | `print`, `summary` |
| `et_prediction` | `et_predict()` | `print` |
| `et_prediction_list` | `et_predict()` on `et_model_list` | `print` |
| `et_shelf_life` | `shelf_life()` | `print`, `summary` |

---

## Full Example: Allele Frequency Forecasting

```r
library(ErrorTracer)
library(glmnet)

# --- Prepare data ---
# train_df: historical years with columns z_diff, Tmean, PPT, SWE
# forecast_df: future years with predictor values only

# 1. Fit elastic net
x_mat <- as.matrix(train_df[, c("Tmean", "PPT", "SWE")])
cv_fit <- cv.glmnet(x_mat, train_df$z_diff, alpha = 0.5)

# 2. Extract informed priors
prior_spec <- extract_priors(cv_fit, multiplier = 2.0, min_sd = 0.1)

# 3. Fit Bayesian model
fit <- et_fit(
  formula = z_diff ~ Tmean + PPT + SWE,
  data    = train_df,
  priors  = prior_spec,
  chains  = 4L, iter = 2000L, cores = 4L
)

# 4. Predict with environmental noise propagation
pred <- et_predict(
  model     = fit,
  newdata   = forecast_df,
  env_noise = list(Tmean = 0.5, PPT = 10.0, SWE = 5.0),
  ci_levels = c(0.5, 0.80, 0.90, 0.95)
)

# 5. Decompose uncertainty
decomp <- decompose_uncertainty(pred)

# 6. Assess forecast shelf life
# For arcsin-sqrt responses use the observed training range; for bounded
# responses supply explicit bounds.
sl <- shelf_life(
  pred,
  plausible_range          = range(train_df$z_diff),
  ci_level                 = 0.90,
  threshold                = 1.0,
  time_col                 = "year",
  max_extrapolation_factor = 10
)

# 7. Calibrate (if validation data available)
cal <- et_calibrate(pred, observed = valid_df, response_col = "z_diff")

# 8. Diagnose
et_diagnose(fit)

# 9. Visualize
library(ggplot2)
et_plot_decomposition(decomp)
et_plot_shelf_life(sl)
et_plot_calibration(cal)
et_plot_forecast(pred, observed = valid_df,
                 response_col = "z_diff", time_col = "year")
et_plot_prior_posterior(fit)
et_plot_coefficients(fit)
```

---

## Group-Level Analysis Example

```r
# Fit one model per SNP cluster, then predict and summarize shelf life by cluster
fit_list <- et_fit(
  formula  = z_diff ~ Tmean + PPT + SWE,
  data     = all_df,
  priors   = prior_spec,
  grouping = "cluster_id",
  chains = 4L, iter = 2000L, cores = 4L
)

pred_list <- et_predict(
  fit_list,
  newdata   = forecast_df,
  env_noise = list(Tmean = 0.5, PPT = 10.0),
  ci_levels = c(0.50, 0.90, 0.95)
)

# Per-group decomposition and shelf life
decomp_all <- decompose_uncertainty(pred_list)
sl_all     <- shelf_life(pred_list,
                         plausible_range          = range(train_df$z_diff),
                         ci_level                 = 0.90,
                         time_col                 = "year",
                         max_extrapolation_factor = 10)

# Per-group diagnostics summary
diag_list  <- et_diagnose(fit_list)
diag_list$summary  # data.frame: rhat_ok, neff_ok, n_divergences, elpd_loo per cluster

# Faceted shelf life plot
et_plot_shelf_life(sl_all) +
  ggplot2::facet_wrap(~ group, ncol = 4)
```

---

## Comparison with Existing R Packages

| Package | What it does | What is missing |
|---|---|---|
| `brms` | Bayesian regression | No shelf life, no decomposition, no enet→prior pipeline |
| `propagate` | Analytical error propagation | No posterior sampling, no Bayesian framework |
| `rstanarm` | Bayesian regression | Same gaps as `brms` |
| `forecast` / `fable` | Time series forecasting | No ecological/genomic data structures, no decomposition |
| `INLA` | Spatial/temporal Bayes | No shelf life concept, very different user base |

**ErrorTracer's niche:** the full pipeline from regularized feature selection → informed Bayesian refitting → structured uncertainty decomposition → forecast shelf life, designed for ecology and genomics.

---

## Package Structure

```
ErrorTracer/
├── R/
│   ├── priors.R      # extract_priors() — dispatch for glmnet, lm, glm, ranger
│   ├── fit.R         # et_fit() — Bayesian model fitting via brms
│   ├── predict.R     # et_predict() — posterior prediction with decomposition
│   ├── decompose.R   # decompose_uncertainty() — param / env / residual split
│   ├── shelf_life.R  # shelf_life() — forecast horizon analysis
│   ├── calibrate.R   # et_calibrate(), et_diagnose()
│   ├── plot.R        # et_plot_*() — six ggplot2 visualizations
│   └── utils.R       # internal helpers (standardize, logging, etc.)
├── tests/
│   └── testthat/
├── vignettes/
│   ├── genomics.Rmd  # Drosophila allele frequency example
│   └── ecology.Rmd   # Phenology / species abundance example
├── DESCRIPTION
├── NAMESPACE
```

---

## Dependencies

**Imports** (installed automatically):

| Package | Version | Purpose |
|---|---|---|
| `brms` | >= 2.20.0 | Bayesian regression via Stan |
| `loo` | >= 2.6.0 | LOO cross-validation |
| `bayesplot` | >= 1.10.0 | Posterior predictive checks |
| `ggplot2` | >= 3.4.0 | Visualization |
| `tidyr` | >= 1.3.0 | Data reshaping |
| `rlang` | >= 1.1.0 | Tidy evaluation |

**Suggests** (install separately as needed):

| Package | Purpose |
|---|---|
| `glmnet` | Elastic net / lasso prior extraction |
| `ranger` | Random forest prior extraction |
| `dplyr` | Data manipulation in vignettes |
| `testthat` | Unit testing |
| `knitr`, `rmarkdown` | Vignette building |

**Backend** (required; install separately):
- `cmdstanr` (recommended) or `rstan`

---

## Caveats and Limitations

- **Small-n regime:** When `n < 10` observations, informed priors from `extract_priors()` dominate the posterior. This is the setting where the enet→prior pipeline provides the most value, but is also most sensitive to prior misspecification. Document prior choices carefully.
- **Intercept handling:** The glmnet intercept is excluded from the prior by default, as it reflects upstream centering of training data. If your data is not pre-centered, verify that the intercept prior (default `Normal(0, 2.5)`) is appropriate.
- **Environmental uncertainty is additive:** `env_var` is estimated via perturbation and is reported separately from `total_var`, which is computed from the full posterior predictive (parameter + residual only). Interpret the decomposition table accordingly.
- **`ranger` priors are undirected:** Because random forests provide no signed coefficients, ranger-derived priors are centered at zero. This is conservative but avoids false directional assumptions.

---

## License

MIT License. See `LICENSE` for details.

---

## Authors

- **Luis Javier Madrigal-Roca** (Creator, Maintainer) — madrigalrocalj@yahoo.com
- **John Kelly** (Author) — jkk@ku.edu
