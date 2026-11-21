# CIMEHR

CIMEHR fits longitudinal outcome models to electronic health record (EHR) data
in which both the **visit process** (when a patient is seen) and the
**recording process** (whether the outcome is recorded at a visit) may be
informative. The package provides the CIMEHR estimator (Laplace approximation +
pairwise likelihood + estimating equation), its spline and higher-order Laplace
(HL) variants, and 25 comparison methods (29 methods in total), all behind one interface,
`fit_method()`. Each method is implemented in its own file in `R/`. See
`help(package = "CIMEHR")` for the reference manual.

Version 0.2.0. License: MIT (see `inst/COPYRIGHTS` for the Genz routines).

## Installation

From GitHub:

```r
# install.packages("remotes")
remotes::install_github("ysph-dsde/CIMEHR")
# add build_vignettes = TRUE to also build the tutorial (slow: it runs a
# 50-resample bootstrap for four methods)
```

From a local copy, in the directory that contains the `CIMEHR/` folder:

```sh
R CMD INSTALL CIMEHR
# or build a tarball first
R CMD build CIMEHR
R CMD INSTALL CIMEHR_0.2.0.tar.gz
```

or from R:

```r
install.packages(c("nlme", "Rcpp"))           
install.packages("path/to/CIMEHR", repos = NULL, type = "source")
```

**Dependencies.** R >= 4.1.0; `nlme` and `Rcpp` (CRAN), plus `parallel`,
`splines`, `stats`, `tools` and `utils`, which ship with R. The package contains C++ code
(the pairwise likelihoods of `pairlik` and of the CIMEHR recording stage), so
installing from source needs a C++ compiler (for example Rtools on Windows or
the Xcode command line tools on macOS). `mice` and `pan` are
suggested and are only needed for the multiple-imputation methods
(`mi_lmm`, `mi_liang`, `mi_iirr`), which stop with an error when they are not
installed. Plotting fitted mean curves additionally requires the suggested
package `ggplot2`.

## Data format

A long `data.frame`, one row per clinic visit. Column names are arguments; the
defaults are shown.

| Column | Argument (`fit_method` / `CIMEHR`) | Content |
|---|---|---|
| `id` | `id` / `id_col` | Subject identifier, never missing. |
| `time` | `time` / `time_col` | Visit time. |
| `Y` | `outcome` / `y_col` | Outcome; `NA` when not recorded. |
| `R` | `obs_indicator` / `r_col` | 1 if the outcome was recorded at the visit, else 0. |
| `C` | `censor_col` | End of follow-up (censoring time); constant within subject. |
| `E` | `entry_col` | Optional entry time; constant within subject. |
| `Z`, `X`, ... | covariate arguments | Numeric subject-level covariates, constant within subject. |

Rules checked before fitting:

- Every column used, except `id`, must be numeric. Encode factors with
  `model.matrix()` first.
- Every visit needs `R` equal to 0 or 1; a recorded visit (`R == 1`) needs a
  finite `Y`.
- A subject with no visits has exactly one placeholder row with
  `time = NA`, `Y = NA`, `R = NA` (covariates, `C` and `E` filled in).
- `C` and `E` are finite; visits lie in `[E, C]`; `E <= C`.

## Quick start with simulated data

The package includes a simulated dataset with 1,000 participants followed
for 60 months, with shared lognormal visiting
frailty, a probit recording model and a linear common outcome baseline.
`Z` is a binary exposure, and `X` is a
continuous baseline covariate. The outcome and recording residual random
coefficients are independent given the shared standard normal latent variable.
The generating parameters and seed are retained with the data.

```r
library(CIMEHR)
data("cimehr_example", package = "CIMEHR")
dat <- merge(cimehr_example$participants, cimehr_example$visits,
             by = "id", all.x = TRUE)
dat <- dat[order(dat$id, dat$time, na.last = TRUE), ]
rownames(dat) <- NULL

fit_cimehr <- fit_method("cimehr", dat, visit_covs = c("Z", "X"),
                  obs_fixed_covs = c("Z", "X"), obs_random_covs = "Z",
                  long_covs = c("Z", "X"), outcome_random_covs = "Z",
                  censor_col = "C")
summary_outcome(fit_cimehr)
summary_visiting(fit_cimehr)
summary_observation(fit_cimehr)
confint(fit_cimehr)
extract_coefficient(fit_cimehr, "Z")

fit_native <- CIMEHR(dat, covars_visit_XV = c("Z", "X"),
                      covars_outcome_fixed_XY = c("Z", "X"),
                      covars_outcome_random_link_ZY = "Z",
                      covars_obs_fixed_XO = c("Z", "X"),
                      covars_obs_random_link_ZO = "Z", censor_col = "C")

fit_liang <- fit_method("liang_visit", dat, visit_covs = c("Z", "X"),
                        long_covs = c("Z", "X"), outcome_random_covs = "Z",
                        censor_col = "C")
```

Run `vignette("cimehr-workflow", package = "CIMEHR")` for the complete
four-method tutorial, including data checks, factor encoding, trajectories,
95% confidence intervals and interpretation of diagnostics. Its directly
executable companion is
`system.file("examples", "cimehr_workflow.R", package = "CIMEHR")`.
The exact data generator is installed as
`system.file("examples", "simulate_example.R", package = "CIMEHR")`;
`?cimehr_example` gives the generating models and parameter values.

The tutorial displays the four methods' outcome estimates, stacked SEs and
95% confidence intervals, followed by baseline and population-average curves
with pointwise 95% participant-bootstrap intervals. Each curve comparison
contains four method panels with the generating truth. The corresponding PDFs
and numerical tables are installed in
`system.file("examples", "results", package = "CIMEHR")`.
The population comparison fixes `Z = 0` and `Z = 1` at equal weights and
`X = 0` for all methods. The tutorial script reproduces these results from the
bundled simulated data; no fitting settings are changed.

Main `fit_method()` arguments: `visit_covs` (visit or marker intensity model),
`long_covs` (outcome fixed effects), `outcome_random_covs` and
`obs_random_covs` (random-link covariates of the outcome and recording models),
`obs_fixed_covs` and `cimehr_visit_covs` (recording and CIMEHR visit models;
both default to `visit_covs`), `outcome_fixed_structure`,
`outcome_random_structure`, `outcome_random_intercept` and `seed`. Each method
uses only the arguments it needs. Structural columns (`id`, `time`, `outcome`,
`obs_indicator`, `censor_col`, `entry_col`) cannot be set through `controls`.
`CIMEHR()` leaves unspecified covariate sets empty, whereas `fit_method()`
fills `obs_fixed_covs` and `cimehr_visit_covs` from `visit_covs`.
For EHRJoint, `obs_fixed_covs` can differ from `visit_covs` and `long_covs`;
these recording predictors must be numeric and constant within subject.
`obs_fixed_covs = character()` fits an intercept-only recording model. The
recording design is also used in the full stacked covariance calculation.

Named wrappers call `fit_method()` for one family: `Summary_stat(stat =)`,
`LMM()`, `Pairwise_likelihood()`, `EHRJoint()`; `Lin_Ying()`, `Liang()`,
`JMVL_G()`, `IIRR()` and `IIRR_balancing()` (with
`process = "marker"` or `"visit"`); `Multiple_imputation_IP()` and
`Linear_increment_IP()` (with `ip_method = "LMM"`, `"Liang"` or `"IIRR"`).
`CIMEHR()`, `CIMEHR_HL()`, `CIMEHR_spline()` and `CIMEHR_spline_HL()` fit
`cimehr`, `cimehr_hl`, `cimehr_spline` and `cimehr_spline_hl` with native
argument names; spline fits have a `predict()` method. In
`method_comparisons()`, per-method options are given as
`controls = list(liang_visit = list(...))`.

## Stacked inference

The following methods use `inference = "stacked"` by default:

| Method keys | Uncertainty included in the outcome covariance |
|---|---|
| `lin_ying_marker`, `lin_ying_visit` | Rate estimation and the direct empirical influence of both risk-set and nearest-history profile means. |
| `liang_marker`, `liang_visit` | Rate estimation, profiled Breslow exposure, frailty moments, posterior frailty and outcome estimation, including the empirical center in profile mode. |
| `cimehr`, `cimehr_hl` | Visiting, recording and outcome stages. |
| `cimehr_spline`, `cimehr_spline_hl` | Visiting, recording and spline outcome stages. |

For example, using the data in the quick start above:

```r
fit_ly_marker <- Lin_Ying(dat, process = "marker", visit_covs = c("Z", "X"),
                          long_covs = c("Z", "X"), censor_col = "C")
fit_ly_visit <- Lin_Ying(dat, process = "visit", visit_covs = c("Z", "X"),
                         long_covs = c("Z", "X"), censor_col = "C")
fit_liang_marker <- Liang(dat, process = "marker", visit_covs = c("Z", "X"),
                          long_covs = c("Z", "X"), censor_col = "C")
fit_liang_visit <- Liang(dat, process = "visit", visit_covs = c("Z", "X"),
                         long_covs = c("Z", "X"), censor_col = "C")
fit <- fit_method("liang_visit", dat, visit_covs = c("Z", "X"),
                  long_covs = c("Z", "X"), censor_col = "C",
                  controls = list(inference = "stacked"))
summary_outcome(fit)
vcov(fit, all = TRUE)
```

## Choosing the spline (CIMEHR spline)

The baseline time trend and the trajectory contrasts of `CIMEHR_spline()` and
`CIMEHR_spline_HL()` use a basis chosen by the user:

| Argument | Meaning |
|---|---|
| `spline_type` | `"natural"` (natural cubic splines, `splines::ns`) or `"bspline"` (`splines::bs`) |
| `spline_degree` | Polynomial degree of B-splines (natural splines are cubic) |
| `baseline_df`, `trajectory_df` | Number of basis functions; interior knots at quantiles of the visit times |
| `baseline_knots`, `trajectory_knots` | Explicit interior knots (override the `df`) |
| `boundary_knots` | Boundary knots; also the time scale of random slopes in time |
| `reference_time` | Time at which trajectory contrasts are zero |

```r
fit <- CIMEHR_spline(dat, covars_visit_XV = c("Z", "X"),
                     covars_outcome_fixed_XY = c("Z", "X"),
                     covars_obs_fixed_XO = c("Z", "X"), censor_col = "C",
                     covars_outcome_random_link_ZY = "Z",
                     covars_obs_random_link_ZO = "Z",
                     spline_type = "bspline", spline_degree = 2L,
                     baseline_knots = c(15, 30, 45), boundary_knots = c(0, 60))
fit2 <- fit_method("cimehr_spline", dat, visit_covs = c("Z", "X"),
                   long_covs = c("Z", "X"), censor_col = "C",
                   obs_fixed_covs = c("Z", "X"), obs_random_covs = "Z",
                   outcome_random_covs = "Z",
                   controls = list(spline_type = "natural", baseline_df = 6L,
                                   boundary_knots = c(0, 60)))
```

## Pairwise likelihood (Chen, Ning and Cai)

`fit_method("pairlik", ...)` and `Pairwise_likelihood()` compare observations
from different subjects. The time trend is chosen with `time_formula`, a
one-sided formula in the time column (`~time` by default; for example
`~time + I(time^2)`, `~poly(time, 2)`, `~splines::ns(time, df = 3)`, or `NULL`
for none). Subject pairs are sampled (`max_subject_pairs`), and every cross
observation pair of a sampled subject pair is used. The pair computations run
in C++ (Rcpp). For a continuous outcome the pairwise likelihood identifies
`beta / sigma^2`; `beta` is reported on the outcome scale by multiplying with
the residual variance of the observed outcomes, a working scale, so
`diagnostics$beta_scale_identified` is `FALSE`.

## Methods

"Marker" methods model the process of recorded outcomes (`R == 1`); "visit"
methods model all clinic visits. `available_comparison_methods()` returns the
full registry (method key, label, estimand, variance scope, follow-up
requirements); `cimehr_methods()` returns the default set used by
`method_comparisons()`, and `cimehr_methods("all")` returns every key. Methods
are identified by key or label, never by number; a numeric `method` is an
error.

| Key | Label | File in `R/` | Description |
|---|---|---|---|
| `summary_min` | Summary-Min | `summary_min.R` | OLS of each subject's minimum recorded outcome (HC3) |
| `summary_mean` | Summary-Mean | `summary_mean.R` | OLS of each subject's mean recorded outcome (HC3) |
| `summary_median` | Summary-Median | `summary_median.R` | OLS of each subject's median recorded outcome (HC3) |
| `summary_max` | Summary-Max | `summary_max.R` | OLS of each subject's maximum recorded outcome (HC3) |
| `lmm` | LMM | `lmm.R` | Linear mixed model (`nlme`) on recorded outcomes |
| `oa_lmm` | OA-LMM | `oa_lmm.R` | LMM adjusted for the cumulative number of recorded outcomes |
| `va_lmm` | VA-LMM | `va_lmm.R` | LMM adjusted for the cumulative number of visits |
| `lin_ying_marker` | Lin-Ying (marker) | `lin_ying_marker.R` | Lin-Ying estimating equation, marker process |
| `liang_marker` | Liang (marker) | `liang_marker.R` | Liang joint model with gamma frailty, marker process |
| `pairlik` | PairLik | `pairlik.R` | Pairwise likelihood (Chen, Ning and Cai) |
| `jmvl_g_marker` | JMVL-G (marker) | `jmvl_g_marker.R` | Parametric joint model with Gaussian random effects, marker process |
| `iirr_weighting_marker` | IIRR-Weighting (marker) | `iirr_weighting_marker.R` | Inverse intensity rate ratio weighting, marker process |
| `iirr_balancing_marker` | IIRR-Balancing (marker) | `iirr_balancing_marker.R` | IIRR entropy balancing, marker process |
| `mi_lmm` | MI-LMM | `mi_lmm.R` | Multiple imputation + LMM (needs `mice`, `pan`) |
| `mi_liang` | MI-Liang | `mi_liang.R` | Multiple imputation + Liang (needs `mice`, `pan`) |
| `mi_iirr` | MI-IIRR | `mi_iirr.R` | Multiple imputation + IIRR (needs `mice`, `pan`; common follow-up) |
| `li_lmm` | LI-LMM | `li_lmm.R` | Linear-increments reconstruction + LMM |
| `li_liang` | LI-Liang | `li_liang.R` | Linear-increments reconstruction + Liang |
| `li_iirr` | LI-IIRR | `li_iirr.R` | Linear-increments reconstruction + IIRR (common follow-up) |
| `ehrjoint` | EHRJoint | `ehrjoint.R` | EHRJoint (Du, Shi and Mukherjee) |
| `iirr_weighting_visit` | IIRR-Weighting (visit) | `iirr_weighting_visit.R` | IIRR weighting, visit process |
| `cimehr_spline` | CIMEHR spline | `cimehr_spline.R` | CIMEHR with spline baseline and trajectories |
| `lin_ying_visit` | Lin-Ying (visit) | `lin_ying_visit.R` | Lin-Ying, visit process |
| `liang_visit` | Liang (visit) | `liang_visit.R` | Liang, visit process |
| `jmvl_g_visit` | JMVL-G (visit) | `jmvl_g_visit.R` | JMVL-G, visit process |
| `iirr_balancing_visit` | IIRR-Balancing (visit) | `iirr_balancing_visit.R` | IIRR balancing, visit process |
| `cimehr` | CIMEHR | `cimehr.R` | Laplace approximation + pairwise likelihood + estimating equation |
| `cimehr_hl` | CIMEHR-HL | `cimehr_hl.R` | CIMEHR with higher-order Laplace posterior moments |
| `cimehr_spline_hl` | Spline CIMEHR-HL | `cimehr_spline_hl.R` | Spline CIMEHR with higher-order Laplace moments |

Each method lives in `R/<key>.R`; file names carry no numbers. A named wrapper
is defined in the file of a method it fits (for example `Liang()` in
`liang_marker.R`). Shared code lives in `registry.R`, `dispatch.R`,
`input_data.R`, `native_adapter.R`, `stages.R`, `stages_spline.R`,
`outcome_summary.R`, `cimehr_shared.R`, `cimehr_hl_moments.R` and
`cimehr_hl.R` (which is also the method file of `cimehr_hl` and holds
`CIMEHR_HL()`).

## Results

Every fit is a list of class `cimehr_fit` (spline fits add `cimehr_spline_fit`,
HL fits add `cimehr_hl_fit`). Main components:

| Component | Content |
|---|---|
| `coefficients` | Named estimates. For `cimehr` and `cimehr_hl` this holds all stages; for `cimehr_spline` and `cimehr_spline_hl` it includes the baseline spline coefficients. |
| `se` | Standard errors of `coefficients`. |
| `vcov` | Covariance of the outcome-stage estimates. |
| `vcov_all` | Joint all-stage covariance of the CIMEHR methods. `vcov(fit, all = TRUE)` returns the all-stage covariance of any method that provides one. |
| `parameter_table` | Outcome-stage table (as printed): `parameter`, `stage`, `estimate`, `se`, `df`, `lower`, `upper`, `point_valid`, `ci_available`, `inference_valid`, `inference_status`, `identification_status`, `identification_warning`, `boundary`, `variance_type`. |
| `variance_type`, `variance_label` | Type and description of the reported variance. |
| `diagnostics` | Convergence and validity flags, counts, `elapsed_seconds`, captured `warnings`, `package_version`, `n_visits_at_entry_excluded`. CIMEHR identification and numerical warnings are also signaled once to the caller and shown by `print()`; internal refits retain the diagnostics without repeatedly signaling them. |
| `method_key`, `method`, `estimand`, `assumptions`, `call` | Identification of the fit. |
| `fit` / `nuisance` | Native fit object or nuisance estimates (method-specific). |
| `refit` | For the nine methods supporting mean curves, stored input data and evaluated fitting settings used by the optional full patient bootstrap. |

Accessors: `coef()`, `vcov()`, `confint()`, `summary()` and `print()`;
`summary_outcome()`, `summary_visiting()` and `summary_observation()` return
stage tables (empty when a method has no such stage); `coef_stage()` returns
one stage's estimates; `extract_coefficient(fit, term)` returns one row and
accepts `"Z"` for both `Z` and `beta.Z`. The main `lower`/`upper` limits retain
all checked available intervals. Separate `regular_lower`/`regular_upper` and
`conditional_lower`/`conditional_upper` columns preserve the inference scope.
A boundary marker alone does not suppress a computable outcome interval or
turn its finite SE into a calculation failure. Nonfinite or unidentified
parameter covariances remain unavailable. In the selected-face sandwich, a
fixed boundary variance has no computed influence: its missing SE is not a
positive variance suppressed by a reporting flag and is not replaced by zero.
The tables distinguish `parameter_boundary` from `fit_boundary`, independently
of fitting success. `se_status` describes whether an SE is available, inference
was disabled, a boundary coordinate was not computed, or a numerical or
identification problem was established. Older or incomplete objects with no
evidence for the cause use `unknown`; a boundary flag alone is not evidence of
a numerical failure. `ci_status` distinguishes regular and conditional intervals
from unavailable or uncalibrated inference.

For a fitted zero variance with a finite supplied SE but no supported boundary
reference distribution, the SE remains visible and default `lower`/`upper`
remain unavailable. Use the following only to inspect arithmetic Wald limits:

```r
summary_observation(fit, numerical_wald = TRUE)
summary_visiting(fit, numerical_wald = TRUE)
```

This explicit request adds `numerical_lower`, `numerical_upper`,
`numerical_wald_available`, and `numerical_wald_scope`. It does not change the
default interval columns or validity flags. These numerical limits are not
calibrated boundary confidence intervals. Missing SEs are not imputed, and
negative covariance diagonals are not converted into zero SEs by reporting
fallbacks.

Formal inference for a boundary variance requires a separately justified
method. The current package does not implement a calibrated constrained
composite-profile test or confidence interval. Ordinary participant bootstrap
is not automatically a solution for boundary variance inference. The existing
trajectory bootstrap and its defaults are unchanged.

## Fitted baseline and population mean curves

Use `fit_method()` to fit the model, `meanfit()` to construct a curve object, and `summary()` or `plot()` to inspect it. This follows the model → curve → plot workflow in the [official `survival::survfit.coxph` documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/survival/html/survfit.coxph.html). Here the target is a longitudinal outcome mean, under the assumptions of the fitted longitudinal method.

The interface supports CIMEHR, CIMEHR-HL, CIMEHR-spline, CIMEHR-spline-HL, Liang-V, Liang-M, MI+Liang, LI+Liang, and EHRJoint. Each uses the same post-estimation commands:

```r
# dat is constructed from cimehr_example in the quick start above.
keys <- c("cimehr", "liang_visit", "ehrjoint")
fits <- setNames(lapply(keys, function(key)
  fit_method(key, dat, visit_covs = c("Z", "X"),
             long_covs = c("Z", "X"), obs_fixed_covs = c("Z", "X"),
             outcome_random_covs = "Z", obs_random_covs = "Z",
             censor_col = "C")), keys)

profiles <- data.frame(Z = c(0, 1), X = c(0, 0))
curves <- lapply(fits, meanfit, newdata = profiles)
summary(curves[["cimehr"]])
p <- plot(curves[["cimehr"]])
ggplot2::ggsave("CIMEHR_population_trajectories.pdf", p,
                width = 7, height = 4)

baseline <- meanfit(fits[["cimehr"]], type = "baseline")
population <- meanfit(fits[["cimehr"]])
standardized <- meanfit(fits[["cimehr"]], newdata = profiles,
                        aggregate = TRUE, weights = c(.4, .6))
as.data.frame(standardized)
```

With supplied `newdata`, `meanfit()` returns a separate curve for each row by default. Set `aggregate = TRUE` to average over those profiles; `weights` specifies their mixture and is normalized to sum to one. With `newdata = NULL`, the default averages one stored profile per subject in the population retained by that method. LI reconstruction eligibility can change this population. Supply the same profiles and weights across methods when the comparison should use a common population. Predictions average the random effects to their population mean and remain on the modeled outcome scale, including the log scale for a log-transformed outcome.

`type = "baseline"` returns the common baseline mean, beta₀(t). For ordinary CIMEHR and EHRJoint, recovery smooths compensated baseline increments against visiting-baseline increments. The cumulative numerator A(t) = ∫ beta₀(u) dΛ₀(u) is an intermediate quantity. Liang uses its own stored outcome-event normalization or fitted parametric time basis; MI+Liang averages the completed-data baseline curves. CIMEHR spline methods evaluate their fitted baseline spline directly.

`times` uses the fitted time units and must lie within the stored follow-up domain; the default is 101 equally spaced points. For increment-based recovery, `bandwidth` is in those same units and defaults to 20% of the domain width. For example, a bandwidth of `1` means one year when the model was fitted in years. A fitted spline or parametric time basis is evaluated directly. Local recovery can return `NA` where the window contains too few distinct usable event times or its local design is singular. CIMEHR evaluates declared time interactions using the stored design; explicit time-dependent profiles can be supplied through `newdata = function(t) ...` when required by the fitted covariate paths.

For analytical curve intervals, use `se.fit = TRUE`. CIMEHR spline methods use the joint covariance of baseline-spline and outcome coefficients, including their covariance, to form pointwise Wald intervals. Averaged curves use the weighted prediction design before the variance calculation. Without a supported curve covariance, requesting analytical intervals raises an error because coefficient SEs alone omit baseline-recovery uncertainty.

### Optional patient bootstrap

`meanfit()` performs no bootstrap by default. Set `bootstrap = TRUE` to obtain curve SEs and pointwise Wald intervals for any of the nine supported methods. The default is `B = 50L`; increase it, for example to `200L`, to reduce Monte Carlo variability in the bootstrap SEs. `conf.int` sets the confidence level and defaults to `.95`.

```r
boot50 <- meanfit(fits[["cimehr"]], newdata = profiles,
                  bootstrap = TRUE, B = 50L, seed = 2026)
summary(boot50)
p <- plot(boot50)                  
plot(boot50, conf.int = FALSE)      

boot200 <- meanfit(fits[["cimehr"]], type = "baseline",
                   bootstrap = TRUE, B = 200L, seed = 2026,
                   conf.int = .90)
```

Each sample resamples whole subjects, including those without visits, and assigns fresh identifiers to repeated copies. All stages and any MI or LI operations are refitted from the stored input data. Supplied model controls are retained; cached nuisance fits are discarded, and automatic data-dependent defaults are recalculated. The original prediction grid and smoothing bandwidth are held fixed. Bootstrap inference takes precedence over `se.fit` when both are requested.

At each curve point, the SE is the sample SD of the finite bootstrap estimates. It is **not divided by the square root of B**. The interval is the original fitted value plus or minus `qnorm((1 + conf.int) / 2)` times that SD. Failed fits and unavailable curve points produce warnings and recorded diagnostics; fresh samples do not replace failed attempts. `summary(boot50)` reports `n_boot`, the finite replicate count for each point. Points with fewer than two finite estimates retain the original fitted value but have missing SEs and limits. `boot50$bootstrap` stores the replicate matrix, finite counts, fitting-success count, and failure reasons and stages.

With `newdata = NULL` and `aggregate = TRUE`, each bootstrap curve averages over that refit's retained reference population. With `aggregate = FALSE`, the original reference profiles are held fixed. Explicit profiles or a `newdata` function are also held fixed across samples. For weighted standardization, supply explicit `newdata` and `weights`; bootstrap requests combining `newdata = NULL` with supplied weights are rejected.

`workers = 1L` is the default. Non-Windows systems can use more workers; Windows falls back to sequential execution with a warning. Fits retain the input data and evaluated settings in `fit$refit` for this option. Older fits without that component must be refitted before requesting bootstrap curves. Direct `plot(fit, bootstrap = TRUE, B = 50L)` is a convenience wrapper; use `meanfit()` first to retain and inspect the curve diagnostics. Once a curve object is constructed, plotting never repeats its bootstrap and returns a ggplot object for customization or `ggsave()`.

For data frames directly, use `baseline_mean()` or `population_trajectory()`. Their help pages state their aggregation defaults. Existing spline-specific `predict()` behavior is retained; `meanfit()` provides the common curve interface across methods. Fits saved before prediction metadata was introduced may require refitting with the updated package.
