# OPSR (development version)

# OPSR 1.0.1

* `opsr_simulate()` now takes `seed` argument to enable reproducibility.
* `plot.opsr()` and `opsr_te()` define `type` arg list to behave similarly to `predict.opsr()`.

# OPSR 1.0.0

* `print.summary.opsr()` now shows GOF indicators.
* Added `timeuse_data` from the TimeUse+ study.
* `extract.opsr()`: R2 for each regime in `texreg` functions (and don't repeat GOFs if `beside = TRUE`).
* Nicer summary printing and setting fixed values in null model to `NA`.
* Added `.loglik` arg to `opsr()` to return log-likelihood vector.
* Detects and warns on singularity issues.
* Resolved memory leak.
* Corrected outcome-specific R2 computations.
* Added `type = "Xb"` and `type = "correction"` in `predict.opsr()`.
* Works for only 2 treatment regimes (i.e., the Tobit-5 model).
* Smoothing constant (used in continuity correction) can be passed to `predict.opsr()` via `delta` arg.
* Added `opsr_te()` for treatment effect computations.
* Added `plot()` method for treatment effect visualization.
* Improved `predict()`'s argument argument checking and error messages as well as the man pages.
* Passing the null model as sole argument to `anova()` will raise an error.
* Added vignette corresponding to the JSS submission.

# OPSR 0.1.2

* Patch with minor updates on documentation.

# OPSR 0.1.1

* Patch with missing links.

# OPSR 0.1.0

* Initial CRAN submission.
