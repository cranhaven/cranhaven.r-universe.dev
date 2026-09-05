# IntegMultiReg 0.1.1

* Fixed undefined behavior in the MRF theta summary when a platform occurs in
  only one availability subgroup (zero pairwise theta parameters).

# IntegMultiReg 0.1.0

* First release of the integrative multi-regression (IMR) model of Chekouo et
  al. (2017), extended to continuous (Gaussian) and binary (probit) outcomes in
  addition to right-censored survival.
* `imr()` fits the model and returns an object of class `"imr"` with `print()`,
  `summary()`, `coef()`, `plot()` and `predict()` methods.
* `cv_imr()` evaluates predictive accuracy by repeated fold splits using the
  fitted MCMC samples (AUC, concordance index or mean squared error depending on
  the outcome type), and `predict()` predicts new subjects (routing each to its
  availability subgroup; `platform_names` defaults to the training order).
* `plot()` shows the inclusion-probability and MRF-interaction heatmaps and the
  log-posterior trace; `plot_top_features()` and `plot_subgroup_sizes()` add a
  ranked-biomarker bar chart and a subgroup-size bar chart.
* Ships a simulated example data set, `simIMR`, and a getting-started vignette.
* All native routines are registered, GSL errors no longer abort the R session,
  and the MCMC sampler is quiet by default (use `verbose = TRUE` for progress).
* The sampler's seed defaults to `NULL` and is then drawn from R's RNG, so runs
  follow `set.seed()` like other modelling functions.
