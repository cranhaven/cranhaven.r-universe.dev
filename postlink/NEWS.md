# **postlink 0.1.1**

### New Features
* Updated the `priors` argument for the Bayesian Mixture functions (`adjMixBayes()`, `glmMixBayes()`, `survregMixBayes()`) so that intercept priors and slope coefficient priors are now decoupled.

### Architecture & S3 Methods
* Refactored S3 class assignment system to remove false inheritance from base `glm`, `lm`, and `coxph` classes. Fitted objects now use dedicated package-level classes (e.g., `plmodel`, `plglm`, `plcoxph`).
* Added standard generic extractors (i.e., `coef()`, `vcov()`, `confint()`, `df.residual()`) for compatibility with tools like `lmtest::coeftest()`.
* Added custom methods to indicate unsupported standard likelihood and residual-based generics (e.g., `logLik()`, `profile()`, `anova()`, `extractAIC()`, `cooks.distance()`, `rstudent()`). 
* Consolidated shared printing behaviors for adjustment objects (`adjELE`, `adjMixture`, `adjMixBayes`) using a unified base `print.adjustment()` method via `NextMethod()`, reducing code duplication.
* Added an error message to clarify that the `gaussian` family in `glmMixture` is intended to be used only with the `identity` link for now.

### Documentation & Testing
* Standardized all Roxygen manual page titles to Title Case. Implemented a GitHub Actions workflow (`format-titles.yml`) for documentation styling in continuous integration.
* Removed `VignetteBuilder: knitr` from the `DESCRIPTION` file to resolve a CRAN NOTE, as the extended package articles are hosted only via `pkgdown` for now.
* Updated the `testthat` suite to align with the newly refactored S3 class structures and console outputs.

# **postlink 0.1.0**

* Initial CRAN release.
* Implements a suite of statistical tools (weighting or mixture modeling) for secondary analysis of linked data accounting for mismatch errors.
* Added support for adjusting generalized linear models, Cox proportional hazards models, parametric survival modeling, and contingency tables.
