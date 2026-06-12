# mlmodels 0.1.2

* Added return values in the documentation of exported functions that were missing them.
* Added references to implemented methods in the description.

# mlmodels 0.1.1

* Fixed weighted log-likelihood calculation in `ml_logit()` (both homoskedastic and heteroskedastic versions). 
  This bug previously caused incorrect log-likelihood, AIC, BIC, and convergence issues in weighted logit models.
* All other models were already handling weights correctly.

# mlmodels 0.1.0

* First public release - Initial CRAN submission
* Provides maximum likelihood estimation for Gaussian (linear and log-normal), logit, probit, Poisson, negative binomial (NB1 and NB2), gamma, and beta models.
* Consistent S3 interface with support for modeling scale parameters.
* Multiple variance-covariance estimators (OIM, OPG, robust, cluster-robust, bootstrap, jackknife).
* Full suite of post-estimation tools and hypothesis tests.
* Compatible with `marginaleffects` for marginal effects and predictions.
* Comprehensive vignettes covering main model families and diagnostics.