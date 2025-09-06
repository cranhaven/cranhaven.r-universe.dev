# `tranSurv` 1.2.3
  	* Update maintainer email
# `tranSurv` 1.2.2
  	* Add optim() in search and update searching algorithm
	* Fixed bugs in piecewise models
	* Re-calculate the censoring survival estimates with Nelson-Aalen estimate when
          there the KM estimates drop to 0 at early times.
	* Set robust = FALSE in coxph() to avoid calculating robust variance estimator
	  when a is extreme.
# `tranSurv` 1.2.1
  	* Use grid search (rootSolve then optim) in finding a
  	* Updated piecewise model so `a` is optimized globally
  	* Added goodness of fit test
# `tranSurv` 1.2.0
	* Passed CRAN checks
	* Added regression method, `trReg()`