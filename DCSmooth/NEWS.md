# DCSmooth 1.1.2
* Parallelization for DCS procedure added.
* Bandwidth selection of derivative estimation improved.
* Additional argument "trim" in surface.dcs for trimming the boundaries of the plot.
* Data sets "returns.alv", "returns.bmw", "volumes.alv", "volumes.bmw" added.
* Bugs in estimation of derivatives ("KR" and "LP") corrected and corresponding error model are fixed.
* Default value in IPI_options$infl_par set to (2,1) for local polynomial regression.

# DCSmooth 1.1.0
* Estimation of variance model revised.
* set.options takes argument var_model = c("iid", "sarma_HR", "sarma_sep", "sarma_RSS", "sfarima_RSS") instead of var_est. However, var_est is still available for downstream compatibility, with the old identifiers.
* The notation "QARMA" and "SARMA" has been unified to "SARMA" for all spatial ARMA models.
* Functions qarma.est(), qarma.sim() have been replaced by sarma.est() and sarma.sim(). They are still available and link to the new functions.
* Added summary and print methods for classes "sarma" and "sfarima".
* Output "var_est" of dcs() contains now the complete estimation of error terms of class "sarma" or "sfarima".
* Arguments "model_order" and "order_max" controlling the orders of the variance models are now passed to "set.options()" as additional arguments in the functions ellipsis.
* Functions "kernel.assign()" and "kernel.list()" have been added.
* New datasets "wind.nunn", "wind.yuma", "sun.nunn", "sun.yuma", "returns.alv", "returns.bmw", "returns.sie", "volumes.alv", "volumes.bmw", "volumes.sie" added.
* Test for stationarity of SARMA models fixed.

# DCSmooth 1.0.3
* New datasets "temp.nunn" and "temp.yuma" added.
* Estimation for error terms modeled after separable SARMA included.

# DCSmooth 1.0.2
* Functions for estimation and simulation of SFARIMA(p, q, d) added.

# DCSmooth 1.0.1
* Estimation for SFARIMA errors is now with ML estimation

# DCSmooth 1.0.0
First release version
* Estimation under SFARIMA errors (with `set.options(var_est = "lm")`) is only in experimental state.