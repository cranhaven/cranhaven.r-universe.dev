# funcharts 1.8.0

* `FMRCC_PhaseI()` performs Phase I of the Functional Mixture Regression Control Chart (FMRCC) of Capezza et al. (2025).
* `FMRCC_PhaseII()` performs Phase II of the FMRCC of Capezza et al. (2025).
* `simulate_data_fmrcc()` simulates functional data under the FMRCC model.
* `estimate_mixture()` fits Gaussian mixture regression models with multiple covariance parametrizations.
* Added `plot()` and `lines()` methods for `mfd` objects.
* Added `abline_mfd()` function to add lines to every panel in `mfd` plots.
* Added functions that allow to do standard mathematics with `mfd` objects: `+`, `-`, `*`.
* Added `predict()` method for `pca_mfd` objects, it predicts scores or reconstructs curves for new data.
* `simulate_data_RoMFCC()` simulates data for the robust multivariate functional control chart of Capezza et al. (2024).

References:

* Capezza, C., Centofanti, F., Forcina, D., Lepore, A., and Palumbo, B. (2025) 
  Functional Mixture Regression Control Chart. Accepted for publication in \emph{Annals of Applied Statistics}.
* Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024) Robust Multivariate Functional Control Chart. 
\emph{Technometrics}, 66(4):531--547, doi:10.1080/00401706.2024.2327346.


# funcharts 1.7.0

* `AMFCC_PhaseI()` performs Phase I of the Adaptive Multivariate Functional Control Chart of Centofanti et al. (2025).
* `AMFCC_PhaseII()` performs Phase II of the Adaptive Multivariate Functional Control Chart of Centofanti et al. (2025).

References:

* Centofanti, F., Lepore, A., Palumbo, B. (2025) Adaptive Multivariate Functional Control Chart. 
Accepted for publication in \emph{Technometrics}.

# funcharts 1.6.0

* `simulate_data_FRTM()` simulates data for real-time monitoring of univariate functional data.
* `OEBFDTW()` performs Open-end/Open-begin Functional Dynamic Time Warping (OEB-FDTW).
* `mFPCA()` performs Mixed Functional Principal Component Analysis (mFPCA).
* `FRTM_PhaseI()` performs Phase I of the FRTM method.
* `FRTM_PhaseII()` performs Phase II of the FRTM method.
* `cov_mfd()` calculates the covariance of multivariate functional data returning a `bifd` object.
* `cor_mfd()` calculates the correlation of multivariate functional data returning a `bifd` object.
* `mean_mfd()` calculates the mean of multivariate functional data, returning an `mfd` object.

References:

* Centofanti, F., Lepore, A., Kulahci, M., and Spooner, M. P. (2024).
  Real-time monitoring of functional data. \emph{Journal of Quality Technology}.
  
* Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024). 
Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152, 
doi:https://doi.org/10.1080/00224065.2024.2430978.

# funcharts 1.5.0

* `AMFEWMA_PhaseI()` performs Phase I of the adaptive multivariate functional EWMA control chart of Capezza et al. (2024).
* `AMFEWMA_PhaseII()` performs Phase II of the adaptive multivariate functional EWMA control chart of Capezza et al. (2024).

References:

* Capezza, C., Capizzi, G., Centofanti, F., Lepore, A., Palumbo, B. (2025)
 An Adaptive Multivariate Functional EWMA Control Chart.
 \emph{Journal of Quality Technology},  57(1):1--15,
 doi:https://doi.org/10.1080/00224065.2024.2383674.

# funcharts 1.4.1

* fixed default choices of `RoMFCC_PhaseI()` to be consistent with the choices proposed in in Capezza et al. (2024).

References:

* Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024) 
Robust Multivariate Functional Control Chart. 
\emph{Technometrics}, 66(4):531--547, doi:10.1080/00401706.2024.2327346.


# funcharts 1.4.0

* `rpca_mfd()` performs multivariate functional principal component analysis as described in Capezza et al. (2024).
* `functional_filter()` performs the functional filtering step of the robust multivariate functional control chart framework of Capezza et al. (2024).
* `RoMFDI()` performs the robust multivariate functional data imputation step as described in Capezza et al. (2024).
* `RoMFCC_PhaseI()` performs Phase I of the robust multivariate functional control chart framework of Capezza et al. (2024).
* `RoMFCC_PhaseII()` performs Phase II of the robust multivariate functional control chart framework of Capezza et al. (2024).

References:

* Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024) 
Robust Multivariate Functional Control Chart. 
\emph{Technometrics}, , 66(4):531--547, doi:10.1080/00401706.2024.2327346.
# funcharts 1.3.2

* Updated documentation with the newly published paper, which thoroughly illustrates the funcharts package:
Capezza C, Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2023) funcharts: control charts for multivariate functional data in R.
Journal of Quality Technology, doi:10.1002/asmb.2507.

# funcharts 1.3.1

* the default value of the `parametric_limits` argument in `regr_cc_sof()` is now set to `FALSE`.

# funcharts 1.3.0

* all basis function systems that can be used in the `fda` package now can be used also with `funcharts`, which previously it could be used only with B-spline basis.
In particular, Fourier, exponential, monomial, polygonal, power and constant basis function systems are available.
* `get_outliers_mfd()` allows to find outliers among multivariate functional data using the functional boxplot through the `fbplot()` function of the `roahd` package.
* test coverage has been increased
* `control_charts_sof()` and `control_charts_sof_real_time()` have been deprecated.
Instead, use `regr_cc_sof()` and `regr_cc_sof_real_time()`, respectively, with argument `include_covariates = TRUE`. 
This has been done to make more consistent the regression control chart functions for the scalar (`regr_cc_sof()` and `regr_cc_sof_real_time()`) and functional (`regr_cc_fof()` and `regr_cc_fof_real_time()`) response cases.
* `alpha` parameter in all control charting functions, which previously could only be a list with manually specified values of the type-I error probability in each control chart, now can also be a single number between 0 and 1. In this case, Bonferroni correction is automatically applied to take into account the multiplicity problem when more than one control chart is applied.
* `plot_bifd()` now allows to choose to produce also contour or perspective plots of `bifd` objects.
* `simulate_mfd()` is much more general, now it allows to simulate as many covariates as one wants (before the number was fixed to three), it is possible to provide manually the mean and variance function for each variable, it is possible to select the type of correlation function for each variable.
* `plot_mfd()` now relies on patchwork, while the new function `lines_mfd()` allows to add new curve to an existing plot.

# funcharts 1.2.0

* improved backward compatibility, `funcharts` now depends on an older version of R, i.e., >3.6.0 instead of >4.0.0
* `fof_pc()` now is much faster especially when the number of basis functions of the functional coefficient is large since the tensor product has been vectorized.
* the argument `seed` has been deprecated in all functions, so that reproducibility is achieved by setting externally a seed with `set.seed()`, as it is commonly done in R.
* `sim_funcharts()` simulates data sets automatically using the function `simulate_mfd()`. The only input required is the sample size for the Phase I, tuning and Phase II data sets.
* `control_charts_pca()` allows automatic selection of components.
* `get_mfd_list()` and `get_mfd_array()`, with the corresponding real time versions, are now much faster.
* cross-validation in scalar-on-function regression is now much faster, since the for loop is avoided
* inner products are more precise and much faster, because they rely on the pre-computed inner products of the B-spline basis functions, calculated via `inprod.bspline()`.
* argument `seed` is deprecated in all functions. Instead, a seed must be set before calling the functions by using `set.seed()`.

# funcharts 1.1.0

## Major changes

* `simulate_mfd()` simulates example data for `funcharts`. 
It creates a data set with three functional covariates, a functional response generated as a function of the three functional covariates through a function-on-function linear model, and a scalar response generated as a function of the three functional covariates through a scalar-on-function linear model. This function covers the simulation study in Centofanti et al. (2020) for the function-on-function case and also simulates data in a similar way for the scalar response case.

## Minor changes

* Added a `NEWS.md` file to track changes to the package.
* `inprod_mfd_diag()` calculates the inner product between two multivariate functional data objects observation by observation, avoiding calculating it between all possible couples of observations. Therefore, there are n calculations instead of squared n, saving much computational time when calculating the squared prediction error statistic when n is large.
* Code has been improved so that `scale_mfd()` is pre-computed and therefore is not called many times unnecessarily along the different functions.


# funcharts 1.0.0

* Initial release
