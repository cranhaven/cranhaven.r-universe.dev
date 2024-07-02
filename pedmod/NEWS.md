# pedmod 0.2.4
* deal with a deprecated `<<` operator for `arma` objects in version 11.2.3.

# pedmod 0.2.3
* `eval_pedigree_hess` is faster.
* fixed a bug from release 0.2.0 which in extreme settings could cause the C++ 
  code to run forever.
* deal with a breaking change in RcppArmadillo which __could possibly__
  cause issues in this package. See https://stackoverflow.com/a/72533955/5861244
* better starting values are used by `pedmod_profile_prop`. It is also possible 
  to pass a bound on the confidence interval using the `bound` argument.
* `mvndst_grad` is added which computes the gradient with respect to the mean 
  and covariance matrix.

# pedmod 0.2.2
* a minor bug fix on Mac when using Apple LLVM version 10.0.0 with R version
  4.2.0 and x86_64.

# pedmod 0.2.1
* A hessian approximation of objects from `pedigree_ll_terms` is added in 
  the `eval_pedigree_hess` function.
* `pedmod_profile` works with object from `pedigree_ll_terms_loadings`.
* `pedmod_profile_nleq` has been added to construct profile likelihood based 
  confidence intervals for general non-linear transformations of the model 
  parameters.
* An undefined undefined behavior bug has been fixed in the C++ code which 
  possibly effects cases where `use_aprx = TRUE` but only in very extreme 
  settings.
* A bug has been fixed in `pedmod_profile_prop`. `minvls_start` and 
  `maxvls_start` were used instead of `minvls` and `maxvls`.
* The code to compute the limits in `pedmod_profile` and `pedmod_profile_prop` 
  has been changed. The previous code could give very wrong points for the 
  `conf` element if a point was computed very far from one of the confidence 
  limits. The issue was caused by using `approx` in combination with 
  `spline` and with points with great distance.

# pedmod 0.2.0
* `pedigree_ll_terms_loadings` is implemented to support models with individual 
  specific covariance scale parameters (e.g. individual specific 
  heritabilities).
* The minimax tilting method suggested by Botev (2017) (see 
  https://doi.org/10.1111/rssb.12162) is implemented. The method is less 
  numerically stable and thus required more care when implementing. This yield a 
  higher per randomized quasi-Monte Carlo sample cost. Though, the increased 
  cost may be worthwhile for low probability events because of a reduced 
  variance at a fixed number of samples.
* The `vls_scales` argument is added which allows the user to use more 
  randomized quasi-Monte Carlo samples for some log likelihood terms. This is 
  useful e.g. when one uses weighted terms.

# pedmod 0.1.0 
* First release on CRAN.
