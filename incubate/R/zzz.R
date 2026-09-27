# mkuhn, 2025-04-10
# zzz.R to be run last within the package code

.onLoad <- function(libname, pkgname) {
  # build spline functions in current run-time session of user
  #+as splinefun is not 100% portable betw different R-versions
  rlang::env_bind(
    the,
    L = stats::splinefun(
      x = MLEw_approx$coef$W3_richards$nObs,
      y = MLEw_approx$coef$W3_richards$L,
      method = "natural"
    ),
    A = stats::splinefun(
      x = MLEw_approx$coef$W3_richards$nObs,
      y = MLEw_approx$coef$W3_richards$A,
      method = "natural"
    ),
    d = stats::splinefun(
      x = MLEw_approx$coef$W3_richards$nObs,
      y = MLEw_approx$coef$W3_richards$d,
      method = "natural"
    ),
    K = stats::splinefun(
      x = MLEw_approx$coef$W3_richards$nObs,
      y = MLEw_approx$coef$W3_richards$K,
      method = "natural"
    ),
    Xi = stats::splinefun(
      x = MLEw_approx$coef$W3_richards$nObs,
      y = MLEw_approx$coef$W3_richards$Xi,
      method = "natural"
    )
  )
}
