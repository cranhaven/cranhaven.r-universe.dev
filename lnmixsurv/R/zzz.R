#' @importFrom generics tidy
#' @export
generics::tidy
#' @importFrom generics augment
#' @export
generics::augment
#' @importFrom stats nobs
#' @export
stats::nobs

# nocov start

.onLoad <- function(libname, pkgname) {
  make_survival_reg_survival_ln_mixture()
  make_survival_reg_survival_ln_mixture_em()
}

# nocov end
