#' @importFrom stats sigma
#' @export
stats::sigma

#' Extract residual standard deviation or dispersion parameter
#'
#' @description
#' see \code{?glmmTMB::sigma} for more details.
#'
#'
#' @param object An object of class \code{cglmm}.
#' @param ... (ignored; for method compatibility)
#'
#' @return a \code{numeric}.
#' @export
#'
#' @examples
#' testdata_poisson <- simulate_cosinor(100,
#'   n_period = 2,
#'   mesor = 7,
#'   amp = c(0.1, 0.5),
#'   acro = c(1, 1),
#'   beta.mesor = 4.4,
#'   beta.amp = c(0.1, 0.46),
#'   beta.acro = c(0.5, -1.5),
#'   family = "poisson",
#'   period = c(12, 6),
#'   n_components = 2,
#'   beta.group = TRUE
#' )
#'
#' mod <- cosinor_model <- cglmm(
#'   Y ~ group + amp_acro(times,
#'     period = c(12, 6),
#'     n_components = 2,
#'     group = "group"
#'   ),
#'   data = testdata_poisson,
#'   family = glmmTMB::nbinom1()
#' )
#' sigma(mod)
sigma.cglmm <- function(object, ...) {
  glmmTMB::sigma(object$fit, ...)
}
