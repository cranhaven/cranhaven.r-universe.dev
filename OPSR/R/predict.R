#' Predict Method for OPSR Model Fits
#'
#' Obtains predictions for the selection process (probabilities), the outcome process,
#' or returns the inverse mills ratio. Handles also log-transformed outcomes.
#'
#' @param object an object of class `"opsr"`.
#' @param newdata an optional data frame in which to look for variables used in
#'   `object$formula`. See also [`model.matrix.opsr`]. Ensure that at least one
#'   selection outcome matches the 'group' argument (see 'Examples').
#' @param group predict outcome of this group (regime).
#' @param counterfact counterfactual group.
#' @param type type of prediction. Can be abbreviated. See 'Details' section for
#'   more information.
#' @param delta constant that was added during the continuity correction
#'   (\eqn{log(Y_j + \delta)}). Only applies for `type = "unlog-response"`.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a vector of length `nrow(newdata)` (or data used during estimation).
#' @method predict opsr
#' @seealso [`stats::predict`]
#'
#' @details
#' Elements are `NA_real_` if the `group` does not correspond to the observed
#' regime (selection outcome). This ensures consistent output length.
#'
#' If the `type` argument is `"response"` then the continuous outcome is predicted.
#' Use `"unlog-response"` if the outcome response was log-transformed (i.e., either
#' in the `formula` specification or during data pre-processing).
#' `"prob"` returns the probability vector of belonging to `group`, `"mills"`
#' returns the inverse mills ratio, `"correction"` the heckman correction (i.e.,
#' \eqn{\rho_j * \sigma_j * \text{mills}}) and `"Xb"` returns \eqn{X \beta}.
#'
#' @example R/examples/ex-predict.R
#' @export
predict.opsr <- function(object, newdata, group, counterfact = NULL,
                         type = c("response", "unlog-response", "prob", "mills", "correction", "Xb"),
                         delta = 1, ...) {
  args <- as.list(match.call())
  if (!("group" %in% names(args))) {
    stop("'group' argument is required")
  }
  type <- match.arg(type)
  predict_opsr <- function(X_j, W_j, beta_j, rho_j, sigma_j, kappa_j_1, kappa_j, gamma,
                           type = c("response", "unlog-response", "prob", "mills", "correction", "Xb")) {
    type <- match.arg(type)
    Xb <- X_j %*% beta_j  # xbif(j)
    Wg <- W_j %*% gamma  # xbsel
    kappa_j_Wg <- kappa_j - Wg
    kappa_j_1_Wg <- kappa_j_1 - Wg
    nom <- stats::dnorm(kappa_j_Wg) - stats::dnorm(kappa_j_1_Wg)
    denom <- stats::pnorm(kappa_j_Wg) - stats::pnorm(kappa_j_1_Wg)
    imr <- nom / denom  # (negative) inverse mills ratio millsif(j)

    out <-
      switch(type,
             "response" = as.vector(
               Xb - (rho_j * sigma_j) * imr
             ),
             "unlog-response" = as.vector(
               exp(Xb + (sigma_j**2) / 2) *
                 (stats::pnorm(kappa_j_Wg - rho_j * sigma_j) - stats::pnorm(kappa_j_1_Wg - rho_j * sigma_j)) /
                 (stats::pnorm(kappa_j_Wg) - stats::pnorm(kappa_j_1_Wg)) - delta
             ),
             "prob" = as.vector(stats::pnorm(kappa_j_Wg) - stats::pnorm(kappa_j_1_Wg)),
             "mills" = as.vector(-imr),  # -!
             "correction" = as.vector(rho_j * sigma_j * -imr),  # -!
             "Xb" = as.vector(Xb)
      )

    out
  }

  ## W_j is always the factual data matrix
  mm <- model.matrix(object, data = newdata)
  X <- mm$X[[group]]
  W <- mm$W[[group]]
  coefs_j <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[group]]

  if (is.null(X) || is.null(W)) {
    stop("'newdata' does not feature observations from 'group'. Ensure that at",
         " least one selection outcome matches the 'group' argument.")
  }

  ## for the counterfactuals we pass X', beta_j', rho_j' and sigma_j'
  ## if type == prob we even pass kappa_j', kappa_j_1'
  if (!is.null(counterfact)) {
    mm <- model.matrix(object, data = newdata, .filter = group)
    coefs_j_<- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[counterfact]]
    X <- mm$X[[counterfact]]
    coefs_j[["beta_j"]] <- coefs_j_[["beta_j"]]
    coefs_j[["rho_j"]] <- coefs_j_[["rho_j"]]
    coefs_j[["sigma_j"]] <- coefs_j_[["sigma_j"]]
    if (type == "prob") {
      coefs_j[["kappa_j"]] <- coefs_j_[["kappa_j"]]
      coefs_j[["kappa_j_1"]] <- coefs_j_[["kappa_j_1"]]
    }
  }

  ## is kappa_j_1 -Inf and kappa_j Inf a problem => don't think so
  p <- predict_opsr(X, W, coefs_j[["beta_j"]], coefs_j[["rho_j"]],
                    coefs_j[["sigma_j"]], coefs_j[["kappa_j_1"]], coefs_j[["kappa_j"]],
                    coefs_j[["gamma"]], type = type)

  ## ensure to match length of data
  z <- get_z(object, newdata)
  out <- rep(NA_real_, length(z))
  out[z == group] <- p

  out
}
