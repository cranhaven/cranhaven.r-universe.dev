#' Calibration for Binary Outcomes
#'
#' @description Calibrates the naive estimates to account for unobserved confounding when outcome
#' variables are binary. The calibration can be done with user-specific sensitivity parameter or
#' with our pre-provided calibration methods, the worst-case calibration for a single contrast
#' or multivariate calibration for multiple contrasts.
#'
#' @param y \code{data.frame}, \code{matrix} or \code{vector}. Binary outcome variable.
#' @param tr \code{data.frame}. Treatment variables with rows corresponding to observations and columns
#' to variables.
#' @param t \code{data.frame}. Treatment arms of interest.
#' May contain a single or multiple treatments in rows.
#' @param gamma a vector specifying the direction of sensitivity parameters.
#' @param R2 an optional scalar or vector specifying the proportion of residual variance in outcome given the
#' treatment that can be explained by confounders, which determines the magnitude of sensitivity parameters.
#' @param mu_y_t an optional scalar or vector that contains naive estimates of treatment effects
#' ignoring confounding.
#' @param mu_u_tr an optional matrix of conditional confounder means for all observed treatments
#' with latent variables in columns.
#' @param mu_u_t an optional matrix of conditional confounder means for treatments of interest
#' with latent variables in columns.
#' @param cov_u_t an optional covariance matrix of confounders conditional on treatments.
#' @param nU Number of latent confounders to consider.
#' @param nsim an optional scalar specifying the number of sample draws.
#' @param ... further arguments passed to \code{\link{kEstimate}}or \code{\link{pca}}.
#'
#'
#' @return A \code{data.frame} with naive and calibrated estimates of population average outcome receiving
#' treatment \code{t}.
#'
#' @importFrom stats predict
#' @importFrom stats glm
#' @importFrom stats binomial
#'
#' @import tidyverse
#'
#' @export
#'
#' @examples
#' \donttest{
#' # load the example data #
#' y <- GaussianT_BinaryY$y
#' tr <- subset(GaussianT_BinaryY, select = -c(y))
#' t1 <- tr[1:5,]
#' t2 <- rep(0, times = ncol(tr))
#' # calibration #
#' est_b <- bcalibrate(y = y, tr = tr, t = rbind(t1, t2),
#'                     nU = 3, gamma = c(1.27, -0.28, 0),
#'                     R2 = c(0.2, 0.7))
#' est_b_rr <- list(est_df = est_b$est_df[1:5,] / as.numeric(est_b$est_df[6,]),
#'                  R2 = c(0.2, 0.7))
#' plot_estimates(est_b_rr)
#' }


bcalibrate <- function(y, tr, t, gamma, R2 = NULL, mu_y_t = NULL,
                       mu_u_tr = NULL, mu_u_t = NULL, cov_u_t = NULL,
                       nU = NULL, nsim = 4000, ...) {
  # by default, fitting latent confounder model by PPCA #
  if (is.null(mu_u_tr) | is.null(mu_u_t) | is.null(cov_u_t)) {
    message("Fitting the latent confounder model by PPCA with default.")
    if (is.null(nU)) {
      ut_cv <- pcaMethods::kEstimate(tr, method = "ppca", allVariables = TRUE, ...)
      nU <- ut_cv$bestNPcs
    }
    ut_ppca <- pcaMethods::pca(tr, method = "ppca", center = TRUE,
                               nPcs = nU, ...)
    W <- pcaMethods::loadings(ut_ppca)
    tr_hat <- pcaMethods::scores(ut_ppca) %*% t(W)
    sig2est <- sum((tr - tr_hat)^2)/(nrow(tr)*ncol(tr))
    ## cov(U|t) = sigma2*M^{-1}, M = W'W+ sigma2*I
    cov_u_t <- sig2est * solve(t(W) %*% W + sig2est*diag(nU))
    mu_u_tr <- predict(ut_ppca, newdata = tr)$scores
    mu_u_t <- predict(ut_ppca, newdata = t)$scores
  }
  # by default, fitting the outcome by ordinary linear regression model #
  if (is.null(mu_y_t)) {
    message("Observed outcome model fitted by simple probit model with default.")
    lm_y_t <- glm(y ~., family = binomial(link = "probit"), data = data.frame(y, tr))
    mu_y_t <- predict(lm_y_t, newdata = t, type = "response")
  }
  if (!is.null(R2)) {
    cali <- matrix(NA, nrow = length(mu_y_t), ncol = length(R2))
    for (i in 1:length(R2)) {
      gamma <- sqrt(R2[i]) * gamma / sqrt(c(t(gamma) %*% cov_u_t %*% gamma))
      message("R2 = ", R2[i] , ", calibrating observation ")
      cali[,i] <- sapply(1:nrow(t), cali_mean_ybinary_algm, gamma, mu_u_tr, mu_u_t, mu_y_t, ...)
      message("\n")
      }
    # est_df <- data.frame(cbind(mu_y_t, cali))
    # colnames(est_df) <- paste0("R2_", round(c(0, R2), digits = 2))
    # est_df
    } else {
      cali <- matrix(NA, nrow = length(mu_y_t), ncol = length(gamma))
      R2 <- rep(NA, nrow(gamma))
      for (i in 1:nrow(gamma)) {
        message("gamma = ", gamma[i,], "\n", "calibrating observation ")
        cali[,i] <- sapply(1:nrow(t), cali_mean_ybinary_algm, gamma[i,], mu_u_tr, mu_u_t, mu_y_t, ...)
        R2[i] <- c(t(gamma[i,]) %*% cov_u_t %*% gamma[i,])
        message("\n")
      }
    }
  est_df <- data.frame(cbind(mu_y_t, cali))
  colnames(est_df) <- paste0("R2_", round(c(0, R2), digits = 2))
  list(est_df = est_df, R2 = R2)
}






