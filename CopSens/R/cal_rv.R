#' Calculate Robustness Value When Executing Worstcase Calibration
#'
#' @param y \code{data.frame}, \code{matrix} or \code{vector}. Gaussian outcome variable.
#' @param tr \code{data.frame}. Treatment variables with rows corresponding to observations and columns
#' to variables.
#' @param t1 \code{data.frame}. First treatment arms of interest.
#' May contain a single or multiple treatments in rows.
#' @param t2 \code{data.frame}. Second treatment arms of interest,
#' which has same number of row as \code{t1}.
#' @param mu_y_dt an optional scalar or vector that contains naive estimates of treatment effects
#' ignoring confounding.
#' @param sigma_y_t an optional scalar of the standard deviation of outcome conditional on treatments.
#' @param mu_u_dt an optional matrix of difference in conditional confounder means, \eqn{E(U \mid t1) - E(U \mid t2)},
#' with latent variables in columns.
#' @param cov_u_t an optional covariance matrix of confounders conditional on treatments.
#' @param nU Number of latent confounders to consider.
#' @param ... further arguments passed to \code{\link{kEstimate}}, \code{\link{pca}}
#'
#' @return A \code{numeric vector} with elements being the robustness value or \code{NA} if the ignorance region doesn't
#' contains 0 for each contrast of interest.
#'
#' @importFrom stats lm
#' @importFrom stats sigma
#' @importFrom dplyr %>%
#'
#' @export
#' @examples
#' \donttest{
#' # load the example data #
#' y <- GaussianT_GaussianY$y
#' tr <- subset(GaussianT_GaussianY, select = -c(y))
#' # calculate robustness value #
#' cal_rv(y = y, tr = tr, t1 = tr[1:2,], t2 = tr[3:4,])
#'}

cal_rv <- function(y, tr, t1, t2,
                   mu_y_dt = NULL, sigma_y_t = NULL,
                   mu_u_dt = NULL, cov_u_t = NULL, nU = NULL, ...) {
  # by default, fitting latent confounder model by PPCA #
  if (is.null(mu_u_dt) | is.null(cov_u_t)) {
    message("Fitting the latent confounder model by PPCA with default.")
    if (is.null(nU)) {
      ut_cv <- pcaMethods::kEstimate(tr, method = "ppca", allVariables = TRUE, ...)
      nU <- ut_cv$bestNPcs
    }
    ut_ppca <- pcaMethods::pca(tr, method = "ppca", center = TRUE,
                               nPcs = nU, ...)
    W = pcaMethods::loadings(ut_ppca)
    tr_hat <- pcaMethods::scores(ut_ppca) %*% t(W)
    sig2est <- sum((tr - tr_hat)^2)/(nrow(tr)*ncol(tr))
    ## cov(U|t) = sigma2*M^{-1}, M = W'W+ sigma2*I
    if (is.null(cov_u_t)) {
      cov_u_t <- sig2est * solve(t(W) %*% W + sig2est*diag(nU))
    }
    if (is.null(mu_u_dt)) {
      mu_u_dt <- predict(ut_ppca, newdata = t1)$scores - predict(ut_ppca, newdata = t2)$scores
    }
  }
  if (ncol(mu_u_dt) == 1) {
    cov_halfinv <- 1/sqrt(cov_u_t)
  } else {
    eigen_cov <- eigen(cov_u_t)
    cov_halfinv <- eigen_cov$vectors %*% diag(eigen_cov$values^{-1/2}) %*% t(eigen_cov$vectors)
  }
  # by default, fitting the outcome by ordinary linear regression model #
  if (is.null(mu_y_dt) | is.null(sigma_y_t)) {
    message("Observed outcome model fitted by simple linear regression with default.")
    lm_y_t <- lm(y ~., data = data.frame(y,tr))
    if (is.null(mu_y_dt)) {
      mu_y_dt <- predict(lm_y_t, newdata = t1) - predict(lm_y_t, newdata = t2)
    }
    if (is.null(sigma_y_t)) {
      sigma_y_t <- sigma(lm_y_t)
    }
  }
  rv <- (c(mu_y_dt^2) / apply(mu_u_dt %*% cov_halfinv, 1, function(x) sum(x^2)) /
           sigma_y_t^2) %>% round(digits = 4)
  rv[rv > 1] <- NA
  rv
}



