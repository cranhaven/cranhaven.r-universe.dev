#' Post-selection inference for PLSMM
#'
#' This function debias the lasso coefficients estimated from the \code{\link{plsmm_lasso}} function 
#' and computes p-values.
#' 
#' @param x A matrix of predictor variables.
#' @param y A continuous vector of response variable.
#' @param series A variable representing different series or groups in the data modeled as a random intercept.
#' @param plsmm_output Output object obtained from the \code{\link{plsmm_lasso}} function.
#' @param a A scalar that adjusts the variance of the random intercept \eqn{\phi} by \eqn{a \times \sigma_{\phi}}, default is 1.
#' @param Z (Optional) Pre-computed correction score matrix. If provided, it will be used directly for debiasing.
#' 
#' @return A data frame containing debiased coefficients, standard errors, confidence intervals, and p-values.
#' 
#' @details The original data is decorrelated, and a correction score matrix is computed. The correction scores are a measure of correlation between the predictors in the data.
#' The debiasing process utilizes these scores to compute debiased estimates of the coefficients, along with associated p-values.
#' 
#' @examples
#' 
#' set.seed(123)
#' data_sim = simulate_group_inter(N = 50, n_mvnorm = 3, grouped = TRUE,
#'                                 timepoints = 3:5, nonpara_inter = TRUE,
#'                                 sample_from = seq(0,52,13),
#'                                 cos = FALSE, A_vec = c(1, 1.5))
#' sim = data_sim$sim
#' x = as.matrix(sim[,-1:-3])
#' y = sim$y
#' series = sim$series
#' t = sim$t
#' bases = create_bases(t)
#' lambda <- 0.0046
#' gamma <- 0.00000001
#' plsmm_output <- plsmm_lasso(x, y, series, t,
#'   name_group_var = "group", bases$bases,
#'   gamma = gamma, lambda = lambda, timexgroup = TRUE,
#'   criterion = "BIC"
#' )
#' debias_plsmm(x, y, series, plsmm_output)
#' 
#' 
#' @export
debias_plsmm <- function(x, y, series, plsmm_output, a = 1, Z = NULL) {
  y_offset <- y - plsmm_output$lasso_output$out_f$f_fit

  series <- as.factor(series)

  z <- stats::model.matrix(~ series - 1)

  Sigma_a <- a * plsmm_output$su * z %*% t(z) + plsmm_output$se * diag(rep(1, length(y_offset)))

  Sigma_a_svd <- svd(Sigma_a)
  Sigma_a_sqrt_inv <- Sigma_a_svd$u %*% diag(1 / sqrt(Sigma_a_svd$d)) %*% t(Sigma_a_svd$u)

  x_a <- Sigma_a_sqrt_inv %*% x
  y_a <- Sigma_a_sqrt_inv %*% y_offset

  if (is.null(Z)) {
    de_sparsified <- suppressMessages(hdi::lasso.proj(x_a, y_a,
      suppress.grouptesting = TRUE, return.Z = TRUE,
      do.ZnZ = TRUE, betainit = "scaled lasso"
    ))

    debias_score_matrix <- de_sparsified$Z
  } else {
    debias_score_matrix <- Z
  }
  # removing intercept
  beta_original <- plsmm_output$lasso_output$theta[-1] 

  res <- y_a - x_a %*% beta_original

  p <- length(beta_original)

  beta_debias <- rep(NA, p)
  beta_debias_sd <- rep(NA, p)

  for (j in 1:p) {
    score_j <- debias_score_matrix[, j]

    beta_debias[[j]] <- beta_original[j] + sum(score_j * res) / sum(score_j * x_a[, j])

    # Group by 'series' and summarize scaled_res
    df_res <- dplyr::summarize(dplyr::group_by(data.frame(score_j, res, series), series),
      scaled_res = (sum(score_j * res))^2
    )

    # Calculate scaled_rss
    scaled_rss <- sum(df_res$scaled_res)

    beta_debias_sd[[j]] <- sqrt(scaled_rss) / sqrt((sum(score_j * x_a[, j]))^2)
  }

  ci <- cbind(
    beta_debias - 1.96 * beta_debias_sd,
    beta_debias + 1.96 * beta_debias_sd
  )

  # Calculate p-value
  beta_debias_rescaled <- beta_debias * (1 / beta_debias_sd)
  pv <- 2 * stats::pnorm(abs(beta_debias_rescaled), lower.tail = FALSE)

  df.posi <- data.frame(
    Estimate = beta_original,
    Debiased = beta_debias,
    `Std. Error` = beta_debias_sd,
    `Lower 95%` = ci[, 1],
    `Upper 95%` = ci[, 2],
    `p-value` = pv, check.names = FALSE
  )

  return(df.posi)
}
