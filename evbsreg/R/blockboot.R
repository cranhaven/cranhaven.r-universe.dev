#' Block bootstrap standard errors for the EVBS regression model
#'
#' Nonparametric moving-block bootstrap for the parameters of the EVBS
#' regression model. Intended for series with residual serial dependence, where
#' the standard errors obtained from the observed information under the
#' independence assumption are optimistic.
#'
#' @param X Design matrix (including the intercept column).
#' @param y Response vector.
#' @param L Block length.
#' @param B Number of bootstrap resamples.
#'
#' @return A list with the bootstrap standard errors and the matrix of
#'   bootstrap estimates.
#' @examples
#' \donttest{
#' data(itajai)
#' evbs_block_boot(cbind(1, itajai$pressure), itajai$wind, L = 4, B = 100)$se
#' }
#' @export
evbs_block_boot <- function(X, y, L = 4, B = 500) {
  X <- as.matrix(X); n <- length(y)
  est <- matrix(NA_real_, B, ncol(X) + 2L)
  for (b in seq_len(B)) {
    starts <- sample.int(n - L + 1L, ceiling(n / L), replace = TRUE)
    idx <- as.vector(vapply(starts, function(s) s:(s + L - 1L),
                            integer(L)))[seq_len(n)]
    f <- try(evbsreg.fit(X[idx, , drop = FALSE], y[idx]), silent = TRUE)
    if (!inherits(f, "try-error")) {
      est[b, ] <- c(f$betahat, f$alphahat, f$gamahat)
    }
  }
  colnames(est) <- c(paste0("beta", seq_len(ncol(X)) - 1L), "alpha", "gama")
  list(se = apply(est, 2, stats::sd, na.rm = TRUE), estimates = est)
}
