#' Factor Model Testing with Wald, GRS, PY tests and FDR control
#'
#' Performs comprehensive factor model testing including joint tests (Wald, GRS, PY), 
#' individual asset t-tests, and False Discovery Rate control.
#'
#' @param ret A T × N matrix representing the excess returns of N assets at T time points.
#' @param fac A T × K matrix representing the returns of K factors at T time points.
#' @param q.fdr The significance level for FDR (False Discovery Rate) testing, defaulting to 5\%.
#'
#' @return A list containing the following components:
#' \item{alpha}{N-vector of estimated alphas for each asset}
#' \item{tstat}{N-vector of t-statistics for testing individual alphas}
#' \item{pval}{N-vector of p-values for individual alpha tests}
#' \item{Wald}{Wald test statistic for joint alpha significance}
#' \item{p_Wald}{p-value for Wald test}
#' \item{GRS}{GRS test statistic (finite-sample F-test)}
#' \item{p_GRS}{p-value for GRS test}
#' \item{PY}{Pesaran and Yamagata test statistic}
#' \item{p_PY}{p-value for PY test}
#' \item{reject_fdr}{Logical vector indicating which assets have significant alphas after FDR correction}
#' \item{fdr_p}{Adjusted p-values using Benjamini-Hochberg procedure}
#' \item{power_proxy}{Number of significant assets after FDR correction}
#'
#' @examples
#' set.seed(42)
#' T <- 120
#' N <- 25
#' K <- 3
#' fac <- matrix(rnorm(T * K), T, K)
#' beta <- matrix(rnorm(N * K), N, K)
#' alpha <- rep(0, N)
#' alpha[1:3] <- 0.4 / 100  # 3 non-zero alphas
#' eps <- matrix(rnorm(T * N, sd = 0.02), T, N)
#' ret <- alpha + fac %*% t(beta) + eps
#' results <- factor.tests(ret, fac, q.fdr = 0.05)
#' 
#' # View results
#' cat("Wald test p-value:", results$p_Wald, "\n")
#' cat("GRS test p-value:", results$p_GRS, "\n")
#' cat("PY test p-value:", results$p_PY, "\n")
#' cat("Significant assets after FDR:", results$power_proxy, "\n")
#'
#' @export
#' @importFrom stats lm residuals coef cov cor pchisq pf pnorm p.adjust
#' @importFrom MASS ginv
factor.tests <- function(ret, fac, q.fdr = 0.05) {
  T  <- nrow(ret)
  N  <- ncol(ret)
  K  <- ncol(fac)
  
  # --- 1. asset-by-asset OLS ---
  alpha <- numeric(N)
  tstat <- numeric(N)
  pval  <- numeric(N)
  resid <- matrix(NA, T, N)
  
  for (j in 1:N) {
    mod <- lm(ret[, j] ~ fac)
    alpha[j] <- coef(mod)[1]
    resid[, j] <- residuals(mod)
    tstat[j]  <- summary(mod)$coefficients[1, 3]
    pval[j]   <- summary(mod)$coefficients[1, 4]
  }
  
  # --- 2. residual covariance matrices ---
  Sigma_u <- cov(resid)          # full
  Sigma_u_inv <- tryCatch(solve(Sigma_u),
                          error = function(e) MASS::ginv(Sigma_u))
  D_hat   <- diag(diag(Sigma_u)) # diagonal only (PY)
  
  # --- 3. joint tests ---
  # 3a. Wald
  Wald <- T * as.numeric(t(alpha) %*% Sigma_u_inv %*% alpha)
  p_Wald <- pchisq(Wald, df = N, lower.tail = FALSE)
  
  # 3b. GRS (finite-sample F)
  dof_num <- N
  dof_den <- T - N - K
  if (dof_den <= 0) {
    GRS <- p_GRS <- NA_real_
  } else {
    fbar  <- colMeans(fac)
    Omega <- cov(fac)
    adj   <- 1 + as.numeric(t(fbar) %*% solve(Omega) %*% fbar)
    GRS   <- (dof_den / dof_num) * (Wald / adj) / (T - K - 1)
    p_GRS <- pf(GRS, dof_num, dof_den, lower.tail = FALSE)
  }
  
  # 3c. PY (working-independent)
  kappa <- 1 + (2/N) * sum(cor(resid)^2)
  PY_num <- as.numeric(t(alpha) %*% solve(D_hat) %*% alpha) - N
  PY <- PY_num / sqrt(2 * N * kappa)
  p_PY <- pnorm(PY, lower.tail = FALSE)
  
  # --- 4. FDR on asset-level p-values ---
  bh <- p.adjust(pval, method = "BH")
  reject_fdr <- bh <= q.fdr
  power_proxy <- sum(reject_fdr)
  
  # --- 5. return everything ---
  list(alpha = alpha,
       tstat = tstat,
       pval  = pval,
       Wald  = Wald,  p_Wald = p_Wald,
       GRS   = GRS,   p_GRS  = p_GRS,
       PY    = PY,    p_PY   = p_PY,
       reject_fdr = reject_fdr,
       fdr_p      = bh,
       power_proxy= power_proxy)
}