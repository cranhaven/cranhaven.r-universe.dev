#' Distributed Factor Model Testing with Wald, GRS, PY tests and FDR control
#'
#' Performs comprehensive factor model testing in distributed environment across multiple nodes,
#' including joint tests (Wald, GRS, PY), individual asset t-tests, and False Discovery Rate control.
#'
#' @param ret A T × N matrix representing the excess returns of N assets at T time points.
#' @param fac A T × K matrix representing the returns of K factors at T time points.
#' @param n1 The number of assets allocated to each node
#' @param K The number of nodes
#' @param q.fdr The significance level for FDR (False Discovery Rate) testing, defaulting to 5\%.
#'
#' @return A list containing the following components:
#' \item{alpha_list}{List of alpha vectors from each node}
#' \item{tstat_list}{List of t-statistics from each node}
#' \item{pval_list}{List of p-values from each node}
#' \item{Wald_list}{List of Wald test statistics from each node}
#' \item{p_Wald_list}{List of p-values for Wald tests from each node}
#' \item{GRS_list}{List of GRS test statistics from each node}
#' \item{p_GRS_list}{List of p-values for GRS tests from each node}
#' \item{PY_list}{List of Pesaran and Yamagata test statistics from each node}
#' \item{p_PY_list}{List of p-values for PY tests from each node}
#' \item{reject_fdr_list}{List of logical vectors indicating significant assets after FDR correction from each node}
#' \item{power_proxy_list}{List of number of significant assets after FDR correction from each node}
#' \item{combined_alpha}{Combined alpha vector from all nodes}
#' \item{combined_pval}{Combined p-value vector from all nodes}
#' \item{combined_reject_fdr}{Combined FDR rejection vector from all nodes}
#' \item{total_power_proxy}{Total number of significant assets across all nodes after FDR correction}
#'
#' @examples
#' set.seed(42)
#' T <- 120
#' N <- 100  # Larger dataset for distributed testing
#' K_factors <- 3
#' fac <- matrix(rnorm(T * K_factors), T, K_factors)
#' beta <- matrix(rnorm(N * K_factors), N, K_factors)
#' alpha <- rep(0, N)
#' alpha[1:10] <- 0.4 / 100  # 10 non-zero alphas
#' eps <- matrix(rnorm(T * N, sd = 0.02), T, N)
#' ret <- alpha + fac %*% t(beta) + eps
#' 
#' # Distributed testing with 4 nodes, each handling 25 assets
#' results <- Dfactor.tests(ret, fac, n1 = 25, K = 4, q.fdr = 0.05)
#' 
#' # View combined results
#' cat("Total significant assets after FDR:", results$total_power_proxy, "\n")
#' cat("Combined results across all nodes:\n")
#' print(summary(results$combined_alpha))
#'
#' @export
#' @importFrom stats lm residuals coef cov cor pchisq pf pnorm p.adjust
#' @importFrom MASS ginv
Dfactor.tests <- function(ret, fac, n1, K, q.fdr = 0.05) {
  T <- nrow(ret)
  N <- ncol(ret)
  K_factors <- ncol(fac)
  
  # Validate input parameters
  if (n1 * K > N) {
    stop("n1 * K cannot exceed total number of assets N")
  }
  
  # Initialize lists to store results from each node
  alpha_list <- list()
  tstat_list <- list()
  pval_list <- list()
  Wald_list <- list()
  p_Wald_list <- list()
  GRS_list <- list()
  p_GRS_list <- list()
  PY_list <- list()
  p_PY_list <- list()
  reject_fdr_list <- list()
  power_proxy_list <- list()
  
  # Create asset allocation matrix for distributed processing
  L <- matrix(rep(0, K * n1), ncol = n1)
  R <- matrix(0, n1, N)
  
  for (i in 1:K) {
    # Sample assets for current node
    L[i, ] <- sample(1:N, n1, replace = FALSE)
    r <- matrix(c(1:n1, L[i, ]), ncol = n1, byrow = TRUE)
    R[t(r)] <- 1
    
    # Extract asset returns for current node
    ret_node <- ret[, L[i, ]]
    
    # Perform factor model testing on node subset
    node_results <- .node.factor.tests(ret_node, fac, q.fdr)
    
    # Store node results
    alpha_list[[i]] <- node_results$alpha
    tstat_list[[i]] <- node_results$tstat
    pval_list[[i]] <- node_results$pval
    Wald_list[[i]] <- node_results$Wald
    p_Wald_list[[i]] <- node_results$p_Wald
    GRS_list[[i]] <- node_results$GRS
    p_GRS_list[[i]] <- node_results$p_GRS
    PY_list[[i]] <- node_results$PY
    p_PY_list[[i]] <- node_results$p_PY
    reject_fdr_list[[i]] <- node_results$reject_fdr
    power_proxy_list[[i]] <- node_results$power_proxy
  }
  
  # Combine results across all nodes
  combined_results <- .combine.node.results(alpha_list, pval_list, reject_fdr_list, 
                                            power_proxy_list, L, N, q.fdr)
  
  # Return comprehensive results
  return(list(
    alpha_list = alpha_list,
    tstat_list = tstat_list,
    pval_list = pval_list,
    Wald_list = Wald_list,
    p_Wald_list = p_Wald_list,
    GRS_list = GRS_list,
    p_GRS_list = p_GRS_list,
    PY_list = PY_list,
    p_PY_list = p_PY_list,
    reject_fdr_list = reject_fdr_list,
    power_proxy_list = power_proxy_list,
    combined_alpha = combined_results$combined_alpha,
    combined_pval = combined_results$combined_pval,
    combined_reject_fdr = combined_results$combined_reject_fdr,
    total_power_proxy = combined_results$total_power_proxy,
    asset_allocation = L
  ))
}

#' Helper function for node-level factor model testing
#' @noRd
.node.factor.tests <- function(ret_node, fac, q.fdr) {
  T <- nrow(ret_node)
  N_node <- ncol(ret_node)
  K <- ncol(fac)
  
  # Asset-by-asset OLS
  alpha <- numeric(N_node)
  tstat <- numeric(N_node)
  pval <- numeric(N_node)
  resid <- matrix(NA, T, N_node)
  
  for (j in 1:N_node) {
    mod <- lm(ret_node[, j] ~ fac)
    alpha[j] <- coef(mod)[1]
    resid[, j] <- residuals(mod)
    tstat[j] <- summary(mod)$coefficients[1, 3]
    pval[j] <- summary(mod)$coefficients[1, 4]
  }
  
  # Residual covariance matrices
  Sigma_u <- cov(resid)
  Sigma_u_inv <- tryCatch(solve(Sigma_u),
                          error = function(e) MASS::ginv(Sigma_u))
  D_hat <- diag(diag(Sigma_u))
  
  # Joint tests
  # Wald test
  Wald <- T * as.numeric(t(alpha) %*% Sigma_u_inv %*% alpha)
  p_Wald <- pchisq(Wald, df = N_node, lower.tail = FALSE)
  
  # GRS test
  dof_num <- N_node
  dof_den <- T - N_node - K
  if (dof_den <= 0) {
    GRS <- p_GRS <- NA_real_
  } else {
    fbar <- colMeans(fac)
    Omega <- cov(fac)
    adj <- 1 + as.numeric(t(fbar) %*% solve(Omega) %*% fbar)
    GRS <- (dof_den / dof_num) * (Wald / adj) / (T - K - 1)
    p_GRS <- pf(GRS, dof_num, dof_den, lower.tail = FALSE)
  }
  
  # PY test
  kappa <- 1 + (2/N_node) * sum(cor(resid)^2)
  PY_num <- as.numeric(t(alpha) %*% solve(D_hat) %*% alpha) - N_node
  PY <- PY_num / sqrt(2 * N_node * kappa)
  p_PY <- pnorm(PY, lower.tail = FALSE)
  
  # FDR on asset-level p-values
  bh <- p.adjust(pval, method = "BH")
  reject_fdr <- bh <= q.fdr
  power_proxy <- sum(reject_fdr)
  
  return(list(
    alpha = alpha,
    tstat = tstat,
    pval = pval,
    Wald = Wald,
    p_Wald = p_Wald,
    GRS = GRS,
    p_GRS = p_GRS,
    PY = PY,
    p_PY = p_PY,
    reject_fdr = reject_fdr,
    power_proxy = power_proxy
  ))
}

#' Helper function to combine results from all nodes
#' @noRd
.combine.node.results <- function(alpha_list, pval_list, reject_fdr_list, 
                                  power_proxy_list, L, N, q.fdr) {
  K <- length(alpha_list)
  
  # Initialize combined vectors
  combined_alpha <- rep(NA, N)
  combined_pval <- rep(NA, N)
  combined_reject_fdr <- rep(FALSE, N)
  
  # Combine results from all nodes
  for (i in 1:K) {
    assets <- L[i, ]
    combined_alpha[assets] <- alpha_list[[i]]
    combined_pval[assets] <- pval_list[[i]]
    combined_reject_fdr[assets] <- reject_fdr_list[[i]]
  }
  
  # Apply global FDR correction across all assets
  global_bh <- p.adjust(combined_pval, method = "BH")
  global_reject_fdr <- global_bh <= q.fdr
  total_power_proxy <- sum(global_reject_fdr)
  
  return(list(
    combined_alpha = combined_alpha,
    combined_pval = combined_pval,
    combined_reject_fdr = global_reject_fdr,
    total_power_proxy = total_power_proxy
  ))
}