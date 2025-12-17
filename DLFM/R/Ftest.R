#' @name Ftest
#' @title Apply the Farmtest method to the Laplace factor model
#' @description This function simulates data from a Lapalce factor model and applies the FarmTest
#' for multiple hypothesis testing. It calculates the false discovery rate (FDR)
#' and power of the test.
#' @param data A matrix or data frame of simulated or observed data from a Laplace factor model.
#' @param p1 The number or proportion of non-zero hypotheses.
#' @param alpha The significance level for controlling the false discovery rate (default: 0.05).
#' @param K The number of factors to estimate (default: -1, meaning auto-detect).
#' @param alternative The alternative hypothesis: "two.sided", "less", or "greater" (default: "two.sided").
#' @return A list containing the following elements:
#' \item{FDR}{The false discovery rate, which is the proportion of false positives among all discoveries (rejected hypotheses).}
#' \item{Power}{The statistical power of the test, which is the probability of correctly rejecting a false null hypothesis.}
#' \item{PValues}{A vector of p-values associated with each hypothesis test.}
#' \item{RejectedHypotheses}{The total number of hypotheses that were rejected by the FarmTest.}
#' \item{reject}{Indices of rejected hypotheses.}
#' \item{means}{Estimated means.}
#' @examples
#' library(LaplacesDemon)
#' library(MASS)
#' n=1000
#' p=10
#' m=5
#' mu=t(matrix(rep(runif(p,0,1000),n),p,n))
#' mu0=as.matrix(runif(m,0))
#' sigma0=diag(runif(m,1))
#' F=matrix(mvrnorm(n,mu0,sigma0),nrow=n)
#' A=matrix(runif(p*m,-1,1),nrow=p)
#' lanor <- rlaplace(n*p,0,1)
#' epsilon=matrix(lanor,nrow=n)
#' D=diag(t(epsilon)%*%epsilon)
#' data=mu+F%*%t(A)+epsilon
#' p1=40
#' results <- Ftest(data, p1)
#' print(results$FDR)
#' print(results$Power)
#' @export
Ftest <- function(data, p1, alpha = 0.05, K = -1, alternative = c("two.sided", "less", "greater")) {
  
  alternative <- match.arg(alternative)
  
  # Check if p1 is proportion or count
  if (p1 < 1 && p1 > 0) {
    # p1 is a proportion, convert to count
    p1_count <- round(p1 * ncol(data))
  } else {
    # p1 is a count
    p1_count <- p1
  }
  
  # Apply FarmTest (simplified version)
  output <- simplified_farm_test(data, alpha, K, alternative)
  
  # Calculate FDR and power based on p1_count non-zero hypotheses
  # Assuming first p1_count hypotheses are non-zero (for simulation purposes)
  n_hypotheses <- ncol(data)
  
  if (p1_count > n_hypotheses) {
    p1_count <- n_hypotheses
  }
  
  # Get rejected hypotheses indices
  rejected_indices <- output$reject
  n_rejected <- length(rejected_indices)
  
  # Calculate FDR and Power
  if (n_rejected > 0) {
    # False discoveries: rejected among the null hypotheses (indices > p1_count)
    false_discoveries <- sum(rejected_indices > p1_count)
    fdr <- false_discoveries / n_rejected
    
    # True discoveries: rejected among the alternative hypotheses (indices <= p1_count)
    true_discoveries <- sum(rejected_indices <= p1_count)
    power <- true_discoveries / p1_count
  } else {
    fdr <- 0
    power <- 0
  }
  
  # Return the results as a list
  return(list(
    FDR = fdr,
    Power = power,
    PValues = output$pValues,
    RejectedHypotheses = n_rejected,
    reject = rejected_indices,
    means = output$means
  ))
}

# Simplified implementation of farm.test
simplified_farm_test <- function(X, alpha = 0.05, K = -1, 
                                 alternative = c("two.sided", "less", "greater")) {
  
  alternative <- match.arg(alternative)
  n <- nrow(X)
  p <- ncol(X)
  
  # Estimate means using robust Huber-like approach
  means <- apply(X, 2, function(x) {
    # Simple robust mean estimation (trimmed mean)
    stats::quantile(x, 0.5)  # Using median as robust estimator
  })
  
  # Estimate factor model if K != 0
  if (K != 0) {
    # Simple factor estimation using PCA
    if (K < 0) {
      # Auto-detect number of factors
      K <- estimate_number_of_factors(X)
    }
    
    if (K > 0 && K < min(n, p)) {
      # Perform PCA
      pca_result <- stats::prcomp(X, center = TRUE, scale. = FALSE)
      
      # Get factor loadings and scores
      loadings <- pca_result$rotation[, 1:K, drop = FALSE]
      scores <- pca_result$x[, 1:K, drop = FALSE]
      
      # Remove factor effects
      X_resid <- X - scores %*% t(loadings)
    } else {
      X_resid <- X
      loadings <- matrix(0, p, 0)
      K <- 0
    }
  } else {
    X_resid <- X
    loadings <- matrix(0, p, 0)
  }
  
  # Calculate test statistics
  if (K == 0) {
    # Without factor adjustment
    resid_means <- means
    resid_sd <- apply(X_resid, 2, function(x) {
      # Robust scale estimation using MAD
      stats::mad(x, constant = 1)
    })
  } else {
    # With factor adjustment
    resid_means <- apply(X_resid, 2, stats::median)
    resid_sd <- apply(X_resid, 2, function(x) stats::mad(x, constant = 1))
  }
  
  # Calculate t-statistics
  t_stat <- resid_means / (resid_sd / sqrt(n))
  
  # Calculate p-values based on alternative hypothesis
  if (alternative == "two.sided") {
    p_values <- 2 * (1 - stats::pt(abs(t_stat), df = n - 1))
  } else if (alternative == "less") {
    p_values <- stats::pt(t_stat, df = n - 1)
  } else { # "greater"
    p_values <- 1 - stats::pt(t_stat, df = n - 1)
  }
  
  # Adjust p-values using Benjamini-Hochberg method
  p_adjust <- stats::p.adjust(p_values, method = "BH")
  
  # Determine significant hypotheses
  significant <- as.integer(p_adjust <= alpha)
  
  # Get indices of rejected hypotheses
  reject <- which(significant == 1)
  if (length(reject) == 0) {
    reject <- "no hypotheses rejected"
  }
  
  # Return results
  return(list(
    means = means,
    stdDev = resid_sd,
    loadings = loadings,
    nFactors = K,
    tStat = t_stat,
    pValues = p_values,
    pAdjust = p_adjust,
    significant = significant,
    reject = reject,
    type = ifelse(K == 0, "no factor", "unknown"),
    n = n,
    p = p,
    h0 = rep(0, p),
    alpha = alpha,
    alternative = alternative
  ))
}

# Helper function to estimate number of factors
estimate_number_of_factors <- function(X) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Simple eigenvalue ratio method
  if (n > 1 && p > 1) {
    # Center the data
    X_centered <- scale(X, scale = FALSE)
    
    # Calculate covariance matrix
    Sigma <- stats::cov(X_centered)
    
    # Eigen decomposition
    eigen_vals <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
    
    # Calculate ratios of consecutive eigenvalues
    ratios <- eigen_vals[-length(eigen_vals)] / eigen_vals[-1]
    
    # Estimate number of factors (simple heuristic)
    # Use eigenvalues greater than average
    avg_eigen <- mean(eigen_vals)
    K <- sum(eigen_vals > avg_eigen)
    
    # Ensure K is reasonable
    K <- min(K, floor(min(n, p) / 2))
  } else {
    K <- 0
  }
  
  return(max(0, K))
}