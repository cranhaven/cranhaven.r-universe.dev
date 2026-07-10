f <- function(t, A, omega, cos = FALSE) {
  if (cos) {
    return(as.vector(-A * cos(2 * pi * t / omega)))
  }
  as.vector(A * sin(2 * pi * t / omega))
}

cst_cor <- function(n, rho) {
  m <- matrix(rho, n, n)
  diag(m) <- 1
  return(m)
}

#' Simulate PLSMM
#'
#' Simulate a partial linear semiparametric mixed-effects model.
#'
#' @param N Number of subjects.
#' @param n_mvnorm Number of covariate generates from a multivariate normal distribution.
#' @param grouped Logical indicating whether to include grouping effect.
#' @param timepoints Vector specifying timepoints for each subject.
#' @param nonpara_inter Logical indicating whether the nonparametric function is specific to each group.
#' @param sample_from Vector of time points to sample from.
#' @param cos Logical indicating whether to use cosine function for second group.
#' @param A_vec Vector of amplitudes for the nonlinear functions for each group.
#' @return A list with three components: 
#'   \item{sim}{Simulated data frame.}
#'   \item{phi}{Individual random intercepts.}
#'   \item{f_val}{Values of the nonlinear functions.}
#' @export
#' @examples
#' 
#' simulate_group_inter(
#'   N = 50, n_mvnorm = 100, grouped = TRUE,
#'   timepoints = 3:5, nonpara_inter = TRUE,
#'   sample_from = seq(0, 10, by = 0.1), cos = FALSE, A_vec = c(1, 1.5)
#' )
#'
simulate_group_inter <- function(N = 50, n_mvnorm = 100, grouped = TRUE,
                                 timepoints = 3:5, nonpara_inter = TRUE,
                                 sample_from, cos = FALSE, A_vec = c(1, 1.5)) {
  if (nonpara_inter) {
    A <- c(A_vec[1], A_vec[2])
    omega <- c(60, 110)
    
    f0_mean <- mean(f(sample_from, A[1], omega[1], cos = FALSE))
    f1_mean <- mean(f(sample_from, A[2], omega[2], cos = cos))
  } else {
    A <- c(A_vec[1], A_vec[1])
    omega <- c(60, 60)
    f0_mean <- mean(f(sample_from, A[1], omega[1], cos = FALSE))
    f1_mean <- f0_mean
  }
  
  phi <- stats::rnorm(N, 0, sqrt(0.5))
  
  y <- NULL
  sim <- NULL
  f_val <- NULL
  
  for (i in 1:N) {
    ni <- sample(timepoints, 1)

    
    if (grouped) {
      theta <- c(3, 2, 1)
    } else {
      theta <- c(0, 2, 0)
    }
    
    group <- rep(sample(c(0, 1), 1), ni)
    
    x1 <- rep(stats::rnorm(1, 1, sqrt(0.5)), ni)
    
    eps <- stats::rnorm(ni, 0, sqrt(0.2))
    
    t <- sort(sample(sample_from, ni, replace = F))
    
    if (group[1] == 0) {
      sim <- rbind(sim, cbind(
        rep(i, ni), t, phi[i] + f(t, A[1], omega[1], cos = FALSE) + eps - f0_mean,
        group, x1
      ))
      
      f_val <- c(f_val, f(t, A[1], omega[1], cos = FALSE) - f0_mean)
    } else {
      sim <- rbind(sim, cbind(
        rep(i, ni), t, phi[i] + f(t, A[2], omega[2], cos = cos) + eps - f1_mean,
        group, x1
      ))
      
      f_val <- c(f_val, f(t, A[2], omega[2], cos = cos) - f1_mean)
    }
  }
  
  x <- MASS::mvrnorm(nrow(sim), rep(0, n_mvnorm + 1), cst_cor(n_mvnorm + 1, 0))
  
  sim <- cbind(sim, x)
  
  colnames(sim) <- c("series", "t", "y", "group", paste0("x", 1:(ncol(x) + 1)))
  
  sim[, "x2"] <- sim[, "group"] * sim[, "x1"]
  sim[, "y"] <- sim[, "y"] + sim[, c("group", "x1", "x2"), drop = F] %*% theta
  
  sim <- as.data.frame(sim)
  
  phi <- rep(phi, table(sim$series))
  
  sim <- sim[order(sim$series, sim$t), ]
  
  f_val <- f_val[order(sim$series, sim$t)]
  
  phi <- phi[order(sim$series, sim$t)]
  
  return(list(sim = sim, phi = phi, f_val = f_val))
}