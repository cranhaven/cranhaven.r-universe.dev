
col_logsumexp <- function(mat) {
  maxes <- apply(mat,2,max)
  return(
    maxes + log(colSums(exp(sweep(mat, 2, maxes))))
  )
}

row_logsumexp <- function(mat) {
  maxes <- apply(mat,1,max)
  return(
    maxes + log(rowSums(exp(sweep(mat, 1, maxes))))
  )
}

log_sinkhorn_test <- function(mass_x, mass_y, cost = NULL, eps = 0.05, niterations = 100){
  update_g <- function(cost, f, lambda, log_b) {
    -lambda * col_logsumexp(-sweep(cost, 1, f)/lambda) - lambda * log_b
  }
  update_f <- function(cost, g, lambda, log_a) {
    -lambda * row_logsumexp(-sweep(cost, 2, g)/lambda) - lambda * log_a
  }
  converge_log <- function(pot, pot_old, tol) {
    return(isTRUE(sum(abs(pot - pot_old)/abs(pot_old)) < tol))
  }
  n1 <- length(mass_x)
  n2 <- length(mass_y)
  log_x <- log(mass_x)
  log_y <- log(mass_y)
  
  # costp <- cost^p
  lambda <- eps * stats::median(cost)
  f_old <- rep(0,n1)
  g_old <- rep(0,n2)
  
  f <- -lambda * row_logsumexp(-cost/lambda) + lambda * log_x
  g <- update_g(cost, f, lambda, log_y)
  for( i in 1:niterations) {
    f <- update_f(cost, g, lambda, log_x)
    g <- update_g(cost, f, lambda, log_y)
    
    if (converge_log(f, f_old, tol = 1e-8)) {
      break
    } else {
      f_old <- f
      g_old <- g
    }
  }
  
  return(list(f = f, g = g))
}


col_softmin <- function(mat) {
  -log(colSums(exp(mat)))
}

row_softmin <- function(mat) {
  -log(rowSums(exp(mat)))
}

log_sinkhorn_test_nomax <- function(mass_x, mass_y, cost = NULL, eps = 0.05, niterations = 100){
  generate_S <- function(cost, f, g, lambda) {
    S <- -sweep(sweep(cost, 1, f), 2, g)/lambda
    return(S)
  }
  update_g <- function(cost, f, g, lambda, log_a, log_b) {
    S <- generate_S(cost, f, g, lambda)
    return(
      lambda * col_softmin(S) + g + lambda * log_b
    )
  }
  update_f <- function(cost, f, g, lambda, log_a, log_b) {
    S <- generate_S(cost, f, g, lambda)
    return(
      lambda * row_softmin(S) + f + lambda * log_a
    )
  }
  converge_log <- function(pot, pot_old, tol) {
    return(isTRUE(sum(abs(pot - pot_old))/sum(abs(pot_old)) < tol))
  }
  n1 <- length(mass_x)
  n2 <- length(mass_y)
  log_x <- log(mass_x)
  log_y <- log(mass_y)
  
  # costp <- cost^p
  lambda <- eps * stats::median(cost)
  f <- -lambda * row_logsumexp(-cost/lambda) + lambda * log_x
  f_old <- rep(0,n1)
  g <- -lambda * col_logsumexp(-sweep(cost,1,f)/lambda) + lambda * log_y
  g_old <- rep(0,n2)
  
  for( i in 1:niterations) {
    f <- update_f(cost, f, g, lambda, log_x, log_y)
    g <- update_g(cost, f, g, lambda, log_x, log_y)
    
    if(any(is.nan(f)) || any(is.nan(g))) browser()
    if (converge_log(f, f_old, tol = 1e-8)) {
      break
    } else {
      f_old <- f
      g_old <- g
    }
  }
  
  return(list(f = f, g = g))
}

log_sinkhorn_test_nomax_KL <- function(mass_x, mass_y, cost = NULL, eps = 0.05, niterations = 100){
  generate_S <- function(cost, f, g, lambda) {
    S <- -sweep(sweep(cost, 1, f), 2, g)/lambda
    return(S)
  }
  update_g <- function(cost, f, g, lambda, log_a, log_b) {
    S <- generate_S(cost, f, g, lambda)
    return(
      lambda * col_softmin(sweep(S,1,log_a, "+")) + g 
    )
  }
  update_f <- function(cost, f, g, lambda, log_a, log_b) {
    S <- generate_S(cost, f, g, lambda)
    return(
      lambda * row_softmin(sweep(S,2,log_b, "+")) + f
    )
  }
  converge_log <- function(pot, pot_old, tol) {
    return(isTRUE(sum(abs(pot - pot_old))/sum(abs(pot_old)) < tol))
  }
  n1 <- length(mass_x)
  n2 <- length(mass_y)
  log_x <- log(mass_x)
  log_y <- log(mass_y)
  
  # costp <- cost^p
  lambda <- eps * stats::median(cost)
  f <- -lambda * row_logsumexp(sweep(-cost/lambda,2,log_y,"+")) 
  f_old <- rep(0,n1)
  g <- -lambda * col_logsumexp(sweep(-sweep(cost,1,f)/lambda,1,log_x,"+"))
  g_old <- rep(0,n2)
  
  for( i in 1:niterations) {
    f <- update_f(cost, f, g, lambda, log_x, log_y)
    g <- update_g(cost, f, g, lambda, log_x, log_y)
    
    if(any(is.nan(f)) || any(is.nan(g))) browser()
    if (converge_log(f, f_old, tol = 1e-8)) {
      break
    } else {
      f_old <- f
      g_old <- g
    }
  }
  
  return(list(f = f, g = g))
}


#' Round transportation matrix to feasible set
#'
#' @param transport_matrix A transportation matrix returned by an approximate method
#' @param mass_x The distribution of the first margin
#' @param mass_y The distribution of the second margin
#'
#' @return Returns a transportation matrix projected to the feasible set.
#' @keywords internal
round_transport_matrix <- function(transport_matrix, mass_x, mass_y) {
  # set.seed(32423)
  # n <- 100
  # d <- 10
  # x <- matrix(rnorm(d*n), nrow=d, ncol=n)
  # y <- matrix(rnorm(d*n), nrow=d, ncol=n)
  # mass_x <- rep(1/n,n)
  # mass_y <- rep(1/n,n)
  # cost <- approxOT::cost_calc(x,y, 2.0)
  # tpot <- sinkhorn_pot(mass_x, mass_y, p = 2,
  #            cost=cost)
  # tmat <- exp(sweep(sweep(cost^2,1,tpot$f ),2,tpot$g)/(0.05 * median(cost^2)))
  # tmat <- tmat/sum(tmat)
  # 
  # rounded <- approxOT:::round_transport_matrix(tmat, mass_x = mass_x,
  # mass_y = mass_y)
  # all.equal(rowSums(rounded), mass_x)
  transport_matrix <- as.matrix(transport_matrix)
  stopifnot(nrow(transport_matrix) == length(mass_x))
  stopifnot(ncol(transport_matrix) == length(mass_y))
  a <- as.double(mass_x)
  b <- as.double(mass_y)
  
  
  tmat <- round_2_feasible_(transport_matrix, a, b)
  return(tmat)
}