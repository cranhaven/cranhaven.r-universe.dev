# sinkhorn_distance <- function(x1, x2, p = 1, ground_p = 2, eps = 0.05, niter = 100){
#   w1 <- rep(1/ncol(x1), ncol(x1))
#   w2 <- rep(1/ncol(x2), ncol(x2))
#   C <- cost_calc(x1, x2, ground_p)
#   epsilon <- eps * median(C^p)
#   wass <- sinkhorn_(w1, w2, C^p, epsilon, niter)
#   ### CORRECTION OF THE MARGINALS
#   # explained in the appendix of Coupling of Particle Filters, Jacob Lindsten Schon  (arXiv v2 appendix E)
#   Phat <- wass$transportmatrix
#   u <- rowSums(Phat)
#   utilde <- colSums(Phat)
#   alpha <- min(pmin(w1/u, w2/utilde))
#   r <- (w1 - alpha * u) / (1 - alpha)
#   rtilde <- (w2 - alpha * utilde) / (1 - alpha)
#   P <- alpha * Phat + (1 - alpha) * matrix(r, ncol = 1) %*% matrix(rtilde, nrow = 1)
#   return(list(uncorrected = (sum(Phat * C^p))^(1/p), corrected = (sum(P * C^p))^(1/p)))
# }

sinkhorn_distance <- function(mass_x, mass_y, cost = NULL, p = 1, eps = 0.05, niter = 100){
  costp <- cost^p
  epsilon <- eps * stats::median(costp)
  wass <- sinkhorn_(p_ = mass_x, q_ = mass_y, 
                    cost_matrix_ = costp, epsilon = epsilon, niterations = niter)
  ### CORRECTION OF THE MARGINALS
  # explained in the appendix of Coupling of Particle Filters, Jacob Lindsten Schon  (arXiv v2 appendix E)
  Phat <- wass$transportmatrix
  u <- rowSums(Phat)
  utilde <- colSums(Phat)
  alpha <- min(pmin(mass_x/u, mass_y/utilde))
  r <- (mass_x - alpha * u) / (1 - alpha)
  rtilde <- (mass_y - alpha * utilde) / (1 - alpha)
  P <- if ( alpha < 1 ) {
    alpha * Phat + (1 - alpha) * matrix(r, ncol = 1) %*% matrix(rtilde, nrow = 1)
  } else {
    Phat
  }
  return(list(uncorrected = (sum(Phat * costp))^(1/p), corrected = (sum(P * costp))^(1/p)))
}

sinkhorn_transport <- function(mass_x, mass_y, cost = NULL, eps = 0.05, niterations = 100){
  n1 <- length(mass_x)
  n2 <- length(mass_y)
  # costp <- cost^p
  epsilon <- eps * stats::median(cost)
  transp <- sinkhorn_(mass_x, mass_y, cost, epsilon, niterations)
  ### CORRECTION OF THE MARGINALS
  # explained in the appendix of Coupling of Particle Filters, Jacob Lindsten Schon  (arXiv v2 appendix E)
  Phat <- transp$transportmatrix
  u <- rowSums(Phat)
  utilde <- colSums(Phat)
  alpha <- min(pmin(mass_x/u, mass_y/utilde))
  r <- (mass_x - alpha * u) / (1 - alpha)
  rtilde <- (mass_y - alpha * utilde) / (1 - alpha)
  P <- alpha * Phat + (1 - alpha) * matrix(r, ncol = 1) %*% matrix(rtilde, nrow = 1)
  return(list(from = rep(1:n1, n2), 
              to = rep(1:n1, each = n2), 
              mass = c(P)))
}
