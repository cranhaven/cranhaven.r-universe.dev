calculate_lsq2 <- function(U, k, m) {
  Qm <- count.grid(U, k, m)
  Am <- (m*k) * (Qm / sum(Qm))
  
  alpha <- alpha_logis(dim(U)[1] / (k*m))
  target <- alpha * as.vector(Am) + (1 - alpha) * rep(1, k*m)
  
  Quad.m <- diag(k*m)
  Quad.v <- target
  
  Eq.m <- matrix(0, nrow = k + m, ncol = k*m)
  for(i in 1:m) {
    Eq.m[i, ((i-1)*k+1):(i*k)] <- 1
  }
  for(i in 1:k) {
    Eq.m[m+i, seq(i, m*k, by = k)] <- 1
  }
  Eq.v <- c(rep(k, m), rep(m, k))
  
  Eq.m <- Eq.m[-nrow(Eq.m), ]
  Eq.v <- Eq.v[-length(Eq.v)]
  
  Ineq.m <- rbind(diag(k*m), -diag(k*m))
  Ineq.v <- c(rep(0, k*m), -rep(min(k, m), k*m))
  
  value <- limSolve::lsei(
    A = Quad.m, B = Quad.v,
    E = Eq.m, F = Eq.v,
    G = Ineq.m, H = Ineq.v,
    type = 1
  )
  
  Dm <- matrix(value$X, nrow = k, ncol = m)
  
  list(
    Density = Dm,
    Quantity = Qm,
    alpha = alpha,
    m = m,
    k = k
  )
}
