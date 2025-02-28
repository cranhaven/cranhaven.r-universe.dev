library(CVXR)
library(quadprog)

solve_subproblem_B_cvx <- function(H) {
  q <- nrow(H)
  r <- ncol(H)
  ones_q <- rep(1, q)
  ones_r <- rep(1, r)
  B_cvx <- CVXR::Variable(r, q, nonneg = TRUE)
  objective <- CVXR::norm(B_cvx, "F")^2 -2*CVXR::matrix_trace(B_cvx %*% H)
  constraints <- list(B_cvx %*% ones_q == ones_r)
  prob <- CVXR::Problem(CVXR::Minimize(objective), constraints)
  cvx_res <- CVXR::solve(prob)
  B <- cvx_res$getValue(B_cvx)
  B[abs(B) < 1e-8] <- 0
  return(B)
}

solve_subproblem_B_quadprog <- function(H) {
  r <- ncol(H)
  q <- nrow(H)
  ones_q <- rep(1, q)
  diag_q <- diag(q)
  ones_r <- rep(1, r)
  Amat <- t(rbind(ones_q, diag_q))
  bvec <- c(1, rep(0, q))
  B <- c()
  for (i in 1:r) {
    B <- rbind(B, quadprog::solve.QP(Dmat = diag_q, dvec = H[, i], Amat = Amat, bvec = bvec, meq = 1)$solution)
  }
  return(B)
}

compute_obj_fun_subproblem_B <- function(B, C) {
  ones_r <- rep(1, nrow(B))
  return(.5 * norm(B, "F")^2 + .25 * t(ones_r) %*% B %*% t(B) %*% ones_r
         - sum(B * t(C)))
}
