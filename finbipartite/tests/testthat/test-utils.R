set.seed(42)
library(igraph)
library(CVXR)

test_that("test from_B_to_adjacency and from_B_to_laplacian", {
  r <- 3
  q <- 2
  p <- r + q
  bipartite <- sample_bipartite(r, q, type="Gnp", p = 0.5, directed=FALSE)
  E(bipartite)$weight <- runif(gsize(bipartite), min = 2, max = 5)
  Ltrue <- as.matrix(laplacian_matrix(bipartite))
  Atrue <- diag(diag(Ltrue)) - Ltrue
  B <- -Ltrue[1:r, (r+1):p]
  expect_true(sum(abs(Ltrue - finbipartite:::from_B_to_laplacian(B))) < 1e-10)
  expect_true(sum(abs(Atrue - finbipartite:::from_B_to_adjacency(B))) < 1e-10)
})

test_that("solution for subproblem B is indeed optimal", {
  r <- 3
  q <- 2
  K <- diag(r) + .5 * matrix(1, r, r)
  C <- matrix(runif(r * q), q, r)
  B <- matrix(runif(r * q), r, q)
  B <- B / rowSums(B)
  obj_fun <- c(finbipartite:::compute_obj_fun_subproblem_B(B, C))
  for (i in 1:10) {
    H <- t(B) + 2 * (C - t(B) %*% K) / (2 + r)
    B <- finbipartite:::solve_subproblem_B_cvx(H)
    obj_fun <- c(obj_fun, finbipartite:::compute_obj_fun_subproblem_B(B, C))
  }
  expect_true(all((obj_fun[-1] - obj_fun[-length(obj_fun)]) <= 0))
})

test_that("solve_subproblem_B_cvx and solve_subproblem_B_quadprog agree", {
  r <- 3
  q <- 2
  K <- diag(r) + .5 * matrix(1, r, r)
  C <- matrix(runif(r * q), q, r)
  B <- matrix(runif(r * q), r, q)
  B <- B / rowSums(B)
  obj_fun <- c(finbipartite:::compute_obj_fun_subproblem_B(B, C))
  for (i in 1:10) {
    H <- t(B) + 2 * (C - t(B) %*% K) / (2 + r)
    B_cvx <- finbipartite:::solve_subproblem_B_cvx(H)
    B_quadprog <- finbipartite:::solve_subproblem_B_quadprog(H)
    expect_true(sum(abs(B_cvx - B_quadprog) / sum(abs(B_quadprog))) < 1e-4)
  }
})
