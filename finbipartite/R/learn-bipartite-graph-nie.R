library(spectralGraphTopology)
library(quadprog)

#' @title Laplacian matrix of a k-component bipartite graph via Nie's method
#'
#' Computes the Laplacian matrix of a bipartite graph on the basis of an observed similarity matrix.
#'
#' @param S a p x p similarity matrix, where p is the number of nodes in the graph.
#' @param r number of nodes in the objects set.
#' @param q number of nodes in the classes set.
#' @param k number of components of the graph.
#' @param learning_rate gradient descent parameter.
#' @param eta rank constraint hyperparameter.
#' @param maxiter maximum number of iterations.
#' @param reltol relative tolerance as a convergence criteria.
#' @param verbose whether or not to show a progress bar during the iterations.
#' @param record_objective whether or not to record the objective function value during iterations.
#' @return A list containing possibly the following elements:
#' \item{\code{laplacian}}{estimated Laplacian matrix}
#' \item{\code{adjacency}}{estimated adjacency matrix}
#' \item{\code{B}}{estimated graph weights matrix}
#' \item{\code{maxiter}}{number of iterations taken to reach convergence}
#' \item{\code{convergence}}{boolean flag to indicate whether or not the optimization converged}
#' \item{\code{obj_fun}}{objective function value per iteration}
#' @references Feiping Nie, Xiaoqian Wang, Cheng Deng, Heng Huang.
#'             "Learning A Structured Optimal Bipartite Graph for Co-Clustering".
#'             Advances in Neural Information Processing Systems (NIPS 2017)
#' @examples
#' library(finbipartite)
#' library(igraph)
#' set.seed(42)
#' r <- 50
#' q <- 5
#' p <- r + q
#'
#' bipartite <- sample_bipartite(r, q, type="Gnp", p = 1, directed=FALSE)
#' # randomly assign edge weights to connected nodes
#' E(bipartite)$weight <- 1
#' Lw <- as.matrix(laplacian_matrix(bipartite))
#' B <- -Lw[1:r, (r+1):p]
#' B[,] <- runif(length(B))
#' B <- B / rowSums(B)
#' # utils functions
#' from_B_to_laplacian <- function(B) {
#'   A <- from_B_to_adjacency(B)
#'   return(diag(rowSums(A)) - A)
#' }
#'
#' from_B_to_adjacency <- function(B) {
#'   r <- nrow(B)
#'   q <- ncol(B)
#'   zeros_rxr <- matrix(0, r, r)
#'   zeros_qxq <- matrix(0, q, q)
#'   return(rbind(cbind(zeros_rxr, B), cbind(t(B), zeros_qxq)))
#' }
#' Ltrue <- from_B_to_laplacian(B)
#' X <- MASS::mvrnorm(100*p, rep(0, p), MASS::ginv(Ltrue))
#' S <- cov(X)
#' bipartite_graph <- learn_bipartite_graph_nie(S = S,
#'                                              r = r,
#'                                              q = q,
#'                                              k = 1,
#'                                              learning_rate = 5e-1,
#'                                              eta = 0,
#'                                              verbose=FALSE)
#' @export
#' @import spectralGraphTopology
#' @import quadprog
learn_bipartite_graph_nie <- function(S,
                                      r,
                                      q,
                                      k,
                                      learning_rate = 1e-4,
                                      eta = 1,
                                      maxiter = 1000,
                                      reltol = 1e-6,
                                      verbose = TRUE,
                                      record_objective = FALSE) {
  # number of nodes
  p <- r + q
  ones_r <- rep(1, r)
  # Laplacian initialization
  L_ <- L(spectralGraphTopology:::w_init("naive", MASS::ginv(S)))
  # B initialization
  B <- as.matrix(-L_[1:r, (r+1):p] + 1e-5)
  B <- B / rowSums(B)
  Srq <- project_onto_simplex(-L_[1:r, (r+1):p])
  L_ <- from_B_to_laplacian(Srq)
  obj_seq <- c()
  if (verbose)
    pb <- progress::progress_bar$new(format = "<:bar> :current/:total  eta: :eta",
                                     total = maxiter, clear = FALSE, width = 80)
  for (i in 1:maxiter) {
    # update vector
    V <- eigen(L_, symmetric = TRUE)$vectors[, (p-k+1):p]
    VVt <- V %*% t(V)
    grad_B <- (B - Srq) + eta * (0.5*ones_r %*% t(diag(VVt)[(r+1):p]) - VVt[1:r, (r+1):p])
    B_update <- project_onto_simplex(B - learning_rate * grad_B)
    L_ <- from_B_to_laplacian(B_update)
    obj_seq <- c(obj_seq, compute_obj_fun_nie(B_update, Srq, V, L_, eta))
    if (k > 1) {
        n_zero_eigvals <- sum(eigen(L_)$values < 1e-8)
        if (n_zero_eigvals < k) {
            eta <- eta * 2
        } else if (n_zero_eigvals > k){
            eta <- 0.5 * eta
        }
        if (n_zero_eigvals == k) {
            has_converged = TRUE
            break
        }
        if (eta > 1e3) {
            eta <- 1e3
        }
    }
    has_converged = (norm(B - B_update, 'F')/norm(B, 'F') < reltol) && (i > 1)
    B <- B_update
    if (verbose)
      pb$tick()
    if (has_converged)
      break
  }
  B_update[B_update < 1e-5] <- 0
  results <- list(laplacian = from_B_to_laplacian(B_update),
                  adjacency = from_B_to_adjacency(B_update),
                  B = B_update,
                  maxiter = i,
                  convergence = has_converged,
                  obj_fun = obj_seq)
  return(results)
}

compute_obj_fun_nie <- function(B, Srq, V, L_, eta) {
    return(norm(B - Srq, "F")^2 + eta * sum(diag(t(V) %*% L_ %*% V)))
}
