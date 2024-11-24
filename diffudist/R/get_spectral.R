#' @title Laplacian Spectral Decomposition
#'
#' @description
#' Returns the eigenvalue spectrum together with eigenvectors of a Laplacian
#' corresponding to a network. This involves computing the eigendecomposition of
#' a (symmetric) matrix, so it is computationally intense and may take some time.
#' The decomposition of the normalized Laplacian \eqn{L = I - D^{-1}A} takes
#' is computed through the decomposition of its symmetric version
#' \eqn{L = D^{-\frac{1}{2}}AD^{-\frac{1}{2}}}. See the package vignette for details.
#' @param g the network in the [igraph] format
#' @param type the Laplacian type, default "Normalized Laplacian".
#'   At the moment this is the only available option. For other types of Laplacians
#'   one should get autonomously the eigendecomposition, e.g. using \link[base]{eigen}.
#'   See the package vignette for an example.
#' @param verbose whether warnings have to be printed or not
#' @return lambdas the eigenvalues of the Laplacian
#' @return `u_L` the matrix of left eigenvectors (rows)
#' @return `u_R` the matrix of right eigenvectors (columns)
#' @seealso \code{\link{get_laplacian}} \code{\link{get_ddm_from_eigendec}}
#' @references Bertagnolli, G., & De Domenico, M. (2021). Diffusion geometry of multiplex and
#'   interdependent systems. Physical Review E, 103(4), 042301.
#'   \doi{10.1103/PhysRevE.103.042301}
#'   \href{https://arxiv.org/abs/2006.13032}{arXiv: 2006.13032}
#' @rdname getSpectralDecomp
#' @export
get_spectral_decomp <- function(g, type = "Normalized Laplacian", verbose = FALSE) {
  if (!igraph::is_connected(g)) {
    stop("Non connected network, cannot compute/decompose normalised Laplacian")
  }
  if (is.null(igraph::E(g)$weight)) {
    if (verbose) {
      cat("Unweighted network\n")
    }
    # igraph::E(g)$weight <- 1
  }
  if (igraph::gsize(g) > 0) {
    # L = D - A
    L <- igraph::laplacian_matrix(g, sparse = FALSE)
    # rename D = D^{1/2}
    D <- sqrt(igraph::strength(g, mode = "out"))
    D <- diag(D)
    # D^{-1/2}
    D_inv <- 1 / sqrt(igraph::strength(g, mode = "out"))
    D_inv <- diag(D_inv)
    # (symmetric) normalised Laplacian
    L <- (D_inv %*% L) %*% D_inv
    s_dec <- eigen(L, symmetric = TRUE)
    u_L <- crossprod(s_dec$vectors, D)  # t(s_dec$vectors) %*% D
    u_R <- D_inv %*% s_dec$vectors
  } else {
    stop("Edge set is empty!")
  }
  return(list(
    "lambdas" = s_dec$values,
    "u_L" = u_L,
    "u_R" = u_R
  ))
}

#' @title Distance Matrix from Laplacian spectral decomposition
#' @description
#'    Returns the diffusion distance matrix when the spectrum (more precisely,
#'    the eigendecomposition) of the Laplacian is provided as
#'    input (useful to speed up batch calculations).
#'
#'    For instance, the random walk normalised Laplacian \eqn{I - D^{-1}A},
#'    which generates the classical continuous-time random walk over a network,
#'    can be easily and obtained from the spectral decomposition of the symmetric
#'    normalised Laplacian
#'    \eqn{\mathcal{L} = D^{-\frac{1}{2}} L D^{-\frac{1}{2}} = D^{-\frac{1}{2}} (D - A) D^{-\frac{1}{2}}}.
#'    More specifically,
#'    \eqn{\bar{L} = I - D^{-1} A = D^{-\frac{1}{2}} \mathcal{L} D^{\frac{1}{2}}}
#'    and, since \eqn{\mathcal{L}} is symmetric it can be decomposed into
#'    \eqn{\mathcal{L} = \sum_{l = 1}^N \lambda_l u_l u_l^T}, hence
#'    \deqn{\bar{L} = \sum_{l = 1}^N \lambda_l u^R_l u^L_l} where
#'    \eqn{u^L_l = u_l^T D^{\frac{1}{2}}} and \eqn{u^R_l = u_l D^{-\frac{1}{2}}}.
#'
#' @param tau diffusion time (scalar)
#' @param Q eigenvector matrix
#' @param Q_inv inverse of the eigenvector matrix
#' @param lambdas eigenvalues (vector)
#' @param verbose whether warnings have to be printed or not
#' @return The diffusion distance matrix \eqn{D_t}, a square numeric matrix
#'   of the Euclidean distances between the rows of the stochastic matrix
#'   \eqn{P(t) = e^{-\tau L}}, where \eqn{-L} is the Laplacian generating a
#'   continuous-time random walk (Markov chain) over the network.
#'   The matrix exponential is here computed using the given eigendecomposition
#'   of the Laplacian matrix \eqn{e^{-\tau L} = Q e^{-\tau \Lambda} Q^{-1}}.
#' @seealso \code{\link{get_spectral_decomp}}
#' @references Bertagnolli, G., & De Domenico, M. (2021). Diffusion geometry of multiplex and
#'   interdependent systems. Physical Review E, 103(4), 042301.
#'   \doi{10.1103/PhysRevE.103.042301}
#'   \href{https://arxiv.org/abs/2006.13032}{arXiv: 2006.13032}
#' @export
get_ddm_from_eigendec <- function(tau, Q, Q_inv, lambdas,
                                  verbose = FALSE) {
  Nodes <- length(lambdas)
  expL <- eigenMapMatMult(eigenMapMatMult(Q, as.matrix(diag(exp(-tau * lambdas)))), Q_inv)
  # expL <- Q %*% diag(exp(-tau * lambdas)) %*% Q_inv
  if (abs(sum(expL) - Nodes) > 1e-6) {
    stop("expL is not a stochastic matrix! Check the row sums.")
  }
  if (verbose) {
    cat(paste("Building distance matrix...\n"))
  }
  if (requireNamespace("parallelDist", quietly = TRUE)) {
    # parallel dist
    # computes the Euclidean norm between the rows of the matrix
    # for two row vectors c, y
    # \sqrt(\sum_i (x_i - y_i) ^ 2))
    DM <- parallelDist::parDist(expL)
  } else {
    # dist
    # computes the Euclidean norm between the rows of the matrix
    # for two row vectors c, y
    # \sqrt(\sum_i (x_i - y_i) ^ 2))
    DM <- stats::dist(expL)
  }
  DM <- as.matrix(DM)
  # names
  colnames(DM) <- rownames(Q)
  rownames(DM) <- colnames(DM)
  return(DM)
}

