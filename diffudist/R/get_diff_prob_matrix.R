#' @title Evaluate a Laplacian Matrix
#' @description Returns a specific Laplacian matrix corresponding to the chosen
#' dynamics type and network. The available types are:
#' \describe{
#'   \item{Laplacian}{for the classical combinatorial Laplacian matrix;
#'     it governs the diffusion dynamics on the network}
#'   \item{Normalized Laplacian}{for the Laplacian matrix normalized by degree
#'     matrix, the so-called classical random walk normalized Laplacian;
#'     it governs stochastic walks on the network}
#'   \item{Quantum Laplacian}{for the Laplacian matrix normalized to be
#'     symmetric; it governs quantum walks on the network}
#'   \item{MERW normalized Laplacian}{the maximal-entropy random walk (RW)
#'     normalized Laplacian; it governs stochastic walks on the network,
#'     in which the random walker moves according to a maximal-entropy RW [1].}
#' }
#'
#' The maximum entropy random walk (MERW) chooses the stochastic matrix which
#' maximizes \eqn{H(S)}, so that the walker can explore every walk of the
#' same length with equal probability.
#' Let \eqn{\lambda_N, \phi} be the leading eigenvalue and
#' corresponding right eigenvector of the adjacency matrix \eqn{A}. Then the
#' transition matrix corresponding to the discrete-time random walk is
#' \eqn{\Pi_{ij} = \frac{A_{ij}}{\lambda_N}\frac{\phi_j}{\phi_i}.}
#' The MERW (normalized) Laplacian is then given by \eqn{I - \Pi}.
#' Note that we use the notation \eqn{\Pi} and \code{Pi} to avoid confusion
#' with the abbreviation \code{T} for the logical \code{TRUE}.
#'
#' @param g a network
#' @param type the type of Laplacian matrix. default \code{"Laplacian"}, the
#' combinatorial Laplacian. Other types:
#' \code{c("Normalized Laplacian", "Quantum Laplacian", "MERW Normalized
#'   Laplacian")}.
#' Note that you can type abbreviations, e.g. "L", "N", "Q", "M" for the
#' respective types (case is ignored). The argument match is done through
#' \code{\link[strex]{match_arg}}.
#' @param weights edge weights, representing the strength/intensity (not the cost!) of each link.
#'    if weights is NULL (the default) and g has an edge attribute called weight, then
#'    it will be used automatically.
#'    If this is NA then no weights are used (even if the graph has a weight attribute).
#' @param verbose default \code{TRUE}. If information on the type of Laplacian
#' or on edge weights should be printed.
#' @return the (`type`) Laplacian matrix of network `g`
#' @references
#'    [1] Burda, Z., et al. (2009). Phys Rev. Lett. 102 160602(April), 1–4.
#'    \doi{10.1103/PhysRevLett.102.160602}
#' @export
get_laplacian <- function(g,  type = "Laplacian", weights = NULL, verbose = TRUE) {
  if (is.null(weights)) {
    # default case
    if ("weight" %in% igraph::edge_attr_names(g)) {
      # weight edge attribute
      if (verbose) {
        cat("Weighted network. Using edge weights as connection strenghts.\n")
      }
    } else {
      # no (numeric) weight edge attribute
      if (verbose) {
        cat("Unweighted network.\n")
      }
    igraph::E(g)$weight <- 1
    }
  } else if (length(weights) == 1 && is.na(weights)) {
    if (verbose) {
      cat("Ignoring edge weights.\n")
    }
    g <- igraph::set_edge_attr(g, name = "weight", value = 1)
  } else if (is.numeric(weights)) {
    # providing new weights
    if (verbose) {
      cat("Adding given weights as edge attributes.\n")
    }
    igraph::E(g)$weight <- weights
  }
  types <-
    c("Laplacian", "Normalized Laplacian", "Quantum Laplacian",
      "MERW Normalized Laplacian")
  if (requireNamespace("strex", quietly = TRUE)) {
    type <- strex::match_arg(type, types, ignore_case = TRUE)
  } else {
    type <- match.arg(toupper(type), types)
  }
  if (verbose)
    cat("Evaluating the", type, "matrix\n")

  N <- igraph::vcount(g)
  # Combinatorial Laplacian
  L <- igraph::laplacian_matrix(g, normalized = FALSE, weights = NULL, sparse = FALSE)
  if (type == "Normalized Laplacian") {
    D <- 1 / igraph::strength(g, mode = "out")
    D[which(is.infinite(D))] <- 0
    # if node i has no out-going edges, then the i-th row of the Laplacian
    # is a zero vector
    D <- Matrix::Matrix(diag(D))
    L <- D %*% L    # L_{norm} = D^{-1} * (D - A)
  }

  if (type == "Quantum Laplacian") {
    D <- 1 / sqrt(igraph::strength(g, mode = "out"))
    D[which(is.infinite(D))] <- 0
    # if node i has no out-going edges, then the i-th row of the Laplacian
    # is a zero vector
    D <- Matrix::Matrix(diag(D))
    L <- (D %*% L) %*% D
  }

  if (type == "MERW Normalized Laplacian") {
    A <- igraph::as_adjacency_matrix(g, attr = 'weight', sparse = FALSE)
    if (!igraph::is_connected(g, mode = "strong")) {
      cat("The network is not strongly connected!\n")
      cat("It is not guaranteed that the Perron-Frobenius theorem holds!\n")
    }
    tmp <- eigen(A, only.values = FALSE)
    leading_eigenvalue <- tmp$values[which.max(tmp$values)]
    leading_eigenvector <- c(tmp$vectors[, which.max(tmp$values)])
    if (all(leading_eigenvector[which(leading_eigenvector != 0)] < 0)) {
      leading_eigenvector <- -leading_eigenvector
    }
    rm(tmp)
    # compute the MERW transition matrix
    # the following matrix-form is equivalent to
    # for (i in 1:N) {
    #   for (j in 1:N) {
    #     Pi[i, j] <- A[i, j] / leading_eigenvalue *
    #                leading_eigenvector[j] / leading_eigenvector[i]
    #   }
    # }
    Pi <- 1. / leading_eigenvalue *
      Matrix::Matrix(diag(1 / leading_eigenvector)) %*%
      A %*%
      Matrix::Matrix(diag(leading_eigenvector))
    rm(A)
    # check that Pi is a stochastic matrix
    if (abs(sum(Pi) - N) > 1e-6) {
      stop(
        paste0(
          " Problems in building the MERW-transition matrix -> ",
          abs(sum(Pi) - N),
          ". Aborting process."
        )
      )
    }
    # L = I - Pi
    L <- Matrix::Matrix(diag(1, nrow = N)) - Pi
  }
  return(Matrix::drop0(L))
}

#' @rdname get_laplacian
getLaplacianMatrix <- get_laplacian

#' @title Diffusion Probability Matrix
#' @description
#' Returns a matrix where each entry encodes the diffusion probability between two nodes
#' @param g a single-layer network
#' @param tau diffusion time
#' @param type default "Normalized Laplacian". The type of Laplacian (i.e. of
#' dynamics) to consider. Other types available are:
#' \describe{
#'   \item{Laplacian}{for the classical combinatorial Laplacian matrix;
#'     it governs the diffusion dynamics on the network}
#'   \item{Normalized Laplacian}{for the Laplacian matrix normalized by degree
#'     matrix, the so-called classical random walk normalized Laplacian;
#'     it governs stochastic walks on the network}
#'   \item{Quantum Laplacian}{for the Laplacian matrix normalized to be
#'     symmetric; it governs quantum walks on the network}
#'   \item{MERW normalized Laplacian}{the maximal-entropy random walk (RW)
#'     normalized Laplacian; it governs stochastic walks on the network,
#'     in which the random walker moves according to a maximal-entropy RW [1].}
#' }
#' Note that you can type abbreviations, e.g. "L", "N", "Q", "M" for the
#' respective types (case is ignored). The argument match is done through
#' \code{\link[strex]{match_arg}}.
#' @param weights edge weights, representing the strength/intensity (not the cost!) of each link.
#'    if weights is NULL (the default) and g has an edge attribute called weight, then
#'    it will be used automatically.
#'    If this is NA then no weights are used (even if the graph has a weight attribute).
#' @param verbose default \code{TRUE}. If information on the type of Laplacian
#' or on edge weights should be printed.
#' @return The matrix \eqn{exp^{-\tau L}}, exponential of a Laplacian matrix.
#' @keywords Markov-chain transition probabilities
#' @seealso \code{\link{get_laplacian}, \link{get_distance_matrix}}
#' @references
#'   De Domenico, M. (2017). Diffusion Geometry Unravels the Emergence of
#'   Functional Clusters in Collective Phenomena. Physical Review Letters.
#'   \doi{10.1103/PhysRevLett.118.168301}
#'
#'   Bertagnolli, G., & De Domenico, M. (2021). Diffusion geometry of multiplex and
#'   interdependent systems. Physical Review E, 103(4), 042301.
#'   \doi{10.1103/PhysRevE.103.042301}
#'   \href{https://arxiv.org/abs/2006.13032}{arXiv: 2006.13032}
#'
#' @export
get_diffusion_probability_matrix <- function(g, tau, type = "Normalized Laplacian",
                                             weights = NULL, verbose = TRUE) {
  types <- c("Laplacian", "Normalized Laplacian", "Quantum Laplacian",
             "MERW Normalized Laplacian")
  if (requireNamespace("strex", quietly = TRUE)) {
    tryCatch(
      type <- strex::match_arg(type, types, ignore_case = TRUE),
      error = function(e) {
        cat(
          "ERROR! Wrong type of Laplacian, available types are:\n",
          types, "\n",
          "Aborting process.\n"
        )
      }
    )
  } else {
    tryCatch(
      type <- match.arg(toupper(type), types),
      error = function(e) {
        cat(
          "ERROR! Wrong type of Laplacian, available types are:\n",
          types, "\n",
          "Aborting process.\n"
        )
      }
    )
  }
  # by default weights are considered, if present
  # if (is.null(igraph::E(g)$weight)) {
  #   cat("Warning: missing edge weights. Assigning 1 by default\n")
  #   igraph::E(g)$weight <- 1
  # }
  # N <- length(V(g))
  L <- get_laplacian(g, type, weights, verbose)
  Pt <- as.matrix(expm::expm(-tau * as.matrix(L)))
  return(Pt)
}

#' @describeIn get_diffusion_probability_matrix Old deprecated function
#' @usage getDiffusionProbabilityMatrix(g, tau, type = "Normalized Laplacian", weights = NULL,
#'                                      verbose = TRUE)
#' @export
getDiffusionProbabilityMatrix <- function(g, tau, type = "Normalized Laplacian",
                                          weights = NULL, verbose = TRUE) {
  .Deprecated("get_diffusion_probability_matrix")
  return(get_diffusion_probability_matrix(g, tau, type = type,
                                          weights = NULL, verbose = TRUE))
}

#' @rdname get_diffusion_probability_matrix
#' @export
get_diffu_Pt <- get_diffusion_probability_matrix

#' @title Diffusion probability matrix from transition matrix
#' @description Description here
#' @param Pi Transition matrix. We do not use \code{T} to avoid conflicts
#' with the abbreviation for \code{TRUE}), instead we indicated the transition
#' matrix with the capital greek letter \eqn{\Pi} in the equations and
#' \code{Pi} in the code.
#' @param tau diffusion time
#' @return \eqn{exp^{-\tau (I - \Pi)}}, exponential of the normalized Laplacian
#' matrix corresponding to the given transition rate matrix (or transition
#' probability matrix of a discrete-time Markov chain).
#' @keywords Markov-chain probabilities transition
#' @seealso \code{\link{get_diffusion_probability_matrix}}
#' @references
#' De Domenico, M. (2017). Diffusion Geometry Unravels the Emergence of
#' Functional Clusters in Collective Phenomena. Physical Review Letters.
#' \doi{10.1103/PhysRevLett.118.168301}
#'
#' Bertagnolli, G., & De Domenico, M. (2020). Diffusion Geometry of Multiplex
#' and Interdependent Systems.
#' \href{https://arxiv.org/abs/2006.13032}{arxiv preprint arxiv:2006.13032}
#' @export
get_diffusion_probability_matrix_from_T <- function(Pi, tau) {
  N <- nrow(Pi)
  if (ncol(Pi) != N) {
    stop("Pi is not a square amtrix!")
  }
  if (abs(sum(Pi) - N) > 1e-6) {
    stop("Pi is not a stochastic matrix! Check the row sums.")
  }
  L <- Matrix::Matrix(diag(1, nrow = N)) - Pi
  Pt <- as.matrix(expm::expm(-tau * L))
  return(Pt)
}

#' @rdname get_diffusion_probability_matrix_from_T
#' @export
get_diffu_Pt_from_T <- get_diffusion_probability_matrix_from_T

#' @rdname get_diffusion_probability_matrix_from_T
#' @export
get_diffu_Pt_from_Pi <- get_diffusion_probability_matrix_from_T

#' @rdname get_diffusion_probability_matrix_from_T
#' @export
get_diffusion_probability_matrix_from_Pi <- get_diffusion_probability_matrix_from_T
