#' @title Diffusion Distance Matrix
#'
#' @description Returns a matrix where each entry encodes the diffusion distance
#' between two nodes of a network.
#'
#' The diffusion distance at time \eqn{\tau} between nodes \eqn{i, j \in G}
#' is defined as
#' \deqn{D_{\tau}(i, j) = \vert \mathbf{p}(t|i) - \mathbf{p}(t|j) \vert_2}
#' with \eqn{\mathbf{p}(t|i) = (e^{- \tau L})_{i\cdot} = \mathbf{e}_i e^{- \tau L}}
#' indicating the i-th row of the stochastic matrix \eqn{e^{- \tau L}} and
#' representing the probability (row) vector of a random walk dynamics
#' corresponding to the initial condition \eqn{\mathbf{e}_i}, i.e. the random
#' walker is in node \eqn{i} at time \eqn{\tau = 0} with probability 1.
#'
#' @param g a (single-layer) network
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
#' @param weights edge weights, representing the strength/intensity (not the cost!)
#'   of each link. If weights is NULL (the default) and g has an edge attribute
#'   called weight, then it will be used automatically.
#'   If this is NA then no weights are used (even if the graph has a weight attribute).
#' @param as_dist If the function should return a matrix or an object of class "dist" as
#'   returned from [stats::as.dist]. Default is FALSE if the number of nodes is smaller
#'   than 1000.
#' @param verbose default TRUE
#' @return The diffusion distance matrix \eqn{D_t}, a square numeric matrix
#'   of the \eqn{L^2}-norm distances between posterior probability vectors, i.e.
#'   Euclidean distances between the rows of the stochastic matrix
#'   \eqn{P(t) = e^{-\tau L}}, where \eqn{-L = -(I - T)} is the generator of the
#'   continuous-time random walk (Markov chain) of given \code{type} over network
#'   \code{g}.
#' @keywords diffusion distance
#' @seealso \code{\link{get_diffusion_probability_matrix}}
#' @references
#'   De Domenico, M. (2017). Diffusion Geometry Unravels the Emergence of
#'   Functional Clusters in Collective Phenomena. Physical Review Letters.
#'   \doi{10.1103/PhysRevLett.118.168301}
#'
#'   Bertagnolli, G., & De Domenico, M. (2021). Diffusion geometry of multiplex and
#'   interdependent systems. Physical Review E, 103(4), 042301.
#'   \doi{10.1103/PhysRevE.103.042301}
#'   \href{https://arxiv.org/abs/2006.13032}{arXiv: 2006.13032}
#' @examples
#' g <- igraph::sample_pa(10, directed = FALSE)
#' dm_crw <- get_distance_matrix(g, tau = 1)
#' dm_merw <- get_distance_matrix(g, tau = 1, type = "MERW")
#' @export
get_distance_matrix <- function(g, tau, type = "Normalized Laplacian", weights = NULL,
                                as_dist = FALSE, verbose = TRUE) {
  # #by default weights are considered, if present
  # if ( is.null(igraph::E(g)$weight) ) {
  #   cat("Warning: missing edge weights. Assigning 1 by default\n")
  #   igraph::E(g)$weight <- 1
  # }
  # N <- length(igraph::V(g))
  expL <- get_diffusion_probability_matrix(g, tau, type, weights, verbose)
  # names
  node_labels <- as.character(igraph::V(g)$name)
  if (is.null(node_labels)) {
    node_labels <- as.character(igraph::V(g)$label)
  }
  if (is.null(node_labels)) {
    node_labels <- as.character(igraph::V(g))    # same as node_labels <- 1:N
  }
  colnames(expL) <- rownames(expL) <- node_labels
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
  if ((!as_dist) && (length(igraph::V(g)) < 1000)) {
    return(as.matrix(DM))
  } else {
    return(DM)
  }
}

# get_distance_matrix <- compiler::cmpfun(getDistanceMatrixRaw)

#' @describeIn get_distance_matrix Old deprecated function
#' @usage getDistanceMatrix(g, tau, type = "Normalized Laplacian", weights = NULL,
#'                          verbose = TRUE)
#' @export
getDistanceMatrix <- function(g, tau, type = "Normalized Laplacian", weights = NULL,
                              verbose = TRUE) {
  .Deprecated("get_distance_matrix")
  return(get_distance_matrix(g, tau, type = type, weights = weights, verbose = verbose))
}

#' @rdname get_distance_matrix
#' @export
get_DDM <- get_distance_matrix

#' @title Diffusion distance matrix from a custom transition matrix
#'
#' @description Returns a matrix where each entry encodes the diffusion distance
#' between two nodes of a network, given a transition matrix on the network
#' and a diffusion time.
#'
#' The diffusion distance at time \eqn{\tau} between nodes \eqn{i, j \in G}
#' is defined as
#' \deqn{D_{\tau}(i, j) = \vert \mathbf{p}(t|i) - \mathbf{p}(t|j) \vert_2}
#' with \eqn{\mathbf{p}(t|i) = (e^{- \tau L})_{i\cdot} = \mathbf{e}_i e^{- \tau L}}
#' indicating the i-th row of the stochastic matrix \eqn{e^{- \tau L}} and
#' representing the probability (row) vector of a random walk dynamics
#' corresponding to the initial condition \eqn{\mathbf{e}_i}, i.e. the random
#' walker is in node \eqn{i} at time \eqn{\tau = 0} with probability 1.
#'
#' The Laplacian \eqn{L} is the normalised laplacian corresponding to the
#' given transition matrix, i.e. \eqn{L = I - Pi}.
#'
#' @param Pi a transition matrix (it should be a stochastic matrix)
#' @param tau diffusion time
#' @param verbose default TRUE
#' @return The diffusion distance matrix \eqn{D_t}, a square numeric matrix
#'   of the \eqn{L^2}-norm distances between posterior probability vectors, i.e.
#'   Euclidean distances between the rows of the stochastic matrix
#'   \eqn{P(t) = e^{-\tau L}}, where \eqn{-L = -(I - T)} is the generator of the
#'   continuous-time random walk (Markov chain) corresponding to the
#'   discrete-time transition matrix \eqn{T=}\code{Pi}.
#' @keywords diffusion distance
#' @seealso \code{\link{get_distance_matrix} \link{get_diffusion_probability_matrix},
#' \link{get_diffusion_probability_matrix_from_T}}
#' @references
#'   De Domenico, M. (2017). Diffusion Geometry Unravels the Emergence of
#'   Functional Clusters in Collective Phenomena. Physical Review Letters.
#'   \doi{10.1103/PhysRevLett.118.168301}
#'
#'   Bertagnolli, G., & De Domenico, M. (2021). Diffusion geometry of multiplex and
#'   interdependent systems. Physical Review E, 103(4), 042301.
#'   \doi{10.1103/PhysRevE.103.042301}
#'   \href{https://arxiv.org/abs/2006.13032}{arXiv: 2006.13032}
#' @examples
#' g <- igraph::sample_pa(10, directed = FALSE)
#' dm <- get_distance_matrix(g, tau = 1)
#' @export
get_distance_matrix_from_T <- function(Pi, tau, verbose = TRUE) {
  Pi <- as.matrix(Pi)
  # check square matrix
  N <- nrow(Pi)
  if (ncol(Pi) != N)
    stop("Pi is not a square matrix!")
  if (abs(sum(Pi) - N) > 1e-6) {
    stop("Pi is not a stochastic matrix! Check the row sums.")
  }
  expL <- get_diffusion_probability_matrix_from_T(Pi, tau)
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
  colnames(DM) <- colnames(Pi)
  rownames(DM) <- colnames(Pi)
  # class(DM) <- "DtDistMatrix"
  return(DM)
}

#' @rdname get_distance_matrix_from_T
#' @export
get_DDM_from_T <- get_distance_matrix_from_T

#' @rdname get_distance_matrix_from_T
#' @export
get_distance_matrix_from_Pi <- get_distance_matrix_from_T

#' @rdname get_distance_matrix_from_T
#' @export
get_DDM_from_Pi <- get_distance_matrix_from_T


