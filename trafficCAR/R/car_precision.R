#' CAR precision matrix from an adjacency matrix
#'
#' Constructs the precision matrix for an intrinsic CAR (ICAR) or proper CAR model:
#' \deqn{Q = \tau (D - \rho A), \quad D = \mathrm{diag}(A \mathbf{1}).}
#'
#' For ICAR, set `type = "icar"` (internally uses \eqn{\rho=1}).
#' For proper CAR, set `type = "proper"` and choose `rho` so that \eqn{D - \rho A}
#' is positive definite (no automatic spectral checks are performed).
#'
#' @param A Square adjacency/weight matrix (base matrix or a `Matrix` sparse type).
#'   Diagonal entries are ignored (set to 0).
#' @param type Either `"icar"` or `"proper"`.
#' @param rho Spatial dependence parameter for proper CAR. Ignored for ICAR.
#' @param tau Positive scalar precision multiplier.
#' @param symmetrize If `TRUE`, replaces `A` by `(A + t(A))/2` before construction.
#' @param check If `TRUE`, performs basic validation and warnings.
#'
#' @return A symmetric sparse precision matrix `Q` (class `"dsCMatrix"`).
#'
#' @examples
#' A <- matrix(0, 4, 4)
#' A[1,2] <- A[2,1] <- 1
#' A[2,3] <- A[3,2] <- 1
#' A[3,4] <- A[4,3] <- 1
#' Q_icar <- car_precision(A, type = "icar", tau = 1)
#' Q_prop <- car_precision(A, type = "proper", rho = 0.9, tau = 2)
#'
#' @export
car_precision <- function(A,
                          type = c("icar", "proper"),
                          rho = 0.99,
                          tau = 1,
                          symmetrize = FALSE,
                          check = TRUE) {
  type <- match.arg(type)

  A <- as_sparse_adjacency(A, symmetrize = symmetrize, check = check)

  if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau) || tau <= 0) {
    stop("`tau` must be a single positive finite number")
  }

  if (type == "proper") {
    if (!is.numeric(rho) || length(rho) != 1L || !is.finite(rho)) {
      stop("`rho` must be a single finite number when `type = \"proper\"`.")
    }

    ## universal admissibility condition
    if (check && abs(rho) >= 1) {
      stop("`rho` outside admissible range for proper CAR")
    }
  }

  d <- Matrix::rowSums(A)

  if (type == "proper" && check && any(d == 0)) {
    stop("proper CAR not defined for graphs with isolated nodes")
  }

  if (type == "icar" && check && any(d == 0)) {
    warning(
      "adjacency has isolated node(s) with degree 0; Q will be singular for those components"
    )
  }

  rho_use <- if (type == "icar") 1 else rho

  Q <- Matrix::Diagonal(x = d) - (rho_use * A)

  if (!identical(tau, 1)) {
    Q <- tau * Q
  }

  Q <- Matrix::forceSymmetric(Q, uplo = "U")
  methods::as(Q, "dsCMatrix")
}






#' Intrinsic CAR (ICAR) precision matrix
#'
#' Constructs the intrinsic CAR precision matrix
#' \deqn{Q = \tau \, s (D - A),}
#' where \eqn{s} is a scaling constant chosen so that the
#' geometric mean of the marginal variances equals 1.
#'
#' The resulting precision matrix is singular with rank deficiency
#' equal to the number of connected components.
#'
#' @param A Square adjacency/weight matrix.
#' @param tau Positive scalar precision multiplier.
#' @param scale Logical; if `TRUE`, applies Besag scaling.
#' @param symmetrize If `TRUE`, replaces `A` by `(A + t(A))/2`.
#' @param check If `TRUE`, performs basic validation and warnings.
#'
#' @return A symmetric sparse precision matrix (`"dsCMatrix"`).
#'
#' @references
#' Sørbye, S. H. and Rue, H. (2014).
#' Scaling intrinsic Gaussian Markov random field priors.
#'
#' @export
intrinsic_car_precision <- function(A, tau = 1, scale = TRUE, symmetrize = FALSE,
                                    check = TRUE) {

  Q <- car_precision(A = A, type = "icar", tau = tau, symmetrize = symmetrize,
    check = check)

  if (!scale) {
    return(Q)
  }

  ## Besag (geometric mean) scaling
  cholQ <- tryCatch(
    Matrix::Cholesky(Q, super = TRUE, Imult = 0),
    error = function(e) NULL
  )

  if (is.null(cholQ)) {
    warning("scaling failed: Cholesky factorization unsuccessful")
    return(Q)
  }

  ## diagonal of the generalized inverse
  Vinv_diag <- Matrix::diag(Matrix::solve(cholQ, system = "A"))
  Vinv_diag <- Vinv_diag[is.finite(Vinv_diag)]

  if (length(Vinv_diag) == 0L) {
    warning("scaling failed: no finite marginal variances")
    return(Q)
  }

  s <- exp(mean(log(Vinv_diag)))

  Q * s
}


#' @keywords internal
icar_rank_deficiency <- function(A) {
  g <- igraph::graph_from_adjacency_matrix(as.matrix(A), mode = "undirected")
  sum(igraph::components(g)$csize > 1)
}

#' Apply sum-to-zero constraint to ICAR precision
#'
#' Projects onto the subspace orthogonal to the constant vector.
#'
#' @param Q ICAR precision matrix
#'
#' @return Constrained precision matrix
#' @export
icar_sum_to_zero <- function(Q) {
  n <- nrow(Q)
  C <- diag(n) - matrix(1 / n, n, n)
  Qc <- C %*% Q %*% C
  Matrix::forceSymmetric(Matrix::Matrix(Qc, sparse = TRUE))
}


