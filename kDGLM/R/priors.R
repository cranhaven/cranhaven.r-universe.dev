#' Zero sum prior
#'
#' Defines the prior of a structural block to be such that the latent states sum zero with probability one.
#'
#' @param block dlm_block object: The structural block.
#' @param var.index integer: The index of the variables from which to set the prior.
#' @param weights numeric: A vector indicating which linear transformation of the data is 0 with probability 1. Default is equivalent to a zero-sum restriction.
#'
#' @return A dlm_block object with the desired prior.
#'
#' @export
#' @examples
#'
#' polynomial_block(mu = 1, D = 0.95) |>
#'   block_mult(5) |>
#'   zero_sum_prior()
#'
#' @details
#' The covariance matrix of the evolution and the drift parameter are also altered to guarantee that the zero sum condition will always hold.
#' The discount factor must be the same for all variables whose prior is being modified.
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' @family auxiliary functions for defining priors.
#'
#' @references
#'    \insertAllCited{}
zero_sum_prior <- function(block, var.index = 1:block$n, weights = rep(1, length(var.index))) {
  n <- length(var.index)
  weights <- weights / sum(weights)
  transf <- matrix(-weights, n, n)
  diag(transf) <- 1 + diag(transf)


  block$a1[var.index] <- block$a1[var.index] - mean(block$a1[var.index])
  block$R1[var.index, var.index] <- t(transf) %*% block$R1[var.index, var.index] %*% transf
  for (i in 1:block$t) {
    D <- block$D[var.index, var.index, i]
    d <- D[D != 0]
    if (min(d) != max(d)) {
      warning("Not all latent states have the same discount factor. All values will be set to the minimum.")
    }
    block$D[var.index, var.index, i] <- min(d)
    block$D[-var.index, var.index, i] <- block$D[var.index, -var.index, i] <- 0
    block$h[var.index, i] <- block$h[var.index, i] - mean(block$h[var.index, i])
    block$H[var.index, var.index, i] <- transf %*% block$H[var.index, var.index, i] %*% transf
    block$H[-var.index, var.index, i] <- block$H[var.index, -var.index, i] <- 0
  }
  block$status <- check.block.status(block)
  return(block)
}

#' CAR prior
#'
#' Defines the prior of a structural block as a Conditional Autoregressive (CAR) prior.
#'
#' @param block dlm_block object: The structural block.
#' @param adj.matrix matrix: The adjacency matrix.
#' @param scale numeric: The tau parameter for the CAR model (see references).
#' @param rho numeric: The rho parameter for the CAR model (see references).
#' @param var.index integer: The index of the variables from which to set the prior.
#' @param sum.zero Bool: If true, all latent states will add to 0.
#' @return A dlm_block object with the desired prior.
#'
#' @importFrom Rfast is.symmetric
#' @export
#' @examples
#'
#' # Creating an arbitrary adjacency matrix
#' adj.matrix <- matrix(
#'   c(
#'     0, 1, 1, 0, 0,
#'     1, 0, 1, 0, 0,
#'     1, 1, 0, 0, 0,
#'     0, 0, 0, 0, 1,
#'     0, 0, 0, 1, 0
#'   ),
#'   5, 5,
#'   byrow = TRUE
#' )
#'
#' polynomial_block(mu = 1, D = 0.95) |>
#'   block_mult(5) |>
#'   CAR_prior(scale = 9, rho = 1, adj.matrix = adj.matrix)
#'
#' @details
#'
#' The filtering algorithm used in this package requires a proper prior for the latent space. As such, this implementation of the CAR prior imposes a zero-sum constraint in the regional effects.
#' The discount factor must be the same for all variables whose prior is being modified.
#'
#' For a revision of the CAR prior, see \insertCite{AlexCar;textual}{kDGLM}.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' @seealso Auxiliary functions for creating structural blocks \code{\link{polynomial_block}}, \code{\link{regression_block}}, \code{\link{harmonic_block}}, \code{\link{TF_block}}.
#'
#' @family auxiliary functions for defining priors.
#'
#' @references
#'    \insertAllCited{}
CAR_prior <- function(block, adj.matrix, scale, rho, sum.zero = FALSE, var.index = 1:block$n) {
  if (dim(adj.matrix)[1] != length(var.index)) {
    stop("Error: Number of regions must be equal to the number of variables.")
  }
  k <- dim(adj.matrix)[1]
  adj.matrix <- matrix(adj.matrix, k, k)
  if (!is.symmetric(adj.matrix)) {
    stop("Error: adj.matrix is not symmetric.")
  }
  # rho <- 0.99

  D.mat <- diag(rowSums(adj.matrix))
  R <- (D.mat - adj.matrix)
  R1 <- ((1 - rho) * diag(k) + rho * R)
  R1_decomp <- eigen(R1)
  index <- which(R1_decomp$values <= 1 - rho + 1e-12)

  car.vectors <- R1_decomp$vector
  fixed.vectors <- R1_decomp$vector[, index]

  car.values <- diag(1 / R1_decomp$values)
  car.values[index, index] <- t(fixed.vectors) %*% block$R1[var.index, var.index] %*% fixed.vectors

  R1 <- car.vectors %*% car.values %*% t(car.vectors)

  scale.values <- rep(1, k)
  scale.values[-index] <- scale
  scale.directions <- car.vectors

  if (is.numeric(scale)) {
    scale.mat <- scale.directions %*% diag(scale.values, ncol = k, nrow = k) %*% t(scale.directions)
    R1 <- scale.mat %*% R1 %*% t(scale.mat)
  } else {
    block$scale[var.index] <- scale.values
    block$direction[var.index, var.index] <- scale.directions
  }

  block$a1[var.index] <- block$a1[var.index] - mean(block$a1[var.index])
  block$R1[var.index, var.index] <- R1
  transf <- matrix(-1 / k, k, k)
  diag(transf) <- 1 + diag(transf)
  for (i in 1:block$t) {
    D <- block$D[var.index, var.index, i]
    d <- D[D != 0]
    if (min(d) != max(d)) {
      warning("Not all latent states have the same discount factor. All values will be set to the average value.")
    }
    block$D[var.index, var.index, i] <- mean(D[D != 0])
    block$D[-var.index, var.index, ] <- block$D[var.index, -var.index, ] <- 0
    block$h[var.index, i] <- block$h[var.index, i] - mean(block$h[var.index, i])
    block$H[var.index, var.index, i] <- transf %*% block$H[var.index, var.index, i] %*% transf
    block$H[-var.index, var.index, i] <- block$H[var.index, -var.index, i] <- 0
  }
  block$status <- check.block.status(block)

  return(block)
}

#' Joint prior
#'
#' Defines the joint prior of a structural block.
#'
#' @param var.index Integer: The index of the variables from which to set the prior.
#' @param a1 Numeric: The prior mean.
#' @param R1 Matrix: The prior covariance matrix.
#' @param block dlm_block object: The structural block.
#'
#' @return A dlm_block object with the desired prior.
#'
#' @export
#' @examples
#'
#' polynomial_block(mu = 1, D = 0.95) |>
#'   block_mult(5) |>
#'   joint_prior(var.index = 1:2, R1 = matrix(c(1, 0.5, 0.5, 1), 2, 2))
#'
#' @details
#' The discount factor must be the same for all variables whose prior is being modified.
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' @family auxiliary functions for defining priors.
#'
#' @importFrom Rfast colAny
#'
#' @references
#'    \insertAllCited{}
joint_prior <- function(block, var.index = 1:block$n, a1 = block$a1[var.index], R1 = block$R1[var.index, var.index]) {
  if (length(a1) != length(var.index) | any(dim(R1) != length(var.index))) {
    stop("Error: The number of indexes does not match the dimension of a1 and/or R1.")
  }
  block$a1[var.index] <- a1
  block$R1[var.index, var.index] <- R1
  D <- block$D[var.index, var.index, , drop = FALSE]
  for (i in 1:block$t) {
    D.i <- D[, , i]
    if (min(D.i[D.i != 0]) != max(D.i[D.i != 0])) {
      warning("D has more than one distict non-zero value. All values will be set to the average value at that time.")
    }
    D[, , i] <- mean(D.i[D.i != 0])
  }
  block$D[var.index, var.index, ] <- D
  block$D[-var.index, var.index, ] <- block$D[var.index, -var.index, ] <- 0
  flags <- block$G.labs == "noise.disc"
  if (any(flags)) {
    block$G.labs[colAny(flags), colAny(flags)] <- "noise.disc"
  }
  block$status <- check.block.status(block)
  return(block)
}
