#' Computes the Log Cholesky Decomposition and the Inverse
#'
#' Computes the log Cholesky decomposition and the inverse of it. The functions
#' are provided as the log Cholesky decomposition is used in the
#' parameterization of the covariance matrix.
#'
#' @param x A positive definite matrix or a vector with a log Cholesky
#' decomposition.
#'
#' @return
#' A numeric vector with the log Cholesky decomposition or a matrix with the
#' inverse.
#'
#' @examples
#' set.seed(1)
#' S <- drop(rWishart(1, 10, diag(10)))
#' log_chol(S)
#' stopifnot(isTRUE(all.equal(S, log_chol_inv(log_chol(S)))),
#'           (NCOL(S) * (NCOL(S) + 1L)) %/% 2L == length(log_chol(S)))
#'
#' @export
log_chol <- function(x){
  x <- chol(x)
  diag(x) <- log(diag(x))
  x[upper.tri(x, TRUE)]
}

#' @rdname log_chol
#' @export
log_chol_inv <- function(x){
  dim <- (sqrt(8 * length(x) + 1) - 1) / 2
  out <- matrix(0, dim, dim)
  out[upper.tri(out, TRUE)] <- x
  diag(out) <- exp(diag(out))
  crossprod(out)
}

jac_log_chol_inv <- function(x){
  z <- chol(log_chol_inv(x))
  n <- NCOL(z)
  res <- kronecker(t(z), diag(n)) %*% get_commutation(n, n) +
    kronecker(diag(n), t(z))

  mult <- rep(1., length(x))
  j <- 0L
  for(i in Reduce(`+`, 1:n, 0L, accumulate = TRUE)[-1]){
    j <- j + 1L
    mult[i] <- z[j, j]
  }

  res <- res[, upper.tri(z, TRUE)] * rep(mult, each = NROW(res))
  res[upper.tri(z, TRUE), ]
}
