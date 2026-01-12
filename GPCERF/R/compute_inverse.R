#' @title
#' Compute matrix inverse for a covariate matrix
#'
#' @description
#' Computes inverse of a covariate matrix using Choleski decomposition.
#'
#' @param mtrx An \code{n * n} covariate matrix
#'
#' @return
#' A matrix that represent the inverse of the input matrix.
#'
#' @keywords internal
#'
compute_inverse <- function(mtrx) {

  if (!is.matrix(mtrx)) {
    stop(paste0("The input mtrx should be a matrix. ",
                "Current format: ", class(mtrx)[1]))
  }

  if (nrow(mtrx) != ncol(mtrx)) {
    stop(paste0("The input mtrx should be a square matrix. ",
                "Current dimension: nrow: ",
                nrow(mtrx), ", ncol: ", ncol(mtrx)))
  }

  t_1 <- proc.time()
  inv_mtrx <- chol2inv(chol(mtrx))
  t_2 <- proc.time()
  logger::log_debug("Wall clock time to compute inverse matrix ",
                    "({nrow(mtrx)}, {ncol(mtrx)}): ",
                    " {t_2[[3]] - t_1[[3]]} s.")
  return(inv_mtrx)
}
