#' Matrix product of two tensors
#'
#' Matrix product of two tensors unfolded on the specified modes.
#'
#' @param x A tensor instance.
#' @param y A tensor instance.
#' @param ms The indices of the modes to compute on. A single value or a vector.
#'
#' @return Return the matrix product of tensors \code{x} and \code{y}.
#'
#' @details Suppose \code{x} is a \eqn{s}-way tensor with dimension \eqn{p_1 \times \ldots \times p_s} and \code{y} is a t-way tensor with dimension \eqn{r_1 \times \ldots \times r_t}. \code{ms} specifies the indices on which the tensors \code{x} and \code{y} are unfolded as columns. Thus, \code{ms} must be a subset of \code{1:min{s,t}}. Meanwhile, the sizes of the dimensions specified by \code{ms} must match, e.g., if \code{ms = 1:k} where \code{k <= min{s,t}}, then \eqn{p_1\times \ldots p_k = s_1\times \ldots s_k}. Let \eqn{X_0} and \eqn{Y_0}  denote the unfolded matrices, the matrix \eqn{X_0 \times Y_0^T} is returned. See \strong{Examples} for a better illustration.
#'
#' @examples
#' x <- rTensor::as.tensor(array(runif(24), c(3, 4, 2)))
#' y <- rTensor::as.tensor(array(runif(24), c(3, 4, 2)))
#' z <- ttt(x, y, 1:2)
#'
#' @importFrom rTensor unfold as.tensor
#' @export

ttt <- function(x, y, ms) {
   s1 <- dim(x)
   s2 <- dim(y)
   idx_1 <- which(!(1:length(s1) %in% ms))
   idx_2 <- which(!(1:length(s2) %in% ms))
   mat_1 <- unfold(x, row_idx = idx_1, col_idx = ms)@data
   mat_2 <- unfold(y, row_idx = idx_2, col_idx = ms)@data
   mat <- tcrossprod(mat_1, mat_2)
   mat <- as.tensor(mat)
   return(mat)
}
