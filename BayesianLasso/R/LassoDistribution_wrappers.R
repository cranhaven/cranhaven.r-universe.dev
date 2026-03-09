#' @export
dlasso <- function(x, a, b, c, logarithm = FALSE) {
  val <- .Call(`_BayesianLasso_dlasso`, as.numeric(x), a, b, c, logarithm)
  val <- drop(val)  # remove dim attribute if it's a 1x1 matrix
  if (length(val) == 1L) val <- val[[1]]  # return scalar if input is scalar
  val
}



#' @export
plasso <- function(q, a, b, c) {
  val <- .Call(`_BayesianLasso_plasso`, as.numeric(q), a, b, c)
  val <- drop(val)  # removes dim attribute if it's 1x1 matrix
  if (length(val) == 1L) val <- val[[1]]  # safely extract scalar
  val
}

#' @export
qlasso <- function(p, a, b, c) {
  val <- .Call(`_BayesianLasso_qlasso`, as.numeric(p), a, b, c)
  val <- drop(val)  # removes dim attribute if it's 1x1 matrix
  if (length(val) == 1L) val <- val[[1]]  # safely extract scalar
  val
}

#' @export
elasso <- function(a, b, c) {
  .Call(`_BayesianLasso_elasso`, a, b, c)
}

#' @export
vlasso <- function(a, b, c) {
  .Call(`_BayesianLasso_vlasso`, a, b, c)
}

#' @export
mlasso <- function(a, b, c) {
  val <- .Call(`_BayesianLasso_mlasso`, a, b, c)
  val <- drop(val)  # removes dim attribute if it's 1x1 matrix
  if (length(val) == 1L) val <- val[[1]]  # safely extract scalar
  val
}

#' @export
rlasso <- function(n, a, b, c) {
  val <- .Call(`_BayesianLasso_rlasso`, as.integer(n), a, b, c)
  val <- drop(val)  # removes dim attribute if it's 1x1 matrix
  if (length(val) == 1L) val <- val[[1]]  # safely extract scalar
  val
}
