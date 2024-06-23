#' @title Determining whether to stop choosing sample
#'
#' @description
#' \code{is_stop_ASE} determines whether to stop choosing sample based on the
#' current estimator
#'
#' @details
#' is_stop_ASE determines if the iteration stop condition is met based on the
#' current estimator
#' @param sandwich A numeric matrix that represent the sandwich information
#'   matrix for covariance
#' @param d  A numeric number specifying the length of the fixed size confidence
#'   set that we specify
#' @param nonZeroIdx A numeric number specifying the index of the non zero
#'   coefficient
#' @param verbose A A logical value to determine whether to get the full
#'   information about the iteration
#' situation
#' @export
#' @return a list of these elements:
#' \item{stop}{a logical value. If TRUE, it means we have choosen enough
#' samples.}
#' \item{eigen}{the maximum eighevalue covariance matrix}

is_stop_ASE <- function(sandwich, d, nonZeroIdx, verbose = FALSE) {
  inv_cov <- solve(sandwich)
  inv_cov <- (inv_cov + t(inv_cov))/2
  # if(sum(isNonZero)<1) isNonZero <- !vector(length = params$p)
  tmp <- matrix(0, dim(inv_cov)[1], dim(inv_cov)[2])
  tmp[nonZeroIdx, nonZeroIdx] <- inv_cov[nonZeroIdx, nonZeroIdx]
  eigen.max <- eigen(tmp, symmetric = TRUE,
                     only.values = TRUE)$values[1]
  # eigen.max <- eigen(inv_cov, symmetric = TRUE,
  # only.values = TRUE)$values[1]

  p0 = length(nonZeroIdx)
  if (verbose) {
    cat("eigen max:", eigen.max, " the criterion value is: ", d^2 / stats::qchisq(df = p0, 0.95), "\n")
    cat("kapa:", (d^2) / (eigen.max * stats::qchisq(df = p0, 0.95)), "\n")
  }
  stop <- (eigen.max < d^2 / stats::qchisq(df = p0, 0.95))
  list(stop=stop, eigen=eigen.max)
}
