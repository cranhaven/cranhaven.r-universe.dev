#' Pool lopo results to original prey types
#'
#' If \code{\link{lopo}} is used to perform a leave-one-prey-out analysis with
#' a partitioned prey library (\code{\link{make_prey_part}}), \code{lopo_pool}
#' pools the partitioned results back to the original unpartitioned prey types.
#'
#' @param est The estimation matrix of a leave-one-prey-out analysis performed
#'   by the function \code{\link{lopo}}, returned as the \code{est} object.
#' @param n_conv An integer vector denoting the number of signature estimates in
#'   the partitioned prey types that converged, returned by a call to
#'   \code{\link{lopo}} as the \code{n_conv} object.
#' @param type_ss An integer vector with the number of signatures (sample size)
#'   in each of the partitioned prey types, returned by a call to
#'   \code{\link{make_prey_part}} as the \code{type_ss} object.
#' @param pre The pre-multiplication matrix returned by a call to
#'   \code{\link{make_prey_part}} as the \code{pool_pre} object.
#' @param post The post-multiplication matrix returned by a call to
#'   \code{\link{make_prey_part}} as the \code{pool_post} object.
#'
#' @return A list containing the following elements, all of which are organized
#'   on the basis of the original unpartitioned prey types: \describe{
#'   \item{est}{A square matrix containing the mean distribution of
#'     leave-one-prey-out estimates among all prey types.}
#'   \item{mean_correct}{The mean proportion correctly estimated across prey
#'     types, unweighted by prey-type sample sizes.}
#'   \item{total_correct}{The proportion of all signatures correctly estimated.}
#'   \item{n_conv}{An integer vector containing the number of estimates that
#'     converged.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string containing a brief summary of the results.}
#' }
#'
#' @section Details:
#' The statistics computed by \code{\link{lopo}} and \code{lopo_pool} are one
#' measure of the distinctiveness of prey types within a prey library.  However,
#' it is important to be aware that such statistics are not necessarily
#' informative of the ability of QFASA to accurately estimate predator diets, as
#' Bromaghin et al. (2015, 2016a, 2016b) found that QFASA performance depends
#' strongly on the interaction between characteristics of a prey library, the
#' specific diet of a predator, and the accuracy of the calibration
#' coefficients.  Consequently, the user is warned not to misinterpret or
#' misrepresent these statistics.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016b. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research} 31:597-606.
#'
#' Bromaghin, J.F., S.M. Budge, G.W. Thiemann, and K.D. Rode. 2016a. Assessing
#'   the robustness of quantitative fatty acid signature analysis to assumption
#'   violations. \emph{Methods in Ecology and Evolution} 7:51-59.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#' @examples
#'
#' lopo_pool(est = matrix(c(0.90, 0.05, 0.30, 0.02,
#'                          0.04, 0.84, 0.09, 0.03,
#'                          0.02, 0.06, 0.35, 0.57,
#'                          0.05, 0.10, 0.15, 0.70), nrow = 4, byrow = TRUE),
#'           n_conv = c(2, 8, 8, 11),
#'           type_ss = c(2, 8, 8, 12),
#'           pre = matrix(c(0.2, 0.8, 0.0, 0.0,
#'                          0.0, 0.0, 0.4, 0.6), nrow = 2, byrow = TRUE),
#'           post = matrix(c(1, 1, 0, 0,
#'                           0, 0, 1, 1), ncol = 2)
#'           )
#'
#' @export
#'
################################################################################


lopo_pool <- function(est, n_conv, type_ss, pre, post){

  # Initialize objects to be returned.
  est_pool <- NA
  conv_pool <- NA
  mean_correct <- NA
  total_correct <- NA



  # Checks on argument dimensions. ---------------------------------------------

  # Is est a square matrix?
  if(nrow(est) != ncol(est)){
    err_code <- 1
    err_message <- "The argument est must be a square matrix!"

    return(list(est = est_pool,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = conv_pool,
                err_code = err_code,
                err_message = err_message))
  }


  # Check matrix dimensions.
  if(ncol(pre) != nrow(est)){
    err_code <- 2
    err_message <- "The columns in pre must equal the rows in est!"

    return(list(est = est_pool,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = conv_pool,
                err_code = err_code,
                err_message = err_message))
  }

  if(ncol(est) != nrow(post)){
    err_code <- 3
    err_message <- "The columns in est must equal the rows in post!"

    return(list(est = est_pool,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = conv_pool,
                err_code = err_code,
                err_message = err_message))
  }

  if(nrow(pre) != ncol(post)){
    err_code <- 4
    err_message <- "The rows in pre must equal the columns in post!"

    return(list(est = est_pool,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = conv_pool,
                err_code = err_code,
                err_message = err_message))
  }


  # Check lengths of vectors.
  if(length(type_ss) != ncol(est) | length(n_conv) != ncol(est)){
    err_code <- 5
    err_message <- paste("The length of the vector type_ss or conv does not",
                         "equal the dimensions of est!")

    return(list(est = est_pool,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = conv_pool,
                err_code = err_code,
                err_message = err_message))
  }



  # Pool the partitioned estimates.
  est_pool <- pre %*% est %*% post



  # Pool the convergence vector.
  conv_pool <- as.vector(n_conv %*% post)



  # Compute the mean correct across prey types.
  mean_correct <- mean(diag(est_pool))



  # Compute the mean correct across individual prey.
  total_correct <- sum(diag(est_pool) * as.vector(type_ss %*% post))/
                   sum(type_ss)



  # Return.
  err_code <- 0
  err_message <- "Success!"

  return(list(est = est_pool,
              mean_correct = mean_correct,
              total_correct = total_correct,
              n_conv = conv_pool,
              err_code = err_code,
              err_message = err_message))
}
