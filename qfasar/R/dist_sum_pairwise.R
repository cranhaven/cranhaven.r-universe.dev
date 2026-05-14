#' Distance between pairs of fatty acid signatures
#'
#' The utility function \code{dist_sum_pairwise} computes the total distance
#' between all possible pairs of fatty acid signatures.
#'
#' @param sig_data A numeric matrix of fatty acid signatures in column-major
#'   format.
#' @param dist_meas An integer indicator of the distance measure to compute.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return The summed distance between all possible pairs of signatures.
#'
#' @section Details:
#' \code{dist_sum_pairwise} is an internal utility function.  The signature
#' data in \code{sig_data} are presumed to be ready for analysis, which is best
#' accomplished by a call to the function \code{prep_sig}.  Consequently, to
#' increase execution speed during simulations, no numeric error checking of the
#' signatures is performed.  Please refer to documentation for the function
#' \code{\link{prep_sig}} for information regarding signature preparation.
#'
#' Please refer to documentation for the function
#' \code{\link{dist_between_2_sigs}} for additional information regarding the
#' arguments \code{dist_meas} and \code{gamma}.
#'
#' Utility functions called by \code{dist_sigs_2_mean}:
#'   \itemize{
#'   \item \code{\link{dist_between_2_sigs}}
#'   }
#'
################################################################################



dist_sum_pairwise <- function(sig_data, dist_meas = 1, gamma = 1){


  # Return a distance of 0 if a single signature is passed.
  if(is.vector(sig_data)){
    return(0)
  }



  # Consider all possible pairs of signatures.
  n_sig <- ncol(sig_data)
  dist <- 0
  for(li1 in 1:(n_sig-1)){
    for(li2 in (li1+1):n_sig){

      # Compute the distance between these two signatures and accumulate.
      dist <- dist + dist_between_2_sigs(sig_data[,li1], sig_data[,li2],
                                         dist_meas, gamma)
    }
  }



  # Return.
  return(dist)
}
