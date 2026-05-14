#' Distance between fatty acid signatures and their mean
#'
#' The utility function \code{dist_sigs_2_mean} computes the summed and mean
#' distance between a collection of fatty acid signatures and their mean
#' signature.
#'
#'
#' @param sig_data A numeric matrix of fatty acid signatures in column-major
#'   format.
#' @param dist_meas An integer indicator of the distance measure to compute.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return A list containing the following elements:\describe{
#'   \item{dist_sum}{The summed distance between each signature and the mean
#'     signature.}
#'   \item{dist_mean}{The mean distance between each signature and the mean
#'     signature.}
#'   }
#'
#' @section Details:
#' This is an internal utility function.  The signature data in \code{sig_data}
#' are presumed to be ready for analysis, which is best accomplished by a call
#' to the function \code{\link{prep_sig}}.  Consequently, to increase execution
#' speed during simulations, no numeric error checking is performed.  Please
#' refer to the documentation for \code{\link{prep_sig}} for additional
#' information.
#'
#' Please refer to the documentation for \code{\link{dist_between_2_sigs}} for
#' additional information regarding distance measures.
#'
#' Utility functions called by \code{dist_sigs_2_mean}:
#'   \itemize{
#'   \item \code{\link{dist_between_2_sigs}}
#'   }
#'
################################################################################



dist_sigs_2_mean <- function(sig_data, dist_meas = 1, gamma = 1){


  # Initialize returned values.
  dist_sum <- NA
  dist_mean <- NA



  # Return a distance of 0 if a single signature is passed.
  if(is.vector(sig_data)){
    return(list(dist_sum=0, dist_mean=0))
    }



  # Determine the number of signatures.
  n_sig <- ncol(sig_data)



  # Compute the mean signature.
  sig_mean <- apply(X=sig_data, MARGIN=1, FUN=mean)



  # Compute the total distance between each signature and the mean.
  dist <- 0
  for(li1 in 1:n_sig){

    # Compute the distance.
    dist <- dist + dist_between_2_sigs(sig_data[,li1], sig_mean, dist_meas, gamma)
    }



# Return.
return(list(dist_sum = dist,
            dist_mean = dist/n_sig))
}
