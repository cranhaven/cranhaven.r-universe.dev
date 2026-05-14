#' Creats a map of the distance between pairs of fatty acid signatures
#'
#' The utility function \code{dist_pairs_map} computes the distance between all
#' possible pairs of fatty acid signatures within each type of prey or predator.
#'
#' @param sig_data A numeric matrix of fatty acid signatures in column-major
#'   format.
#' @param dist_meas An integer indicator of the distance measure to compute.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return A list containing the following elements:\describe{
#'   \item{n_sig}{The number of signatures (columns) in \code{sig_data}.}
#'   \item{sig_1}{The column of \code{sig_data} containing one signature.}
#'   \item{sig_2}{The column of \code{sig_data} containing the other
#'     signature.}
#'   \item{dist}{The distance between signatures \code{sig_1} and \code{sig_2}.}
#' }
#'
#' @section Details:
#' This is an internal utility function.  The signature data in \code{sig_data}
#' are presumed to be ready for analysis, which is best accomplished by a call
#' to the function \code{prep_sig}.  Consequently, to increase execution
#' speed during simulations, no numeric error checking is performed.  Please
#' refer to the documentation for \code{\link{prep_sig}} for additional
#' information.
#'
#' Please refer to the documentation for \code{\link{dist_between_2_sigs}} for
#' additional information regarding distance measures.
#'
#' Storing the distances between all possible pairs of fatty acid signatures
#' along with the locations of each pair requires less memory than a square
#' matrix of all possible pairs, while allowing the location of the signatures
#' to be easily determined.
#'
#' Utility functions called by \code{dist_sigs_2_mean}:
#'   \itemize{
#'   \item \code{\link{dist_between_2_sigs}}
#'   }
#'
################################################################################



dist_pairs_map <- function(sig_data, dist_meas = 1, gamma = 1){


  # Initialize components of the list.
  n_sig <- NA
  sig_1 <- NA
  sig_2 <- NA
  dist <- NA



  # Check that we have more than one signature.
  if(is.vector(sig_data)){
    return(list(n_sig = n_sig,
                sig_1 = sig_1,
                sig_2 = sig_2,
                dist = dist))
  }



  # Determine the number of signatures and compute the number of pairs.
  n_sig <- ncol(sig_data)
  n_pairs <- n_sig*(n_sig - 1)/2



  # Allocate memory for the results.
  sig_1 <- vector(mode = "integer", length = n_pairs)
  sig_2 <- sig_1
  dist <- vector(mode = "numeric", length = n_pairs)



  # Consider all possible pairs of signatures.
  counter <- 0
  for(li1 in 1:(n_sig-1)){
    for(li2 in (li1+1):n_sig){

      # Compute the distance between these two signatures.
      counter <- counter + 1
      sig_1[counter] <- li1
      sig_2[counter] <- li2
      dist[counter] <- dist_between_2_sigs(sig_data[,li1], sig_data[,li2],
                                           dist_meas, gamma)
    }
  }



  # Return.
  return(list(n_sig = n_sig,
              sig_1 = sig_1,
              sig_2 = sig_2,
              dist = dist))
}
