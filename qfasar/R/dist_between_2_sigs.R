#' Compute the distance between two fatty acid signatures
#'
#' The utility function \code{dist_between_2_sigs} computes the distance
#' between two fatty acid signatures.
#'
#' @param sig_1,sig_2 Equal-length numeric vectors of fatty acid signature
#'   proportions.
#' @param dist_meas An integer indicator of the distance measure to compute.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure.  See
#'   Details. Default value 1.
#'
#' @return The selected distance between the two signatures.
#'
#'
#' @section Details:
#' This is an internal utility function.  The signatures in \code{sig_1} and
#' \code{sig_2} are presumed to be ready for analysis, which is best
#' accomplished by a call to the function \code{\link{prep_sig}}.  Consequently,
#' to increase execution speed during simulations, no numeric error checking is
#' performed.  Please refer to documentation for the function
#' \code{\link{prep_sig}} for additional details.
#'
#' If the argument \code{dist_meas} is not one of the following integers, a
#' value of NA is returned:
#' \itemize{
#' \item dist_meas == 1 yields the Aitchison distance measure (Stewart et al.
#'   2014).  This is the default value.
#' \item dist_meas == 2 yields the Kullback-Leibler distance measure of
#'   Iverson et al. (2004).
#' \item dist_meas == 3 yields the chi-square distance measure (Stewart et
#'   al. 2014).
#' }
#'
#' The argument \code{gamma} is only used if \code{dist_meas == 3} and need not
#' be passed otherwise.  If \code{dist_meas == 3}, \code{gamma} must be greater
#' than 0 and cannot exceed 1.  If \code{dist_meas == 3} and a value for
#' \code{gamma} is not passed, a default value of 1 is used.
#'
#' @section References:
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#' Quantitative fatty acid signature analysis: A new method of
#' estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' Stewart, C., S. Iverson, and C. Field. 2014. Testing for a change in diet
#' using fatty acid signatures. \emph{Environmental and Ecological
#' Statistics} 21:775-792.
#'
################################################################################



dist_between_2_sigs <- function(sig_1, sig_2, dist_meas = 1, gamma = 1){

  if(dist_meas == 1){

    ## Compute the Aitchison distance measure.

    # Compute the logs of the signatures.
    log_sig_1 <- log(sig_1)
    log_sig_2 <- log(sig_2)


    # Compute the logs of the geometric means of the signatures.
    log_gm_1 <- mean(log_sig_1)
    log_gm_2 <- mean(log_sig_2)


    # Subtract the log of the geometric means from the log of the signature
    # proportions.
    log_ratio_1 <- log_sig_1 - log_gm_1
    log_ratio_2 <- log_sig_2 - log_gm_2


    # Compute the distance between the signatures.
    dist <- sqrt(sum((log_ratio_1 - log_ratio_2)^2))
  } else if(dist_meas == 2){

    # Compute the Kullback-Leibler distance measure.
    dist <- sum((sig_1 - sig_2)*log(sig_1/sig_2))
  } else if(dist_meas == 3){

    ## Compute the chi-square distance measure.

    # Condition on the value of gamma.
    if(gamma > 0 & gamma <= 1){

      # Power transform the signatures.
      pow_sig_1 <- sig_1^gamma
      pow_sig_1 <- pow_sig_1/sum(pow_sig_1)

      pow_sig_2 <- sig_2^gamma
      pow_sig_2 <- pow_sig_2/sum(pow_sig_2)


      # Compute the distance
      num <- (pow_sig_1 - pow_sig_2)^2
      denom <- pow_sig_1 + pow_sig_2
      denom[denom==0] <- 1
      dist <- sqrt(2*length(sig_1)*sum(num/denom))/gamma
    } else{
      dist <- NA
    }
  } else{
    dist <- NA
  }


  # Return.
  return(dist)
}
