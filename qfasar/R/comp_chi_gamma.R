#' Compute gamma parameter of chi-square distance measure
#'
#' The function \code{comp_chi_gamma} computes the gamma parameter of the
#' chi-square distance measure using the algorithm of Stewart et al. (2014).
#'
#' @param sigs A matrix of fatty acid signatures ready for analysis. Intended to
#'   be the object \code{sig_rep} returned by a call to the function
#'   \code{\link{prep_sig}}.
#' @param cc A vector of calibration coefficients, intended to be the object
#'   \code{cc} returned by the function \code{\link{prep_fa}} or
#'   \code{cc_aug}.
#' @param near_zero A small constant used to terminate the algorithm. Default
#'   value 0.00001.
#' @param min_gamma Smallest desired value of gamma, potentially used to
#'   terminate the algorithm. Default value 0.05.
#' @param space An integer indicator of the estimation space to be used. Default
#'   value 1.
#'
#' @return A list containing the following elements: \describe{
#'   \item{gamma}{The estimated value of gamma.}
#'   \item{gamma_vec}{A numeric vector containing the value of gamma at each
#'     step of the iteration.}
#'   \item{prop_vec}{A numeric vector containing the proportion of all possible
#'     two-element signatures with distance exceeding that of the full
#'     signatures at each step of the iteration. This value is compared to the
#'     argument near_zero.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#' }
#'
#' @section Details:
#' The chi-square distance involves a power transformation of signature
#' proportions, with the power parameter being denoted gamma.
#' \code{comp_chi_gamma} implements the algorithm of Stewart et al. (2014) to
#' find a suitable value of gamma.
#'
#' The algorithm is initialized with inv_gamma equal to 1 and gamma is computed
#' as 1/inv_gamma. The distances between all possible pairs of full signatures
#' are computed (distances). For each pair of full signatures, the distances
#' between all possible sub-signatures comprised of only two fatty acid
#' proportions are computed (sub-distances). The proportion of sub-distances
#' that exceed the corresponding distance is computed across all possible pairs
#' of signatures. If that proportion is less than the argument near_zero, the
#' function returns with gamma equal to 1. Otherwise, the function enters an
#' iterative phase. At each iteration, inv_gamma is incremented by 1, gamma is
#' computed as 1/inv_gamma, distances and sub-distances are recomputed, and the
#' proportion of the sub-distances that exceed their corresponding distance is
#' recomputed. The algorithm terminates when the proportion is less than the
#' argument near_zero or the value of gamma is less than min_gamma.
#'
#' The argument space must equal 1 or 2 (see \code{\link{est_diet}}). If its
#' value is 1, the calibration coefficients are used to map the signatures to
#' the predator space prior to initializing the algorithm.
#'
#' As the number of signatures in the library and/or the number of fatty acids
#' in a signature increases, the number of possible pairs of signatures and the
#' number of all possible two-proportion sub-signatures increases rapidly.
#' Consequently, this algorithm may require long run times. However, it only
#' needs to be run once for any particular library of signatures.
#'
#' @section References:
#' Stewart, C., S. Iverson, and C. Field. 2014. Testing for a change in diet
#'   using fatty acid signatures. \emph{Environmental and Ecological Statistics}
#'   21:775-792.
#'
#' @examples
#' comp_chi_gamma(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                                0.04, 0.11, 0.29, 0.56,
#'                                0.10, 0.05, 0.35, 0.50,
#'                                0.12, 0.03, 0.37, 0.48,
#'                                0.10, 0.06, 0.35, 0.49,
#'                                0.05, 0.15, 0.35, 0.45), ncol=6),
#'                cc = c(0.75, 1.00, 1.50, 0.90),
#'                near_zero = 0.05,
#'                min_gamma = 0.01,
#'                space = 1)
#'
#' @export
#'
################################################################################



comp_chi_gamma <- function(sigs, cc, near_zero = 0.00001, min_gamma = 0.05,
                           space = 1){


  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.
  gamma <- NA
  gamma_vec <- NA
  prop_vec <- NA


  # Check that sigs is a numeric matrix.
  if(!(is.numeric(sigs) & is.matrix(sigs))){
    err_code <- 1
    err_message <- "The argument sigs is not a numeric matrix!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that sigs are non-negative.
  if((min(sigs) < 0) | is.na(min(sigs))){
    err_code <- 2
    err_message <- "One or more signatures are invalid!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that cc is a numeric vector.
  if(!(is.vector(cc) & is.numeric(cc))){
    err_code <- 3
    err_message <- "The argument cc is not a numeric vector!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that number of fatty acids in sigs and cc are equal.
  if(nrow(sigs) != length(cc)){
    err_code <- 4
    err_message <- "The number of fatty acids in sigs and cc differ!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the the values in cc are all greater than 0.
  if(min(cc) <= 0 | is.na(min(cc))){
    err_code <- 5
    err_message <- "All calibration coefficients must exceed 0!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check value of near_zero.
  if(near_zero <= 0 | near_zero >= 1){
    err_code <- 6
    err_message <- "The argument near_zero must be > 0 and < 1!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check value of min_gamma.
  if(near_zero <= 0 | near_zero >= 1){
    err_code <- 7
    err_message <- "The argument min_gamma must be > 0 and < 1!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of space.
  if(space != 1 & space != 2){
    err_code <- 8
    err_message <- "The argument space must equal 1 or 2!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  }



  # Prepare for analysis -------------------------------------------------------

  # Define chi-square distance function.
  chi_dist <- function(sig_1, sig_2, gamma){

    # Close the signatures.
    sig_1 <- sig_1/sum(sig_1)

    sig_2 <- sig_2/sum(sig_2)


    # Compute the distance
    num <- (sig_1 - sig_2)^2
    denom <- sig_1 + sig_2
    denom[denom==0] <- 1
    dist <- sqrt(2*length(sig_1)*sum(num/denom))/gamma
  }



  # Compute constants.
  n_sig <- ncol(sigs)
  n_fa <- nrow(sigs)
  n_sig_pair <- choose(n = n_sig, k = 2)
  n_fa_pair <- choose(n = n_fa, k = 2)
  n_comb <- n_sig_pair*n_fa_pair

  have_gamma <- FALSE
  inv_gamma <- 0
  gamma_vec <- NULL
  prop_vec <- NULL


  # Allocate memory.
  dist_full <- vector(mode = "numeric", length = n_sig_pair)



  # Transform signatures to the specified space --------------------------------
  if(space == 1){
    cc_mat <- matrix(data = cc, nrow = n_fa, ncol = n_sig)
    sigs <- sigs*cc_mat

    prod_mat <- matrix(data = n_sig, nrow = n_fa,
                       ncol = n_sig, byrow=TRUE)
    sigs <- sigs/prod_mat
  }
  rm(cc_mat, prod_mat)



  # Compute gamma -------------------------------------------

  # Start the iterations.
  keep_going <- TRUE
  while(keep_going){

    # Update gamma.
    inv_gamma <- inv_gamma + 1
    gamma <- 1/inv_gamma
    if(gamma < min_gamma){
      keep_going <- FALSE
      break
    }



    # Power transform the signatures.
    # They are closed within the distance measure function.
    pow_sigs <- sigs^gamma



    # Compute the distances based on the full signatures.
    pair_ind <- 0
    for(li1 in 1:(n_sig-1)){
      for(li2 in (li1+1):n_sig){

        # Increment cell index.
        pair_ind <- pair_ind + 1

        # Compute distance between these signatures.
        dist_full[pair_ind] <- chi_dist(sig_1 = pow_sigs[,li1],
                                        sig_2 = pow_sigs[,li2],
                                        gamma = gamma)
      }
    }


    ## Compute proportion of two-part distances less than the full signature
    ## distance.

    # Initialize proportion.
    prop_greater <- 0


    # Consider each possible pair of signatures.
    pair_ind <- 0
    for(li1 in 1:(n_sig-1)){
      for(li2 in (li1+1):n_sig){

        # Increment cell index.
        pair_ind <- pair_ind + 1

        # Consider each possible pair of fatty acids for this signature pair.
        for(li3 in 1:(n_fa-1)){
          for(li4 in (li3+1):n_fa){
            this_dist <- chi_dist(sig_1 = pow_sigs[c(li3,li4),li1],
                                  sig_2 = pow_sigs[c(li3,li4),li2],
                                  gamma = gamma)
            if(this_dist > dist_full[pair_ind]){
              prop_greater <- prop_greater + 1
            }
          }
        }
      }
    }


    # Compute proportion of two-part distances greater than full signature
    # distances.
    prop_greater <- prop_greater/n_comb
    gamma_vec <- c(gamma_vec, gamma)
    prop_vec <- c(prop_vec, prop_greater)


    # Check for termination.
    if(prop_greater < near_zero){
      have_gamma <- TRUE
      keep_going <- FALSE
    }
  }



  # Check whether a value for gamma was found.
  if(have_gamma){
    err_code <- 0
    err_message <- "Success!"

    return(list(gamma = gamma,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))
  } else{
    err_code <- 9
    err_message <- "Caution, a value for gamma was not found!"

    return(list(gamma = NA,
                gamma_vec = gamma_vec,
                prop_vec = prop_vec,
                err_code = err_code,
                err_message = err_message))

  }
}




