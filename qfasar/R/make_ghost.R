#' Make a ghost prey signature
#'
#' Bromaghin et al (2016) studied the performance of QFASA estimators when
#' predators consumed a prey type that was not represented in the prey library,
#' termed a ghost prey.  \code{make_ghost} constructs a signature for a
#' ghost prey type.
#'
#' @param prey_sigs A matrix of prey signatures ready for analysis, intended to
#'   be the object \code{sig_scale} returned by a call to the function
#'   \code{\link{prep_sig}} with the prey data frame or the object
#'   \code{sig_part} returned by \code{\link{make_prey_part}}.
#' @param loc A matrix giving the first and last locations of the
#'   signatures of each prey type within \code{prey_sigs}, intended to be the
#'   object \code{loc} returned by a call to the function
#'   \code{\link{prep_sig}} with the prey data frame or the object
#'   \code{loc} returned by \code{\link{make_prey_part}}.
#' @param ghost_err A proportion strictly greater than 0 and less than 1 used to
#'   control the lower and upper bounds of ghost prey signature proportions.
#'   Default value 0.25.
#' @param dist_meas An integer indicator of the distance measure to be used.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return A list containing the following elements: \describe{
#'   \item{sig}{A numeric vector containing the ghost prey signature.}
#'   \item{dist}{Summed distance between the ghost signature and the mean prey
#'     signatures.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#' }
#'
#' @section Details:
#' One of the major assumptions of QFASA is that the prey library contains
#' representatives of all prey types consumed by a predator. Bromaghin et al.
#' (2016) investigated the robustness of diet estimators to violations of this
#' assumption. The function \code{make_ghost} constructs a ghost prey signature
#' using the methods of Bromaghin et al. (2016).
#'
#' The ghost prey signature is constructed by maximizing the summed distance
#' between the ghost prey signature and the mean prey signatures, while
#' constraining the ghost signature proportions within reasonable bounds to
#' ensure that the signature is somewhat realistic for the prey library. The
#' definition of reasonable bounds is embodied in the argument \code{ghost_err}.
#' \code{ghost_err} is a proportion greater than or equal to zero and less than 1
#' that is used to construct lower and upper bounds of the signature
#' proportions. The lower bound is obtained by multiplying \code{1 - ghost_err}
#' by the minimum mean prey proportion for each fatty acid. Similarly, the upper
#' bound is obtained by multiplying \code{1 + ghost_err} by the maximum mean
#' prey proportion for each fatty acid. The ghost prey signature is then
#' obtained by maximizing the summed distance between the signature and the mean
#' prey signatures, constraining the signature to lie within the
#' bounds and sum to 1. See \code{\link{est_diet}} for information regarding
#' distance measures.
#'
#' This method ensures that the ghost prey signature is somewhat distinct from
#' the other prey types, but not so wildly different that it represents a
#' completely different pattern from the other prey types. Although research
#' into suitable values for \code{ghost_err} has not been conducted, it is
#' probably advisable to use small to moderate values. Bromaghin et al. (2016)
#' used a value of 0.25. As the value of \code{ghost_err} is increased, the
#' resulting signature will tend to become increasing different from any prey
#' type in the library.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, G.W. Thiemann, and K.D. Rode. 2016. Assessing
#'   the robustness of quantitative fatty acid signature analysis to assumption
#'   violations. \emph{Methods in Ecology and Evolution} 7:51-59.
#'
#' @examples
#' make_ghost(prey_sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                                 0.04, 0.11, 0.29, 0.56,
#'                                 0.10, 0.05, 0.35, 0.50,
#'                                 0.12, 0.03, 0.37, 0.48,
#'                                 0.10, 0.06, 0.35, 0.49,
#'                                 0.05, 0.15, 0.35, 0.45), ncol=6),
#'            loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'            ghost_err = 0.15,
#'            dist_meas = 1,
#'            gamma = NA)
#'
#' make_ghost(prey_sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                                 0.04, 0.11, 0.29, 0.56,
#'                                 0.10, 0.05, 0.35, 0.50,
#'                                 0.12, 0.03, 0.37, 0.48,
#'                                 0.10, 0.06, 0.35, 0.49,
#'                                 0.05, 0.15, 0.35, 0.45), ncol=6),
#'            loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2))
#'
#' @export
#'
################################################################################


make_ghost <- function(prey_sigs, loc, ghost_err = 0.25, dist_meas = 1,
                       gamma = 1){


  # Check inputs ---------------------------------------------------------------

  sig <- NA
  dist <- NA


  # Check that prey_sigs is a numeric matrix.
  if(!(is.numeric(prey_sigs) & is.matrix(prey_sigs))){
    err_code <- 1
    err_message <- "The argument prey_sigs is not a numeric matrix!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that prey_sigs are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(prey_sigs) < 0) | is.na(min(prey_sigs))){
    err_code <- 2
    err_message <- "One or more prey signatures are invalid!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that loc is a numeric matrix.
  if(!(is.numeric(loc) & is.matrix(loc))){
    err_code <- 3
    err_message <- "The argument loc is not a numeric matrix!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that loc has at least two rows.
  if(nrow(loc) < 2){
    err_code <- 4
    err_message <- "The argument loc must have at least two rows!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the numbers of prey are equal.
  if(ncol(prey_sigs) != loc[nrow(loc),2]){
    err_code <- 5
    err_message <- "The number of prey in prey_sigs and loc differ!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check values of ghost_err.
  if(ghost_err < 0 | ghost_err >= 1){
    err_code <- 6
    err_message <- "The argument ghost_err must be >= 0 and < 1!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of dist_meas.
  if(!(dist_meas %in% 1:3)){
    err_code <- 7
    err_message <- "The argument dist_meas must equal 1, 2, or 3!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of gamma.
  if(dist_meas == 3 & (gamma <= 0 | gamma > 1)){
    err_code <- 8
    err_message <- "The argument gamma must be positive and no larger than 1!"

    return(list(sig = sig,
                dist = dist,
                err_code = err_code,
                err_message = err_message))
  }



  # Define functions --------------------------------------------------------

  # Define function constraining signature proportions to sum to 1.
  # Note: the optimizer Rsolnp::solnp() requires the variables passed to the
  # objective and constraint functions to be identical.
  sum_constr <- function(ghost, mean_sigs, dist_meas, gamma)
  {
    return(sum(ghost))
  }


  # Define distance measure. This process maximizes the distance between mean
  # prey signatures and the ghost prey signature.
  neg_dist <- function(ghost, mean_sigs, dist_meas, gamma){
    neg_dist <- 0
    for(li1 in 1:ncol(mean_sigs)){
      neg_dist <- neg_dist - dist_between_2_sigs(sig_1 = ghost,
                                                 sig_2 = mean_sigs[,li1],
                                                 dist_meas = dist_meas,
                                                 gamma = gamma)
    }
    return(neg_dist)
  }



  # Make ghost prey signature --------------------------------------------------

  # Compute mean prey signatures.
  n_prey_types <- nrow(loc)
  mean_sigs <- matrix(data = 0, nrow = nrow(prey_sigs), ncol = n_prey_types)
  for(li1 in 1:n_prey_types){
    mean_sigs[,li1] <- apply(X = as.matrix(prey_sigs[,loc[li1,1]:loc[li1,2]]),
                             MARGIN = 1,
                             FUN = mean)
  }
  colnames(mean_sigs) <- rownames(loc)
  rownames(mean_sigs) <- rownames(prey_sigs)



  # Compute min and max for each FA
  lb <- (1 - ghost_err)*apply(X = mean_sigs, MARGIN = 1, FUN = min)
  ub <- (1 + ghost_err)*apply(X = mean_sigs, MARGIN = 1, FUN = max)


  # Derive signature.
  guess <- apply(X = mean_sigs, MARGIN = 1, FUN = mean)
  sol <- Rsolnp::solnp(pars = guess,
                       fun = neg_dist,
                       eqfun = sum_constr,
                       eqB = 1,
                       LB = lb,
                       UB = ub,
                       mean_sigs = mean_sigs,
                       dist_meas = dist_meas,
                       gamma = gamma,
                       control = list(trace=0))



  # Clean up and return --------------------------------------------------------
  if(sol$convergence != 0){
    err_code <- 9
    err_message <- "Rsolnp::solnp failed to converge, solution is suspect!"
  } else{
    err_code <- 0
    err_message <- "Success!"
  }

  sig <- sol$pars
  names(sig) <- rownames(prey_sigs)

  dist <- -sol$values[length(sol$values)]


  return(list(sig = sig,
              dist = dist,
              err_code = err_code,
              err_message = err_message))
}






