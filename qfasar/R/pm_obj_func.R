#' Parameterized mean objective function
#'
#' The utility function \code{pm_obj_func} computes the total distance between
#' observed predator signatures and vector of mean diet proportions common to
#' all predators.
#'
#' @param diet A numeric vector of mean diet composition.
#' @param obs_sig A numeric matrix containing observed predator signatures, in
#'   column-major format.
#' @param mean_sigs A numeric matrix of the mean fatty acid signature for each
#'   prey type in the prey library, in column-major format.
#' @param dist_meas An integer indicator of the distance measure to compute.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return The total distance between observed and modeled signatures.
#'
#' @section Details:
#' This is an internal utility function.  Consequently, to increase execution
#' speed, no numeric error checking is performed within \code{pm_obj_func}.
#' Rather, error checking is presumed to have occurred at a higher level in the
#' calling sequence.
#'
#' The argument \code{obs_sig} is presumed to be a matrix of predator
#' signatures that has been prepared for analysis, which is best accomplished
#' by a call to the function \code{\link{prep_sig}} with the predator data.
#' Similarly, the contents of \code{mean_sigs} should be mean signatures computed
#' from signatures that were prepared for analysis by a call to the function
#' \code{\link{prep_sig}}.
#'
#' The argument \code{diet} is presumed to contain non-negative proportions that
#' sum to 1.0.
#'
#' The arguments \code{dist_meas} and \code{gamma} must be compatible with the
#' function \code{\link{dist_between_2_sigs}}.
#'
#' Please refer to the vignette and documentation for the functions
#' \code{\link{prep_sig}}, \code{\link{sig_scale}}, and
#' \code{\link{dist_between_2_sigs}} for additional details.
#'
#' \code{diet_obj_func} models a predator signature as a mixture of the mean
#' prey-type signatures, with the diet proportions as the mixture proportions,
#' returning the distance between the observed and modeled signatures.  The
#' diet composition of a predator is estimated by minimizing this function with
#' respect to the \code{diet} using the function Rsolnp::solnp.
#'
################################################################################


pm_obj_func <- function(diet, obs_sig, mean_sigs, dist_meas=1, gamma=1){

  # Initialize the distance.
  dist <- 0


  # Model the mean signature.
  diet_mat <- matrix(data = diet, nrow = nrow(mean_sigs),
                     ncol = ncol(mean_sigs), byrow = TRUE)
  mod_sig <- apply(diet_mat*mean_sigs, 1, sum)


  # Sum the distance between each predator signature and the modeled mean
  # signature.
  for(li1 in 1:ncol(obs_sig)){
    dist <- dist + dist_between_2_sigs(sig_1 = obs_sig[,li1],
                                       sig_2 = mod_sig,
                                       dist_meas = dist_meas,
                                       gamma = gamma)
  }

  # Return.
  return(dist)
}
