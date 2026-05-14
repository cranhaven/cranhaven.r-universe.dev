#' Calibration coefficient for an augmented signature proportion
#'
#' \code{cc_aug} computes the optimal calibration coefficient for an
#' augmented signature proportion (Bromaghin et al. 2016).
#' If signature augmentation is to be used, the user must call \code{cc_aug}
#' after first calling \code{\link{prep_sig}} with the prey signature data, in
#' order to derive a calibration coefficient for the augmented signature.
#'
#' @param sig_rep  A numeric matrix containing fatty acid signatures with
#'   proportions from all fatty acids.  See Details.
#' @param sig_scale A numeric matrix containing fatty acid signatures with
#'   proportions from a subset of all fatty acids and an augmented proportion.
#'   See Details.
#' @param cc_all A numeric vector of calibration coefficients for the fatty
#'   acids in \code{sig_rep}.
#' @param use_fa A logical vector denoting the fatty acids in \code{sig_rep}
#'   that are also in \code{sig_scale}.
#' @param dist_meas An integer indicator of the distance measure to compute.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return A list containing the following elements: \describe{
#'   \item{cc}{A numeric vector of calibration coefficients for the augmented
#'     signatures.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#' }
#'
#' @section Details:
#' Calibration coefficients provide a one-to-one mapping between the prey and
#' predator spaces (Bromaghin et al. 2015).  However, when using signature
#' augmentation (Bromaghin et al. 2016), no calibration coefficient is
#' available for the augmented proportion and the function \code{cc_aug} was
#' developed to remedy that lack.
#'
#' \code{cc_aug} transforms complete prey signatures in \code{sig_rep} to the
#' predator space, censors them using \code{fa}, and then augments them.
#' The subset of calibration coefficients in \code{cc_all} corresponding to
#' \code{fa} are combined with a calibration coefficient for the  augmented
#' proportion, the censored signatures in \code{sig_scale} are also transformed to
#' the predator space, and the distance between the two sets of censored
#' signatures is computed.  The calibration coefficient for the augmented
#' proportion is taken as the value that minimizes the distance.  The function
#' Rsolnp::solnp() is used to minimize the distance.
#'
#' The entity passed as the argument \code{fa} is intended to be the
#' corresponding entity returned by a call to \code{prep_fa}.  Similarly,
#' the entities passed as the arguments \code{sig_rep} and \code{sig_scale}
#' are intended to be the corresponding entities returned by a call to
#' \code{prep_sig} with data in a prey library.  Consequently, no error
#' checks are made on these objects.  Please refer to the documentation for
#' \code{\link{prep_fa}} and \code{\link{prep_sig}} for additional details.
#'
#' Use of Rsolnp::solnp limits the ability to return any errors from the
#' function \code{dist_between_2_sigs}.  A crash may be caused by passing
#' invalid values for the arguments \code{dist_meas} or \code{gamma}.  Please
#' refer to documentation for the function \code{\link{dist_between_2_sigs}} for
#' additional information about valid values for these arguments.
#'
#' Utility and external functions called by \code{cc_aug}:
#'   \itemize{
#'   \item \code{\link{dist_between_2_sigs}}
#'   \item Rsolnp::solnp
#'   }
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research} 31:597-606.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#' @examples
#' cc_aug(sig_rep = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                           0.04, 0.11, 0.29, 0.56,
#'                           0.10, 0.05, 0.35, 0.50), ncol = 3),
#'        sig_scale = matrix(c(0.40, 0.50, 0.10,
#'                             0.45, 0.49, 0.06,
#'                             0.35, 0.45, 0.20), ncol = 3),
#'        cc_all = c(0.75, 1.05, 1.86, 0.80),
#'        use_fa = c(FALSE, FALSE, TRUE, TRUE))
#'
#' cc_aug(sig_rep = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                           0.04, 0.11, 0.29, 0.56,
#'                           0.10, 0.05, 0.35, 0.50), ncol = 3),
#'        sig_scale = matrix(c(0.40, 0.50, 0.10,
#'                             0.45, 0.49, 0.06,
#'                             0.35, 0.45, 0.20), ncol = 3),
#'        cc_all = c(0.75, 1.05, 1.86, 0.80),
#'        use_fa = c(FALSE, FALSE, TRUE, TRUE),
#'        dist_meas = 1)
#'
#' cc_aug(sig_rep = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                           0.04, 0.11, 0.29, 0.56,
#'                           0.10, 0.05, 0.35, 0.50), ncol = 3),
#'        sig_scale = matrix(c(0.40, 0.50, 0.10,
#'                             0.45, 0.49, 0.06,
#'                             0.35, 0.45, 0.20), ncol = 3),
#'        cc_all = c(0.75, 1.05, 1.86, 0.80),
#'        use_fa = c(FALSE, FALSE, TRUE, TRUE),
#'        dist_meas = 2)
#'
#' cc_aug(sig_rep = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                           0.04, 0.11, 0.29, 0.56,
#'                           0.10, 0.05, 0.35, 0.50), ncol = 3),
#'        sig_scale = matrix(c(0.40, 0.50, 0.10,
#'                             0.45, 0.49, 0.06,
#'                             0.35, 0.45, 0.20), ncol = 3),
#'        cc_all = c(0.75, 1.05, 1.86, 0.80),
#'        use_fa = c(FALSE, FALSE, TRUE, TRUE),
#'        dist_meas = 3,
#'        gamma = 0.25)
#'
#' cc_aug(sig_rep = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                           0.04, 0.11, 0.29, 0.56,
#'                           0.10, 0.05, 0.35, 0.50), ncol = 3),
#'        sig_scale = matrix(c(0.40, 0.50, 0.10,
#'                             0.45, 0.49, 0.06,
#'                             0.35, 0.45, 0.20), ncol = 3),
#'        cc_all = c(0.75, 1.05, 1.86, 0.80),
#'        use_fa = c(FALSE, FALSE, TRUE, TRUE),
#'        dist_meas = 3)
#'
#' @export
#'
################################################################################


cc_aug <- function(sig_rep, sig_scale, cc_all, use_fa, dist_meas = 1,
                   gamma = 1){


  # Check that all fatty acids are not being used.
  if(length(use_fa) == sum(use_fa)){
    err_code <- 1
    err_message <- paste("Augmentation is not possible if all fatty acids",
                         "are to be used in the analysis!", sep = " ")

    return(list(cc = NA,
                err_code = err_code,
                err_message = err_message))
  }



  # Transform complete prey signatures to the predator space.
  sig_rep_t <- sig_rep*matrix(data = cc_all, nrow = nrow(sig_rep),
                                ncol = ncol(sig_rep))
  sig_rep_t <- sig_rep_t/matrix(data = colSums(sig_rep_t),
                                  nrow = nrow(sig_rep),
                                  ncol = ncol(sig_rep), byrow=TRUE)



  # Censor the transformed fatty acids that are not to be used in the analysis
  # and augment the signatures.
  sig_rep_t <- sig_rep_t[use_fa,]
  sig_rep_t <- rbind(sig_rep_t, 1-colSums(sig_rep_t))


  # Define the objective function.
  obj_func <- function(new_cc, sig_rep_t, sig_scale, old_cc, dist_meas, gamma){

    # Combine calibration coefficients.
    cc <- c(old_cc, new_cc)

    # Transform adjusted signatures.
    sig_scale_t <- sig_scale*matrix(data = cc, nrow = nrow(sig_scale),
                                    ncol = ncol(sig_scale))
    sig_scale_t <- sig_scale_t/matrix(data = colSums(sig_scale_t),
                                      nrow = nrow(sig_scale),
                                      ncol = ncol(sig_scale), byrow=TRUE)

    # Compute the distance between the two sets of signatures.
    dist <- 0
    for(li1 in 1:ncol(sig_rep)){
      dist <- dist + dist_between_2_sigs(sig_1 = sig_rep_t[,li1],
                                         sig_2 = sig_scale_t[,li1],
                                         dist_meas = dist_meas,
                                         gamma = gamma)
    }
    return(dist)
  }


  # Estimate the calibration coefficient for augmented proportion.
  guess <- 1
  lb <- 0
  old_cc <- cc_all[use_fa]
  est <- Rsolnp::solnp(pars = guess,
                       fun = obj_func,
                       LB = lb,
                       control=list(trace = 0),
                       sig_rep_t = sig_rep_t,
                       sig_scale = sig_scale,
                       old_cc=old_cc,
                       dist_meas = dist_meas,
                       gamma = gamma)


  # Return.
  if(est$convergence == 0){
    cc <- c(old_cc, est$pars)
    names(cc) <- c(rownames(sig_rep)[use_fa], "Augmented")

    err_code <- 0
    err_message <- "Success!"
  } else{
    cc <- NA

    err_code <- 2
    err_message <- "Unknown error in Rsolnp::solnp()"
  }
  return(list(cc = cc,
              err_code = err_code,
              err_message = err_message))
}
