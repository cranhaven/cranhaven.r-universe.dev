#' Leave-one-prey-out analysis
#'
#' The function \code{lopo} evaluates the distinctiveness of a prey library by
#' performing a leave-one-prey-out analysis.
#'
#' @param sigs A numeric matrix of fatty acid signatures in column-major
#'   format.
#' @param type A character vector of prey or predator type names.
#' @param uniq_types A character vector of the unique types, sorted
#'     alphanumerically.
#' @param type_ss The number of signatures (sample size) for each unique
#'   \code{type}.
#' @param loc A numeric matrix specifying the location of signatures within
#'   \code{sigs} for each \code{uniq_types}.
#' @param dist_meas A integer indicator of the distance measure to use. Default
#'   value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return A list containing the following elements: \describe{
#'   \item{est}{A square matrix containing the mean distribution of estimates
#'     among all prey types, by prey-type.}
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
#' The object passed as the argument \code{sigs} is intended to be the signature
#' object returned by \code{\link{sig_scale}} or, if the prey library has been
#' partitioned, by \code{\link{make_prey_part}}.
#'
#' The objects passed as the arguments \code{type}, \code{uniq_types},
#' \code{type_ss}, and \code{loc} are intended to be the corresponding objects
#' returned by \code{\link{prep_sig}} or, if the prey library has been
#' partitioned, by \code{\link{make_prey_part}}.
#'
#' The arguments \code{dist_meas} and \code{gamma} must be compatible with the
#' function \code{\link{dist_between_2_sigs}}.
#'
#' \code{lopo} performs a leave-one-prey-out analysis with a prey library and
#' defined prey types (Bromaghin et al. 2016b).  Each signature is
#' temporarily removed from the library, the mean prey-type signature is
#' recomputed, and the "diet" of the removed signature is estimated, after which
#' the removed signature is returned to the library.  This is done for each
#' signature in turn.  The mean estimate for each prey type is returned as a
#' row of \code{est}.  Perfect estimation would result in the square matrix
#' \code{est} having 1.0 along its diagonal and 0.0 in all off-diagonal
#' positions.  Large off-diagonal elements are indicative of confounding, or
#' similarity, between the corresponding prey types.  The returned object
#' \code{mean_correct} is the mean of the diagonal elements of \code{est},
#' while the returned object \code{total_correct} is the mean computed over all
#' signatures in the prey library.
#'
#' Note: the statistics are computed based on the estimates that successfully
#' converge (\code{n_conv}) and prey types that only have a sample size of 1 are
#' skipped.
#'
#' Because of the numerical optimization involved in a leave-one-prey-out
#' analysis, \code{lopo} can take a few minutes to run with a large prey
#' library.
#'
#' The statistics computed by \code{lopo} are one measure of the distinctiveness
#' of prey types within a prey library.  However, it is important to be aware
#' that such statistics are not necessarily informative of the ability of QFASA
#' to accurately estimate predator diets, as Bromaghin et al. (2015, 2016a,
#' 2016b) found that QFASA performance depends strongly on the interaction
#' between characteristics of a prey library, the specific diet of a predator,
#' and the accuracy of the calibration coefficients.  Consequently, the user is
#' warned not to misinterpret or misrepresent these statistics.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016b. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research} 31:597-606.
#'
#' Bromaghin, J.F., S.M. Budge, G.W. Thiemann, and K.D. Rode. 2016b. Assessing
#'   the robustness of quantitative fatty acid signature analysis to assumption
#'   violations. \emph{Methods in Ecology and Evolution} 7:51-59.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#' @examples
#' lopo(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                      0.04, 0.11, 0.29, 0.56,
#'                      0.10, 0.05, 0.35, 0.50,
#'                      0.12, 0.03, 0.37, 0.48,
#'                      0.10, 0.06, 0.35, 0.49,
#'                      0.05, 0.15, 0.35, 0.45), ncol=6),
#'      type = c("Type_1", "Type_1", "Type_2", "Type_2", "Type_3", "Type_3"),
#'      uniq_types = c("Type_1", "Type_2", "Type_3"),
#'      type_ss <- c(2, 2, 2),
#'      loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'      dist_meas = 1)
#'
#' lopo(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                      0.04, 0.11, 0.29, 0.56,
#'                      0.10, 0.05, 0.35, 0.50,
#'                      0.12, 0.03, 0.37, 0.48,
#'                      0.10, 0.06, 0.35, 0.49,
#'                      0.05, 0.15, 0.35, 0.45), ncol=6),
#'      type = c("Type_1", "Type_1", "Type_2", "Type_2", "Type_3", "Type_3"),
#'      uniq_types = c("Type_1", "Type_2", "Type_3"),
#'      type_ss <- c(2, 2, 2),
#'      loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'      dist_meas = 2)
#'
#' lopo(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                      0.04, 0.11, 0.29, 0.56,
#'                      0.10, 0.05, 0.35, 0.50,
#'                      0.12, 0.03, 0.37, 0.48,
#'                      0.10, 0.06, 0.35, 0.49,
#'                      0.05, 0.15, 0.35, 0.45), ncol=6),
#'      type = c("Type_1", "Type_1", "Type_2", "Type_2", "Type_3", "Type_3"),
#'      uniq_types = c("Type_1", "Type_2", "Type_3"),
#'      type_ss <- c(2, 2, 2),
#'      loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'      dist_meas = 3,
#'      gamma = 0.25)
#'
#' lopo(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                      0.04, 0.11, 0.29, 0.56,
#'                      0.10, 0.05, 0.35, 0.50,
#'                      0.12, 0.03, 0.37, 0.48,
#'                      0.10, 0.06, 0.35, 0.49,
#'                      0.05, 0.15, 0.35, 0.45), ncol=6),
#'      type = c("Type_1", "Type_1", "Type_2", "Type_2", "Type_3", "Type_3"),
#'      uniq_types = c("Type_1", "Type_2", "Type_3"),
#'      type_ss <- c(2, 2, 2),
#'      loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'      dist_meas = 3)
#'
#' lopo(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                      0.04, 0.11, 0.29, 0.56,
#'                      0.10, 0.05, 0.35, 0.50,
#'                      0.12, 0.03, 0.37, 0.48,
#'                      0.10, 0.06, 0.35, 0.49,
#'                      0.05, 0.15, 0.35, 0.45), ncol=6),
#'      type = c("Type_1", "Type_1", "Type_2", "Type_2", "Type_3", "Type_3"),
#'      uniq_types = c("Type_1", "Type_2", "Type_3"),
#'      type_ss <- c(2, 2, 2),
#'      loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2))
#'
#' @export
#'
################################################################################


lopo <- function(sigs, type, uniq_types, type_ss, loc, dist_meas = 1,
                 gamma = 1){



  # Check inputs for errors ----------------------------------------------------

  # Initialize objects to be returned.
  est <- NA
  mean_correct <- NA
  total_correct <- NA
  n_conv <- NA


  # Check that sigs is a numeric matrix.
  if(!(is.numeric(sigs) & is.matrix(sigs))){
    err_code <- 1
    err_message <- "The sigs argument is not a numeric matrix!"

    return(list(est = est,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = n_conv,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the signatures are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(sigs) < 0) | is.na(min(sigs)) | (max(apply(sigs, 2, sum)) > 1)){
    err_code <- 2
    err_message <- "One or more signatures are invalid!"

    return(list(est = est,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = n_conv,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of dist_meas.
  if(!(dist_meas %in% 1:3)){
    err_code <- 3
    err_message <- "The argument dist_meas must equal 1, 2, or 3!"

    return(list(est = est,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = n_conv,
                err_code = err_code,
                err_message = err_message))
  }


  # Check combinations of dist_meas, gamma, and the minimum signature proportion.
  dum <- min(sigs)
  if(dist_meas < 3){

    # Check value of the minimum signature proportion.
    if(dum <= 0){
      err_code <- 4
      err_message <- paste("If dist_meas equals 1 or 2,",
                           "signature proportions must exceed 0!",
                           sep=" ")

      return(list(est = est,
                  mean_correct = mean_correct,
                  total_correct = total_correct,
                  n_conv = n_conv,
                  err_code = err_code,
                  err_message = err_message))
    }
  } else{

    # Check value of the minimum signature proportion.
    if(dum < 0){
      err_code <- 5
      err_message <- "Signature proportions cannot be negative!"

      return(list(est = est,
                  mean_correct = mean_correct,
                  total_correct = total_correct,
                  n_conv = n_conv,
                  err_code = err_code,
                  err_message = err_message))
    }

    # Check value of gamma.
    if(is.na(gamma) | gamma <= 0 | gamma > 1){
      err_code <- 6
      err_message <- paste("If dist_meas equals 3, gamma must exceed 0 and",
                           "cannot exceed 1!",
                           sep=" ")

      return(list(est = est,
                  mean_correct = mean_correct,
                  total_correct = total_correct,
                  n_conv = n_conv,
                  err_code = err_code,
                  err_message = err_message))
    }
  }


  # Check the number of signatures.
  if((ncol(sigs) != loc[nrow(loc),2]) | (ncol(sigs) != length(type))){
    err_code <- 7
    err_message <- paste("Information on the number of signatures in the",
                         "arguments sig, type, or loc conflict!",
                         sep=" ")

    return(list(est = est,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = n_conv,
                err_code = err_code,
                err_message = err_message))
  }


  if(length(uniq_types) != nrow(loc)){
    err_code <- 8
    err_message <- paste("The number of unique type names does not equal",
                         "the number of rows in loc!",
                         sep=" ")

    return(list(est = est,
                mean_correct = mean_correct,
                total_correct = total_correct,
                n_conv = n_conv,
                err_code = err_code,
                err_message = err_message))
  }



  # Prepare for the lopo analysis ----------------------------------------------

  # Define function constraining diet proportions to sum to 1.
  # Note: the optimizer Rsolnp::solnp() requires the variables passed to the
  # objective and constraint functions to be identical.
  sum_constr <- function(diet, obs_sig, mean_sigs, dist_meas=1, gamma=1)
  {
    return(sum(diet))
  }


  # Allocate memory.
  n_types <- length(uniq_types)

  est <- matrix(data = 0, nrow = n_types, ncol = n_types)
  colnames(est) <- uniq_types
  rownames(est) <- uniq_types

  n_conv <- vector(mode = "integer", length = n_types)


  # Compute mean prey signatures.
  mean_sigs <- stats::aggregate(x = t(sigs), by = list(type), FUN = mean)
  mean_sigs <- t(mean_sigs[,-1])



  # Initialize optimization variables.
  guess <- rep(1/n_types, n_types)
  low_bound <- rep(0, n_types)
  up_bound <- rep(1, n_types)



  # Leave-one-prey-out analysis ------------------------------------------------

  # Initialize variables.
  prey_index <- 0
  mean_correct <- 0
  total_correct <- 0


  # Consider each prey type.
  for(li1 in 1:n_types){

    # Skip this prey type if there is only one signature.
    if(type_ss[li1] == 1){
      prey_index <- prey_index + 1
      next
    }


    # Consider each individual in this prey type.
    for(li2 in 1:type_ss[li1]){

      # Increment prey index.
      prey_index <- prey_index + 1


      # Remove the information from this prey animal from the mean prey
      # signature.
      temp_mean <- mean_sigs
      temp_mean[,li1] <- (type_ss[li1]*temp_mean[,li1] -
                          sigs[,prey_index])/(type_ss[li1]-1)


      # Estimate this animal's diet.
      this_est <- Rsolnp::solnp(pars = guess, fun = diet_obj_func,
                                eqfun = sum_constr, eqB = 1, LB = low_bound,
                                UB = up_bound, obs_sig = sigs[,prey_index],
                                mean_sigs = temp_mean, dist_meas = dist_meas,
                                gamma = gamma, control = list(trace=0))


      # Store estimate.
      if(this_est$convergence == 0){
        n_conv[li1] <- n_conv[li1] + 1
        est[li1,] <- est[li1,] + this_est$pars
        total_correct <- total_correct + this_est$pars[li1]
      }
    } # end li2


    # Compute mean for this prey type.
    if(n_conv[li1] > 0){
      est[li1,] <- est[li1,]/n_conv[li1]
      mean_correct <- mean_correct + est[li1,li1]
    }
  } # end li1



  # Return.
  mean_correct <- mean_correct/sum(n_conv > 0)
  sum_conv <- sum(n_conv)
  if(sum_conv > 0){
    total_correct <- total_correct/sum_conv
  }

  err_code <- 0
  err_message <- "Success!"

  return(list(est = est,
              mean_correct = mean_correct,
              total_correct = total_correct,
              n_conv = n_conv,
              err_code = err_code,
              err_message = err_message))
}
