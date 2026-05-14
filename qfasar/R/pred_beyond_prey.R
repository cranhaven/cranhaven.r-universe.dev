#' Identify predator signature proportions beyond range of prey
#'
#' The function \code{pred_beyond_prey} identifies predator signaturee
#' proportions that are outside the range of proportions observed in the
#' individual and mean prey signatures.
#'
#' @param pred_sigs A numeric matrix of predator signature(s) in
#'   column-major format. Intended to be the object \code{pred_sigs} returned by
#'   the function \code{\link{est_diet}}.
#' @param prey_sigs A numeric matrix of prey signatures in column-major format.
#'   Intended to be the object \code{prey_sigs} returned by the function
#'   \code{\link{est_diet}}.
#' @param mean_sigs A numeric matrix of mean prey-type signatures. Intended to
#'   be the object \code{prey_sigs} returned by the function
#'   \code{\link{est_diet}}.
#'
#' @return A list containing the following elements: \describe{
#'   \item{beyond_ind}{A logical matrix with TRUE indicating that the
#'     corresponding predator proportion is outside the range of individual
#'     prey proportions.}
#'   \item{beyond_mean}{A logical matrix with TRUE indicating that the
#'     corresponding predator proportion is outside the range of mean prey
#'     proportions.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#' @section Details:
#' In quantitative fatty acid signature analysis, predator signatues are assumed
#' to be a linear mixture of mean prey signatures (Iverson et al. 2004).
#' Predator signature proportions should therefore be within the range of
#' the prey signature proportions. Signature proportions outside the range of
#' prey proportions are indicative of a violation of one or both of the primary
#' model assumptions, i.e., the prey library is incomplete or the calibration
#' coefficients are inaccurate (Bromaghin et al. 2015, 2016a). Consequently,
#' checking for predator proportions that are outside the range of mean prey
#' proportions is an important diagnostic aid to evaluate the reliability
#' of diet estimates.
#'
#' The function \code{pred_beyond_prey} identifies predator signature
#' proportions that outside the range of proportions observed among the
#' individual and mean prey signatures. For purposes of diet estimation,
#' proportions outside the range of the mean signatures are most important.
#' However, \code{pred_beyond_prey} also identifies predator proportions that
#' are outside the range of the individual prey proportions for exploratory
#' purposes.
#'
#' \code{pred_beyond_prey} is designed to be called with inputs returned by the
#' function \code{\link{est_diet}}. Although it is not conceptually necessary to
#' estimate diets before performing this diagnostic check, doing so ensures that
#' the predator and prey signatures have been transformed to the optimization
#' space (Bromaghin et al. 2015) in which diets have been estimated.
#'
#' @section References:
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#'   Quantitative fatty acid signature analysis: A new method of
#'   estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' Bromaghin, J.F., S.M. Budge, G.W. Thiemann, and K.D. Rode. 2016. Assessing
#'   the robustness of quantitative fatty acid signature analysis to assumption
#'   violations. \emph{Methods in Ecology and Evolution} 7:51-59.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#' @examples
#' pred_beyond_prey(pred_sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                                       0.04, 0.11, 0.29, 0.56,
#'                                       0.10, 0.05, 0.35, 0.50,
#'                                       0.12, 0.03, 0.37, 0.48,
#'                                       0.10, 0.06, 0.35, 0.49,
#'                                       0.05, 0.15, 0.35, 0.45), ncol = 6),
#'                  prey_sigs = matrix(c(0.06, 0.09, 0.31, 0.54,
#'                                       0.05, 0.09, 0.30, 0.56,
#'                                       0.03, 0.10, 0.30, 0.57,
#'                                       0.08, 0.07, 0.30, 0.55,
#'                                       0.09, 0.05, 0.33, 0.53,
#'                                       0.09, 0.06, 0.34, 0.51,
#'                                       0.09, 0.07, 0.34, 0.50,
#'                                       0.08, 0.11, 0.35, 0.46,
#'                                       0.06, 0.14, 0.36, 0.44), ncol = 9),
#'                  mean_sigs = matrix(c(0.047, 0.093, 0.303, 0.557,
#'                                       0.087, 0.050, 0.323, 0.530,
#'                                       0.077, 0.106, 0.350, 0.467), ncol = 3))
#'
#' @export
#'
################################################################################

pred_beyond_prey <- function(pred_sigs, prey_sigs, mean_sigs){


  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.  The return objects pred_sig and prey_sig
  # are not included because they are input arguments and involved in the
  # following error checks.
  beyond_ind <- NA
  beyond_mean <- NA



  # Check that pred_sigs is a numeric matrix.
  if(!(is.numeric(pred_sigs) & is.matrix(pred_sigs))){
    err_code <- 1
    err_message <- "The argument pred_sigs is not a numeric matrix!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that pred_sigs are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(pred_sigs) < 0) | is.na(min(pred_sigs))){
    err_code <- 2
    err_message <- "One or more predator signatures are invalid!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Check that prey_sigs is a numeric matrix.
  if(!(is.numeric(prey_sigs) & is.matrix(prey_sigs))){
    err_code <- 3
    err_message <- "The argument prey_sigs is not a numeric matrix!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Check that prey_sigs are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(prey_sigs) < 0) | is.na(min(prey_sigs))){
    err_code <- 4
    err_message <- "One or more prey signatures are invalid!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Check that mean_sigs is a numeric matrix.
  if(!(is.numeric(mean_sigs) & is.matrix(mean_sigs))){
    err_code <- 5
    err_message <- "The argument mean_sigs is not a numeric matrix!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Check that mean_sigs are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(mean_sigs) < 0) | is.na(min(mean_sigs))){
    err_code <- 6
    err_message <- "One or more mean prey signatures are invalid!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Check that signature length are equal.
  if((nrow(pred_sigs) != nrow(prey_sigs)) | (nrow(pred_sigs) != nrow(mean_sigs))){
    err_code <- 7
    err_message <- "The signatures are not of equal length!"

    return(list(beyond_ind = beyond_ind,
                beyond_mean = beyond_mean,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Individual prey signatures -------------------------------------------------

  # Compute minima and maxima.
  min_prop <- matrix(data = apply(X = prey_sigs, MARGIN = 1, FUN = min),
                     nrow = nrow(pred_sigs), ncol = ncol(pred_sigs))
  max_prop <- matrix(data = apply(X = prey_sigs, MARGIN = 1, FUN = max),
                     nrow = nrow(pred_sigs), ncol = ncol(pred_sigs))

  # Identify proportions beyond prey range.
  beyond_ind <- (pred_sigs < min_prop) | (pred_sigs > max_prop)



  # Mean prey signatures -------------------------------------------------------

  # Compute minima and maxima.
  min_prop <- matrix(data = apply(X = mean_sigs, MARGIN = 1, FUN = min),
                     nrow = nrow(mean_sigs), ncol = ncol(pred_sigs))
  max_prop <- matrix(data = apply(X = mean_sigs, MARGIN = 1, FUN = max),
                     nrow = nrow(mean_sigs), ncol = ncol(pred_sigs))

  # Identify proportions beyond prey range.
  beyond_mean <- (pred_sigs < min_prop) | (pred_sigs > max_prop)




  # Return ---------------------------------------------------------------------

  err_code <- 0
  err_message <- "Success!"

  return(list(beyond_ind = beyond_ind,
              beyond_mean = beyond_mean,
              err_code = err_code,
              err_message = err_message))
  }
