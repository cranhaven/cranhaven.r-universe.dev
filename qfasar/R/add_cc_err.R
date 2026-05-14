#' Add error to the calibration coefficients
#'
#' Bromaghin et al (2016) studied the performance of QFASA estimators when
#' predator diets were estimated using calibration coefficients that
#' incorporated a degree of error. \code{add_cc_err} implements their method of
#' adding error to a set of calibration coefficients.
#'
#' @param cc_true A vector of calibration coefficients, intended to be the object
#'   \code{cc} returned by the function \code{\link{prep_fa}}.
#' @param err_bound A proportion strictly greater than 0 and less than 1 used to
#'   control the lower and upper bounds of calibration coefficient error.
#'
#' @return A list containing the following elements: \describe{
#'   \item{cc}{A numeric vector of calibration coefficients with error
#'     incorporated.}
#'   \item{err}{The mean relative absolute error in the calibration
#'     coefficients.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#' }
#'
#' @section Details:
#' One of the major assumptions of QFASA is that the calibration coefficients
#' are known perfectly. Bromaghin et al. (2016) investigated the robustness of
#' diet estimators to violations of this assumption. The function
#' \code{add_cc_err} uses the methods of Bromaghin et al. (2016) to add error
#' to a set of calibration coefficients.
#'
#' The argument err_bound is used to compute box constraints for the calibration
#' coefficients: lower bound equals \code{(1 - err_bound)*cc_true} and upper
#' bound equals \code{(1 + err_bound)*cc_true}. A uniformly distributed random
#' number is generated between the bounds for each calibration coefficient and
#' the vector of coefficients is scaled so that their sum equals the sum of the
#' true calibration coefficients. Because only the relative magnitudes of the
#' calibration coefficients are important in diet estimation, scaling the
#' coefficients to have a common sum ensures comparability between multiple
#' sets of coefficients.
#'
#' The mean relative absolute difference between the true and error-added
#' calibration coefficients is computed as a measure of error for the entire
#' vector.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, G.W. Thiemann, and K.D. Rode. 2016. Assessing
#'   the robustness of quantitative fatty acid signature analysis to assumption
#'   violations. \emph{Methods in Ecology and Evolution} 7:51-59.
#'
#' @examples
#' add_cc_err(cc_true = c(0.75, 1.00, 1.50, 1.15),
#'            err_bound = 0.25)
#'
#' @export
#'
################################################################################

add_cc_err <- function(cc_true, err_bound){


  # Check inputs ---------------------------------------------------------------

  # Initialize returned objects.
  cc <- NA
  err <- NA


  # Check that cc_true is a numeric vector.
  if(!(is.vector(cc_true) & is.numeric(cc_true))){
    err_code <- 1
    err_message <- "The argument cc is not a numeric vector!"

    return(list(cc = cc,
                err = err,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the the values in cc_true are all greater than 0.
  if(min(cc_true) <= 0 | is.na(min(cc_true))){
    err_code <- 2
    err_message <- "All calibration coefficients must exceed 0!"

    return(list(cc = cc,
                err = err,
                err_code = err_code,
                err_message = err_message))
  }


  # Check value of err_bound.
  if(err_bound <= 0 | err_bound >= 1){
    err_code <- 3
    err_message <- "The argument err_bound must be > 0 and < 1!"

    return(list(cc = cc,
                err = err,
                err_code = err_code,
                err_message = err_message))
  }



  # Add error to calibration coefficients --------------------------------------

  # Compute number of fatty acids.
  n_fa <- length(cc_true)


  # Construct upper and lower bounds.
  cc_min <- (1 - err_bound)*cc_true
  cc_max <- (1 + err_bound)*cc_true



  # Add error and rescale.
  rand_num <- stats::runif(n = n_fa)
  cc <- cc_min + rand_num*(cc_max - cc_min)
  cc <- cc*sum(cc_true)/sum(cc)



  # Compute measure of error.
  err <- mean(abs(cc_true - cc)/cc_true)



  # Return.
  err_code <- 0
  err_message <- "Success!"

  return(list(cc = cc,
              err = err,
              err_code = err_code,
              err_message = err_message))
}
