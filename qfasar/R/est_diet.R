#' Estimate predator diet composition
#'
#' \code{est_diet} estimates the diet of one or more predators.
#'
#' @param pred_sigs A vector or matrix of predator signatures ready for
#'   analysis, intended to be the object \code{sig_scale} returned by a call to
#'   the function \code{\link{prep_sig}} with the predator data frame.
#' @param pred_uniq_types A character vector of the unique predator types,
#'   intended to be the object \code{uniq_types} returned by a call to the
#'   function \code{\link{prep_sig}} with the predator data frame.
#' @param pred_loc A vector or matrix giving the first and last locations of the
#'   signatures of each predator type within \code{pred_sigs}, intended to be
#'   the object \code{loc} returned by a call to the function
#'   \code{\link{prep_sig}} with the predator data frame.
#' @param prey_sigs A matrix of prey signatures ready for analysis, intended to
#'   be the object \code{sig_scale} returned by a call to the function
#'   \code{\link{prep_sig}} with the prey data frame or the object
#'   \code{sig_part} returned by \code{\link{make_prey_part}}.
#' @param prey_uniq_types A character vector of the unique prey types, intended
#'   to be the object \code{uniq_types} returned by a call to the function
#'   \code{\link{prep_sig}} with the prey data frame.
#' @param prey_loc A matrix giving the first and last locations of the
#'   signatures of each prey type within \code{prey_sigs}, intended to be the
#'   object \code{loc} returned by a call to the function
#'   \code{\link{prep_sig}} with the prey data frame.
#' @param cc A vector of calibration coefficients, intended to be a subset of
#'   the object \code{cc} returned by the function \code{\link{prep_fa}} or the
#'   object \code{cc} returned by the function \code{\link{cc_aug}}.
#' @param space An integer indicator of the estimation space to be used. Default
#'   value 1.
#' @param dist_meas An integer indicator of the distance measure to be used.
#'   Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#' @param ind_boot The number of bootstrap replications to use in the estimation
#'   of the variance of an individual predator's diet. Default value 100.
#' @param mean_meth An integer indicator of the estimation method for mean diet.
#'   Default value 1.
#' @param var_meth An integer indicator of the estimation method for the
#'   variance of mean diet. Default value 1 (bootstrap estimator).
#' @param mean_boot The number of bootstrap replications to use, needed only if
#'   the bootstrap method of estimating the variance of meat diet is selected
#'   (var_meth == 1). Default value 100.
#'
#' @return A list containing the following elements: \describe{
#'   \item{pred_sigs}{A numeric matrix of predator signatures, potentially
#'     transformed to the prey space.}
#'   \item{prey_sigs}{A numeric matrix of prey signatures, potentially
#'     transformed to the predator space.}
#'   \item{mean_sigs}{A numeric matrix of mean prey-type signatures, potentially
#'     transformed to the predator space.}
#'   \item{est_ind}{A numeric matrix of the estimated diet compositions of
#'     individual predators.}
#'   \item{conv}{A logical vector indicating whether the optimization function
#'     successfully converged.}
#'   \item{obj_func}{A numeric vector of the values of the objective function at
#'     each predator's estimated diet.}
#'   \item{mod_sigs}{A numeric matrix of the modeled signature of each predator
#'     at its estimated diet.}
#'   \item{var_ind}{A numeric array containing the estimated variance matrix for
#'     the estimated mean diet of each predator.}
#'   \item{est_mean}{A numeric matrix containing the estimated mean diet of each
#'     predator type.}
#'   \item{conv_mean}{A logical vector indicating whether the estimated mean
#'     diet of each predator type is based on at least one diet estimate that
#'     converged.}
#'   \item{var_mean}{A numeric array containing the estimated variance matrix
#'     for the estimated mean diet of each predator type.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#' }
#'
#' @section Details:
#' \code{est_diet} estimates the diet of one or more predators.  It implements a
#' variety of estimation options, and is therefore one of the more complicated
#' functions in \code{qfasar}.  Please read the following information and the
#' Diet Estimation section of the vignette for a description of the options.
#'
#' The objects passed via the arguments \code{pred_sig}, \code{pred_uniq_types},
#' and \code{pred_loc} are presumed to be the objects \code{sig_scale},
#' \code{uniq_types}, and \code{loc}, respectively, returned by a call to the
#' function \code{\link{prep_sig}} with the predator data frame.
#'
#' The objects passed via the arguments \code{prey_sig}, \code{prey_uniq_types},
#' and \code{prey_loc} are presumed to be the objects \code{sig_scale},
#' \code{uniq_types}, and \code{loc}, respectively, returned by a call to the
#' function \code{\link{prep_sig}} with the prey data frame.
#'
#' The object passed via the argument \code{cc} is presumed to be a subset of
#' the object \code{cc} returned by a call to the function
#' \code{\link{prep_fa}} with the fatty acid suite data frame. Use the logical
#' vector \code{use} returned by \code{\link{prep_fa}} to subset the list of
#' all calibration coefficients to the suite to be used for diet estimation.
#' Alternatively, if signature augmentation is used, pass the object \code{cc}
#' returned by the funtion \code{\link{cc_aug}}.
#'
#' Bromaghin et al. (2015) introduced the terms \strong{prey space} and
#' \strong{predator space}. These terms refer to the simplexes in which the prey
#' and predator signatures reside. The spaces differ due to predator metabolism
#' of ingested prey tissue and the resulting modification of signature
#' proportions. The calibration coefficients \code{cc} provide a one-to-one
#' mapping or transformation between the prey and predator spaces.  Diet
#' estimation can be performed in either space. Iverson et al. (2004) used
#' calibration coefficients to map predator signatures to the prey space, while
#' Bromaghin et al. (2013) took the opposite approach and mapped prey signatures
#' to the predator space. Simulation work has not revealed any strong reason
#' to prefer one space over the other (Bromaghin et al. 2015). However, be aware
#' that some distance measures will produce different diet estimates in the two
#' spaces.  Please see the vignette for more information.
#'
#' Estimation space options:
#' \itemize{
#'   \item space == 1 Estimation in the predator space, the default value.
#'   \item space == 2 Estimation in the prey space.
#' }
#'
#' \code{qfasar} implements three distance measures that have been used by QFASA
#' practitioners and researchers: Aitchison, Kullback-Leibler, and chi-square.
#' The argument \code{gamma} is a parameter of the chi-square distance measure
#' and its value must be strictly greater than 0 and less than or equal to 1.
#' The distance measure options are:
#' \itemize{
#' \item dist_meas == 1 yields the Aitchison distance measure (Stewart et al.
#'   2014).  This is the default value.
#' \item dist_meas == 2 yields the Kullback-Leibler distance measure of
#'   Iverson et al. (2004).
#' \item dist_meas == 3 yields the chi-square distance measure (Stewart et
#'   al. 2014).
#' }
#' Please refer to the vignette for additional information about distance
#' measures.
#'
#' The covariance matrix of each estimated diet can be estimated by bootstrap
#' sampling the prey library.  The signatures of each prey type are
#' independently sampled with replacement and the predator diet is
#' estimated with the bootstrapped library.  This is replicated \code{ind_boot}
#' times and the covariance matrix is estimated from the replicated estimates
#' (Beck et al. 2007, Bromaghin et al. 2015). If you do not wish to estimate
#' variances for the individual diet estimates, pass a bootstrap sample size
#' of 0 via the argument ind_boot.
#'
#' \code{qfasar} implements two methods of estimating the mean diet of each
#' class of predator.  The first is the empirical mean of the estimated diets.
#' In the second method, called the \strong{parameterized mean} method, the
#' model is parameterized with a single vector of diet proportions common to all
#' predators and mean diet is estimated by minimizing the distance between the
#' signature modeled from the mean diet proportions and each predator's
#' observed signature, summed over all predators.  The parameterized mean method
#' has not yet been thoroughly tested and its inclusion is intended to
#' facilitate future research.  Our limited and unpublished work with the
#' parameterized mean estimator suggests it may perform well when predator
#' signatures are homogeneous, but may be more sensitive to the presence of
#' predators with quite different signatures than the empirical estimator.
#' The options for \code{mean_meth} are:
#' \itemize{
#' \item mean_meth == 0 skips estimation of mean diet.
#' \item mean_meth == 1 yields the empirical estimate of mean diet.  This is the
#'   default value.
#' \item mean_meth == 2 yields the parameterized mean estimate of mean diet.
#' }
#'
#' \code{qfasar} implements two methods of estimating the variance of mean diet
#' estimates, the variance estimator of Beck et al. (2007) and a bootstrap
#' estimator, controlled by the argument \code{var_meth}.  The bootstrap
#' estimator draws independent samples of each prey type to form a bootstrap
#' prey library and a random sample of each predator type, with sample sizes
#' equal to the observed sample sizes.  Mean diet is estimated using the method
#' indicated by \code{mean_meth}.  The argument \code{mean_boot} controls the
#' number of times this is repeated, and the replications are used to estimate
#' the covariance matrix for each predator type. Unpublished work suggests
#' that the bootstrap estimator is more reliable. Note that if using the
#' parameterized-mean estimator for mean diet composition, the Beck estimator
#' is not appropriate.  The options for
#' \code{var_meth} are:
#' \itemize{
#' \item var_meth == 0 skips variance estimation for mean diets.
#' \item var_meth == 1 yields the bootstrap estimator.  This is the default
#'   value.
#' \item var_meth == 2 yields the Beck et al. (2007) estimator.
#' }
#'
#' NOTE: The numerical optimization and bootstrap sampling performed by
#' \code{est_diet} are numerically intensive and can cause long runs times.
#' Patience is advised!  The primary factors causing slow execution are the
#' number of predator signatures, the number of predator and prey types, and
#' bootstrap sample sizes.
#'
#' @section References:
#' Beck, C.A., S.J. Iverson, W.D. Bowen, and W. Blanchard. 2007. Sex differences
#'   in grey seal diet reflect seasonal variation in foraging behaviour and
#'   reproductive espenditure: evidence from quantitative fatty acid signature
#'   analysis. \emph{Journal of Animal Ecology} 76:490-502.
#'
#' Bromaghin, J.F., M.M. Lance, E.W. Elliott, S.J. Jeffries, A.
#'   Acevedo-Gutierrez, and J.M. Kennish. 2013. New insights into the diets of
#'   harbor seals (\emph{Phoca vitulina}) in the Salish Sea revealed by analysis
#'   of fatty acid signatures. \emph{Fishery Bulletin} 111:13-26.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#'   Quantitative fatty acid signature analysis: A new method of
#'   estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' Stewart, C., S. Iverson, and C. Field. 2014. Testing for a change in diet
#'   using fatty acid signatures. \emph{Environmental and Ecological Statistics}
#'   21:775-792.
#'
#' @examples
#' est_diet(pred_sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                               0.04, 0.11, 0.29, 0.56,
#'                               0.10, 0.06, 0.35, 0.49,
#'                               0.05, 0.15, 0.35, 0.45), ncol=4),
#'          pred_uniq_types = c("Pred_1", "Pred_2"),
#'          pred_loc = matrix(c(1, 3, 2, 4), ncol=2),
#'          prey_sigs = matrix(c(0.06, 0.09, 0.31, 0.54,
#'                               0.05, 0.09, 0.30, 0.56,
#'                               0.03, 0.10, 0.30, 0.57,
#'                               0.08, 0.07, 0.30, 0.55,
#'                               0.09, 0.05, 0.33, 0.53,
#'                               0.09, 0.06, 0.34, 0.51,
#'                               0.09, 0.07, 0.34, 0.50,
#'                               0.08, 0.11, 0.35, 0.46,
#'                               0.06, 0.14, 0.36, 0.44), ncol=9),
#'          prey_uniq_types = c("Prey_1", "Prey_2", "Prey_3"),
#'          prey_loc = matrix(c(1, 4, 7, 3, 6, 9), ncol=2),
#'          cc = c(0.75, 1.00, 1.50, 1.15),
#'          space = 1, dist_meas = 1, ind_boot = 2,
#'          mean_meth = 0)
#'
#' @export
#'
################################################################################

est_diet <- function(pred_sigs, pred_uniq_types, pred_loc,
                     prey_sigs, prey_uniq_types, prey_loc, cc,
                     space = 1, dist_meas = 1, gamma = 1, ind_boot = 100,
                     mean_meth = 1, var_meth = 1, mean_boot = 100){

  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.  The return objects pred_sig and prey_sig
  # are not included because they are input arguments and involved in the
  # following error checks.
  mean_sigs <- NA
  est_ind <- NA
  conv <- NA
  obj_func <- NA
  mod_sigs <- NA
  var_ind <- NA
  est_mean <- NA
  conv_mean <- NA
  var_mean <- NA



  # Check that pred_sigs is a numeric matrix.
  if(!(is.numeric(pred_sigs) & is.matrix(pred_sigs))){
    err_code <- 1
    err_message <- "The argument pred_sigs is not a numeric matrix!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that pred_sigs are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(pred_sigs) < 0) | is.na(min(pred_sigs))){
    err_code <- 2
    err_message <- "One or more predator signatures are invalid!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check lengths of pred_uniq_types and pred_loc.
  if(length(pred_uniq_types) != nrow(pred_loc)){
    err_code <- 3
    err_message <- paste("The length of pred_uniq_types does not equal",
                         "the number of rows in pred_loc!",
                         sep=" ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that prey_sigs is a numeric matrix.
  if(!(is.numeric(prey_sigs) & is.matrix(prey_sigs))){
    err_code <- 4
    err_message <- "The argument prey_sigs is not a numeric matrix!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that prey_sigs are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(prey_sigs) < 0) | is.na(min(prey_sigs))){
    err_code <- 5
    err_message <- "One or more prey signatures are invalid!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check lengths of prey_uniq_types and prey_loc.
  if(length(prey_uniq_types) != nrow(prey_loc)){
    err_code <- 6
    err_message <- paste("The length of prey_uniq_types does not equal",
                         "the number of rows in prey_loc!",
                         sep=" ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that cc is a numeric vector.
  if(!(is.vector(cc) & is.numeric(cc))){
    err_code <- 7
    err_message <- "The argument cc is not a numeric vector!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }



  # Check that number of fatty acids in pred_sigs, prey_sigs, and cc are equal.
  if((nrow(pred_sigs) != nrow(prey_sigs)) | (nrow(pred_sigs) != length(cc))){
    err_code <- 8
    err_message <- paste("The number of fatty acids in pred_sigs, prey_sigs,",
                         "and cc are not all equal!",
                         sep = " ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the the values in cc are all greater than 0.
  if(min(cc) <= 0 | is.na(min(cc))){
    err_code <- 9
    err_message <- "All calibration coefficients must exceed 0!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of space.
  if(space != 1 & space != 2){
    err_code <- 10
    err_message <- "The argument space must equal 1 or 2!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of dist_meas.
  if(!(dist_meas %in% 1:3)){
    err_code <- 11
    err_message <- "The argument dist_meas must equal 1, 2, or 3!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check combinations of dist_meas, gamma, and the minimum signature
  # proportion.
  dum_1 <- min(pred_sigs)
  dum_2 <- min(prey_sigs)
  if(dist_meas < 3){

    # Check value of the minimum signature proportion.
    if(dum_1 <= 0 | dum_2 <= 0){
      err_code <- 12
      err_message <- paste("If dist_meas equals 1 or 2,",
                           "signature proportions must exceed 0!",
                           sep=" ")

      return(list(pred_sigs = NA,
                  prey_sigs = NA,
                  mean_sigs = mean_sigs,
                  est_ind = est_ind,
                  conv = conv,
                  obj_func = obj_func,
                  mod_sigs = mod_sigs,
                  var_ind = var_ind,
                  est_mean = est_mean,
                  conv_mean = conv_mean,
                  var_mean = var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }
  } else{

    # Check value of the minimum signature proportion.
    if(dum_1 < 0 | dum_2 < 0){
      err_code <- 13
      err_message <- "Signature proportions cannot be negative!"

      return(list(pred_sigs = NA,
                  prey_sigs = NA,
                  mean_sigs = mean_sigs,
                  est_ind = est_ind,
                  conv = conv,
                  obj_func = obj_func,
                  mod_sigs = mod_sigs,
                  var_ind = var_ind,
                  est_mean = est_mean,
                  conv_mean = conv_mean,
                  var_mean = var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }

    # Check value of gamma.
    if(is.na(gamma) | gamma <= 0 | gamma > 1){
      err_code <- 14
      err_message <- paste("If dist_meas equals 3, gamma must exceed 0 and",
                           "cannot exceed 1!",
                           sep=" ")

      return(list(pred_sigs = NA,
                  prey_sigs = NA,
                  mean_sigs = mean_sigs,
                  est_ind = est_ind,
                  conv = conv,
                  obj_func = obj_func,
                  mod_sigs = mod_sigs,
                  var_ind = var_ind,
                  est_mean = est_mean,
                  conv_mean = conv_mean,
                  var_mean = var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }
  }


  # Check the value of mean_meth.
  if(!(mean_meth %in% 0:2)){
    err_code <- 15
    err_message <- "The argument mean_meth must equal 0, 1, or 2!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check the value of var_meth.
  if(mean_meth > 0 & !(var_meth %in% 0:2)){
    err_code <- 16
    err_message <- "The argument var_meth must equal 0, 1 or 2!"

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }



  # Check bootstrap sample size for individual variance estimation.
  # Note that ind_boot == 0 is valid and skips variance estimation for
  # individual diets.
  if(is.na(ind_boot) | ind_boot == 1 | ind_boot < 0){
    err_code <- 17
    err_message <- paste("The number of bootstrap replications for individual",
                         "diet estimates must be an integer equal to 0 or",
                        "greater than 1!",
                         sep = " ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check bootstrap sample size for mean variance estimation.
  if(mean_meth > 0 & var_meth == 1 & (is.na(mean_boot) | mean_boot < 2)){
    err_code <- 18
    err_message <- paste("The number of bootstrap replications for mean diet",
                         "estimates must be at least 2!",
                         sep = " ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check correspondence between individual and mean variance methods.
  if(ind_boot == 0 & var_meth == 2){
    err_code <- 19
    err_message <- paste("The Beck estimator of mean diet variance requires",
                         "individual bootstrap variances!",
                         sep = " ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Check correspondence estimators for the mean diet.
  if(mean_meth == 2 & var_meth == 2){
    err_code <- 20
    err_message <- paste("Must use the bootstrap variance estimator with the",
                         "parameterized-mean estimator!",
                         sep = " ")

    return(list(pred_sigs = NA,
                prey_sigs = NA,
                mean_sigs = mean_sigs,
                est_ind = est_ind,
                conv = conv,
                obj_func = obj_func,
                mod_sigs = mod_sigs,
                var_ind = var_ind,
                est_mean = est_mean,
                conv_mean = conv_mean,
                var_mean = var_mean,
                err_code = err_code,
                err_message = err_message))
  }



  # Transform signatures to the specified space --------------------------------
  if(space == 1){

    # Transform prey signatures to the predator space.
    cc_mat <- matrix(data = cc, nrow = length(cc), ncol = ncol(prey_sigs))
    prey_sigs <- prey_sigs*cc_mat

    prod_mat <- matrix(data = colSums(prey_sigs), nrow = length(cc),
                       ncol = ncol(prey_sigs), byrow=TRUE)
    prey_sigs <- prey_sigs/prod_mat
  } else{

    # Transform predator signatures to the prey space.
    cc_mat <- matrix(data = cc, nrow = length(cc), ncol = ncol(pred_sigs))
    pred_sigs <- pred_sigs/cc_mat

    prod_mat <- matrix(data = colSums(pred_sigs), nrow = length(cc),
                       ncol = ncol(pred_sigs), byrow=TRUE)
    pred_sigs <- pred_sigs/prod_mat
  }
  rm(cc_mat, prod_mat)



  # Prepare for diet estimation ------------------------------------------------

  # Define function constraining diet proportions to sum to 1.
  # Note: the optimizer Rsolnp::solnp() requires the variables passed to the
  # objective and constraint functions to be identical.
  sum_constr <- function(diet, obs_sig, mean_sigs, dist_meas=dist_meas, gamma=gamma)
  {
    return(sum(diet))
  }


  # Cast bootstrap sample sizes as integers.
  if(!is.na(ind_boot)){
    ind_boot <- as.integer(ind_boot)
  }
  if(!is.na(mean_boot)){
    mean_boot <- as.integer(mean_boot)
  }


  # Define needed constants.
  n_fa <- length(cc)
  n_pred <- ncol(pred_sigs)
  n_pred_types <- length(pred_uniq_types)
  n_prey_types <- length(prey_uniq_types)


  # Compute predator and prey sample sizes.
  pred_ss <- pred_loc[,2] - pred_loc[,1] + 1
  prey_ss <- prey_loc[,2] - prey_loc[,1] + 1


  # Allocate memory for results.
  est_ind <- matrix(data = 0, nrow = n_prey_types, ncol = n_pred)
  colnames(est_ind) <- colnames(pred_sigs)
  rownames(est_ind) <- prey_uniq_types

  conv <- vector(mode = "logical", length = n_pred)
  names(conv) <- colnames(pred_sigs)

  obj_func <- vector(mode = "numeric", length = n_pred)
  names(obj_func) <- colnames(pred_sigs)

  mod_sigs <- matrix(data = 0, nrow = n_fa, ncol = n_pred)
  rownames(mod_sigs) <- rownames(pred_sigs)
  colnames(mod_sigs) <- colnames(pred_sigs)

  if(ind_boot > 1){

    # Will estimate variance of individual estimates via bootstrapping.
    ind_boot_est <- matrix(data = 0, nrow = n_prey_types, ncol = ind_boot)
    var_ind <- array(data = 0, dim = c(n_prey_types, n_prey_types, n_pred),
                     dimnames = list(prey_uniq_types, prey_uniq_types,
                                     colnames(pred_sigs)))
  }

  if(ind_boot > 1 | var_meth == 1) {

    # Estimate the variance of mean estimates via bootstrapping.
    mean_sigs_boot <- matrix(data = 0, nrow = n_fa, ncol = n_prey_types)
  }



  # Compute mean prey signatures.
  mean_sigs <- matrix(data = 0, nrow = n_fa, ncol = n_prey_types)
  for(li1 in 1:n_prey_types){
    mean_sigs[,li1] <- apply(X = as.matrix(prey_sigs[,prey_loc[li1,1]:
                                                      prey_loc[li1,2]]),
                             MARGIN = 1, FUN = mean)
  }
  colnames(mean_sigs) <- prey_uniq_types
  rownames(mean_sigs) <- rownames(prey_sigs)


  # Initialize optimization variables.
  guess <- rep(1/n_prey_types, n_prey_types)
  low_bound <- rep(0, n_prey_types)
  up_bound <- rep(1, n_prey_types)



  # Estimate individual diets --------------------------------------------------
  for(li1 in 1:n_pred){

    # Estimate this predator's diet.
    this_est <- Rsolnp::solnp(pars = guess, fun = diet_obj_func,
                              eqfun = sum_constr, eqB = 1, LB = low_bound,
                              UB = up_bound, obs_sig = pred_sigs[,li1],
                              mean_sigs = mean_sigs, dist_meas = dist_meas,
                              gamma = gamma, control = list(trace=0))


    # Store estimate.
    est_ind[,li1] <- this_est$pars
    conv[li1] <- (this_est$convergence == 0)
    obj_func[li1] <- this_est$values[length(this_est$values)]


    # Model the signature of this predator.
    diet_mat <- matrix(data = this_est$pars, nrow = n_fa, ncol = n_prey_types,
                       byrow = TRUE)
    mod_sigs[,li1] <- apply(X=diet_mat*mean_sigs, MARGIN = 1, FUN = sum)


    ## Bootstrap estimates for individual variance estimation.
    if(ind_boot > 1){

      n_conv <- 0
      keep_going <- TRUE
      while(keep_going){

        # Bootstrap prey and compute mean prey signatures.
        for(li2 in 1:n_prey_types){
          this_sample <- sample(x = prey_loc[li2,1]:prey_loc[li2,2],
                                size = prey_ss[li2], replace = TRUE)
          mean_sigs_boot[,li2] <- apply(X = as.matrix(prey_sigs[,this_sample]),
                                        MARGIN = 1,
                                        FUN = mean)
        }


        # Estimate diet with the bootstrapped prey library.
        this_est <- Rsolnp::solnp(pars = guess, fun = diet_obj_func,
                                  eqfun = sum_constr, eqB = 1, LB = low_bound,
                                  UB = up_bound, obs_sig = pred_sigs[,li1],
                                  mean_sigs = mean_sigs_boot,
                                  dist_meas = dist_meas,
                                  gamma = gamma, control = list(trace=0))
        if(this_est$convergence == 0){
          n_conv <- n_conv + 1
          ind_boot_est[,n_conv] <- this_est$pars

          if(n_conv >= ind_boot){
            keep_going <- FALSE
          }
        }
      } # end while


      # Compute the covariance matrix for this predator.
      for(li2 in 1:(n_prey_types-1)){
        var_ind[li2,li2,li1] <- stats::var(ind_boot_est[li2,])
        for(li3 in (li2+1):n_prey_types){
          var_ind[li2,li3,li1] <- stats::cov(ind_boot_est[li2,],
                                             ind_boot_est[li3,])
          var_ind[li3,li2,li1] <- var_ind[li2,li3,li1]
        }
      }
      var_ind[n_prey_types,n_prey_types,li1] <-
                                    stats::var(ind_boot_est[n_prey_types,])
    } # end variance estimation if
  } # end li1 loop



  # Estimate mean diet ---------------------------------------------------------
  if(mean_meth > 0){

    # Allocate memory.
    est_mean <- matrix(data = 0, nrow = n_prey_types,
                       ncol = n_pred_types)
    colnames(est_mean) <- pred_uniq_types
    rownames(est_mean) <- prey_uniq_types

    conv_mean <- vector(mode = "logical", length = n_pred_types)


    if(mean_meth == 1){

      ## Empirical mean.

      # Consider each predator type.
      for(li1 in 1:n_pred_types){

        # Exclude individual estimates that did not converge.
        pred_list <- pred_loc[li1,1]:pred_loc[li1,2]
        pred_list <- pred_list[conv[pred_loc[li1,1]:pred_loc[li1,2]]]
        good_est <- est_ind[,pred_list]

        # Compute the mean diet estimate.
        if(length(pred_list) > 0){
          conv_mean[li1] <- TRUE
          est_mean[,li1] <- apply(X = est_ind[,pred_list], MARGIN = 1,
                                FUN = mean)
        } else{
          conv_mean[li1] <- FALSE
          est_mean[,li1] <- apply(X = est_ind[,pred_loc[li1,1]:pred_loc[li1,2]],
                                  MARGIN = 1, FUN = mean)
        }
      }
    } else if(mean_meth ==2){

      ## Parameterized mean.

      # Consider each predator type.
      for(li1 in 1:n_pred_types){

        # Isolate signatures for this class.
        temp_sigs <- pred_sigs[,pred_loc[li1,1]:pred_loc[li1,2]]
        this_est <- Rsolnp::solnp(pars = guess, fun = pm_obj_func,
                                  eqfun = sum_constr, eqB = 1, LB = low_bound,
                                  UB = up_bound, obs_sig = temp_sigs,
                                  mean_sigs = mean_sigs, dist_meas = dist_meas,
                                  gamma = gamma, control = list(trace=0))
        est_mean[,li1] <- this_est$pars
        conv_mean[li1] <- (this_est$convergence == 0)
      }
    } # end if
  }



  # Estimate the variance of mean diet -----------------------------------------
  if(mean_meth > 0 & var_meth == 1){

    ## Bootstrap estimate.

    # Allocate memory.
    var_mean <- array(data = 0,
                      dim = c(n_prey_types, n_prey_types, n_pred_types),
                      dimnames = list(prey_uniq_types, prey_uniq_types,
                                      pred_uniq_types))
    mean_boot_est <- matrix(data = 0, nrow = n_prey_types, ncol = mean_boot)


    # Consider each predator type.
    for(li1 in 1:n_pred_types){

      mean_boot_est[1:n_prey_types, 1:mean_boot] <- 0

      # Bootstrap loop.
      for(li2 in 1:mean_boot){

        # Bootstrap prey and compute mean prey signatures.
        for(li3 in 1:n_prey_types){
          this_sample <- sample(x = prey_loc[li3,1]:prey_loc[li3,2],
                                size = prey_ss[li3], replace = TRUE)
          mean_sigs_boot[,li3] <- apply(X = as.matrix(prey_sigs[,this_sample]),
                                        MARGIN = 1,
                                        FUN = mean)
        } # end li3


        # Bootstrap predators.
        this_sample <- sample(x = pred_loc[li1,1]:pred_loc[li1,2],
                              size = pred_ss[li1], replace = TRUE)
        boot_pred <- pred_sigs[,this_sample]


        # Estimate mean diet with these bootstrap samples.
        if(mean_meth == 1){

          ## Estimate the empirical mean.
          n_conv <- 0

          for(li3 in 1:pred_ss[li1]){

            # Estimate diet.
            this_est <- Rsolnp::solnp(pars = guess,
                                      fun = diet_obj_func,
                                      eqfun = sum_constr, eqB = 1,
                                      LB = low_bound, UB = up_bound,
                                      obs_sig = boot_pred[,li3],
                                      mean_sigs = mean_sigs_boot,
                                      dist_meas = dist_meas,
                                      gamma = gamma, control = list(trace=0))
            if(this_est$convergence == 0){
              n_conv <- n_conv + 1
              mean_boot_est[,li2] <- mean_boot_est[,li2] + this_est$pars
            }
          }

          if(n_conv > 1){
            mean_boot_est[,li2] <- mean_boot_est[,li2]/n_conv
          }
        } else{

          this_est <- Rsolnp::solnp(pars = guess, fun = pm_obj_func,
                                    eqfun = sum_constr, eqB = 1, LB = low_bound,
                                    UB = up_bound, obs_sig = boot_pred,
                                    mean_sigs = mean_sigs, dist_meas = dist_meas,
                                    gamma = gamma, control = list(trace=0))
          if(this_est$convergence == 0){
            mean_boot_est[,li2] <- this_est$pars
          }
        }
      } # end li2


      # Isolate bootstrap estimates that converged.
      use_est <- (colSums(mean_boot_est) > 0)
      mean_boot_est <- mean_boot_est[,use_est]


      # Compute the covariance matrix for this predator type.
      for(li2 in 1:(n_prey_types-1)){
        var_mean[li2,li2,li1] <- stats::var(mean_boot_est[li2,])
        for(li3 in (li2+1):n_prey_types){
          var_mean[li2,li3,li1] <- stats::cov(mean_boot_est[li2,],
                                              mean_boot_est[li3,])
          var_mean[li3,li2,li1] <- var_mean[li2,li3,li1]
        }
      }
      var_mean[n_prey_types,n_prey_types,li1] <-
                                   stats::var(mean_boot_est[n_prey_types,])
    } # end li1
  } else if(mean_meth > 0 & var_meth == 2){

    ## Beck et al. (2007) estimate.

    # Allocate memory.
    var_mean <- array(data = 0,
                      dim = c(n_prey_types, n_prey_types, n_pred_types),
                      dimnames = list(prey_uniq_types, prey_uniq_types,
                                      pred_uniq_types))


    # Consider each predator type.
    for(li1 in 1:n_pred_types){

      # Compute the mean individual variance matrix.
      mean_var_ind <- apply(X = var_ind[,,pred_loc[li1,1]:pred_loc[li1,2]],
                            MARGIN = c(1,2), FUN = mean)


      # Compute Beck's estimate.
      for(li2 in 1:(n_prey_types-1)){
        var_mean[li2,li2,li1] <-
               (stats::var(est_ind[li2, pred_loc[li1,1]:pred_loc[li1,2]]) +
                mean_var_ind[li2,li2])/pred_ss[li1]
        for(li3 in (li2+1):n_prey_types){
          var_mean[li2,li3,li1] <-
               (stats::cov(est_ind[li2,pred_loc[li1,1]:pred_loc[li1,2]],
                est_ind[li3,pred_loc[li1,1]:pred_loc[li1,2]]) +
                mean_var_ind[li2,li3])/pred_ss[li1]
          var_mean[li3,li2,li1] <- var_mean[li2,li3,li1]
        }
      }
      var_mean[n_prey_types,n_prey_types,li1] <-
          (stats::var(est_ind[n_prey_types,pred_loc[li1,1]:pred_loc[li1,2]]) +
           mean_var_ind[n_prey_types,n_prey_types])/pred_ss[li1]
    }
  } # end if var_meth



  # Return ---------------------------------------------------------------------
  err_code <- 0
  err_message <- "Success!"

  return(list(pred_sigs = pred_sigs,
              prey_sigs = prey_sigs,
              mean_sigs = mean_sigs,
              est_ind = est_ind,
              conv = conv,
              obj_func = obj_func,
              mod_sigs = mod_sigs,
              var_ind = var_ind,
              est_mean = est_mean,
              conv_mean = conv_mean,
              var_mean = var_mean,
              err_code = err_code,
              err_message = err_message))
}


