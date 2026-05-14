#' Goodness-of-fit for modeled predator signatures
#'
#' The function \code{gof} uses estimated diet compositions and bootstrap
#' resampling of the prey library to construct a statistic that may
#' conservatively indicate predator fatty acid signatures that were not
#' accurately modeled during diet estimation.
#'
#' @param prey_sigs A matrix of prey signatures in the optimization space used
#'   for diet estimation. Intended to be the object \code{prey_sigs} returned by
#'   the function \code{\link{est_diet}}.
#' @param prey_loc A matrix giving the first and last locations of the
#'   signatures of each prey type within \code{prey_sigs}. Intended to be the
#'   object \code{loc} returned by the function \code{\link{prep_sig}} if diets
#'   were estimated using an unpartitioned prey library or
#'   \code{\link{make_prey_part}} if diets were estimated using a partitioned
#'   library.
#' @param mean_sigs A numeric matrix of mean prey-type signatures in the
#'   optimization space used for diet estimation. Intended to be the object
#'   \code{mean_sigs} returned by the function \code{\link{est_diet}}.
#' @param diet_est A numeric matrix of estimated diet compositions. Intended to
#'   be the object \code{est_ind} returned by the function
#'   \code{\link{est_diet}}.
#' @param conv A logical vector indicating whether the optimization function
#'   successfully converged during diet estimation. Intended to be the object
#'   \code{conv} returned by the function \code{\link{est_diet}}.
#' @param obj_func A numeric vector of the value of the minimized objective
#'   function for each predator. Intended to be the object \code{obj_func}
#'   returned by the function \code{\link{est_diet}}.
#' @param dist_meas An integer indicator of the distance measure used for diet
#'   estimation. Default value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#' @param boot_gof The number of bootstrap replications to use. Default value 500.
#'
#' @return A list containing the following elements: \describe{
#'   \item{gof_ss}{The number of diet estimates that converged for each
#'     predator, therefore producing a simulated value of the objective
#'     function.}
#'   \item{p_val}{The proportion of the simulated objective function values that
#'     exceeded the value produced during diet estimation.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#' }
#'
#' @section Details:
#' Diet estimation involves modeling an observed predator fatty acid signature
#' as a mixture of prey signatures. However, methods to assess how well predator
#' signatures are modeled have received little attention in the literature
#' (but see Bromaghin et al. 2015).
#'
#' One byproduct of diet estimation is the value of the distance measure that is
#' minimized during diet estimation (\code{\link{est_diet}}), called the
#' objective function. If a predator signature is accurately modeled, the
#' value of the objective function will be relatively small. Conversely, the
#' more poorly the signature is approximated, the larger the objective function
#' will tend to be. However, what value of the objective function to use as a
#' warning flag for a potentially poor fit is not clear.
#'
#' The function \code{gof} represents one attempt to answer this question. The
#' algorithm is based on the following logic. First, we assume that a predator
#' consumes the mixture of prey specified by its estimated diet composition.
#' Given that assumption, the expected value of the objective function is, in
#' a sense, fixed (Bromaghin 2015). Large values of the objective function are
#' then most likely to occur when variation in a predator signature, which
#' results from the selection of individual prey within prey types, is
#' maximized. Within the framework of simulating predator signatures, variation
#' in the signatures is maximized when the bootstrap sample sizes of the prey
#' signatures used to construct a predator signature are minimized
#' (Bromaghin et al. 2016).
#'
#' Implementing the above logic, \code{gof} randomly samples a single prey
#' signature from each prey type and weights the resulting signatures with a
#' predator's estimated diet composition to construct a modeled signature. The
#' modeled signature is then used to estimate diet. If the optimization function
#' converges, the value of the objective function obtained with the modeled
#' signature is compared to the value of the objective function obtained while
#' estimating diet with the observed signature (argument \code{obj_func}. This
#' is repeated \code{boot_gof} times and the proportion of the simulated
#' objective function values that exceed the observed objective function value
#' is computed. \code{gof} therefore constructs a statistic similar to a
#' p-value, with small values being suggestive of a predator signature that
#' was not closely approximated during diet estimation.
#'
#' NOTE: the method implemented in \code{gof} is at this point only an idea
#' whose performance has not been explored. It has been included in
#' \code{qfasar} to support future research on this topic.
#'
#' @section References:
#' Bromaghin, J.F. 2015. Simulating realistic predator signatures in
#'   quantitative fatty acid signature analysis. Ecological Informatics
#'   30:68-71.
#'
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty acid
#'  signature proportions sum to 1 for diet estimation? Ecological Research
#'  31:597-606.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#'
#' @examples
#' gof(prey_sigs = matrix(c(0.06, 0.09, 0.31, 0.54,
#'                          0.05, 0.09, 0.30, 0.56,
#'                          0.03, 0.10, 0.30, 0.57,
#'                          0.08, 0.07, 0.30, 0.55,
#'                          0.09, 0.05, 0.33, 0.53,
#'                          0.09, 0.06, 0.34, 0.51,
#'                          0.09, 0.07, 0.34, 0.50,
#'                          0.08, 0.11, 0.35, 0.46,
#'                          0.06, 0.14, 0.36, 0.44), ncol = 9),
#'     prey_loc = matrix(c(1, 4, 7, 3, 6, 9), ncol=2),
#'     mean_sigs = matrix(c(0.047, 0.093, 0.303, 0.557,
#'                          0.087, 0.050, 0.323, 0.530,
#'                          0.077, 0.106, 0.350, 0.467), ncol = 3),
#'     diet_est = matrix(c(0.394, 0.356, 0.250,
#'                         0.336, 0.365, 0.299), ncol = 2),
#'     conv = c(TRUE, TRUE),
#'     obj_func = c(1.13, 2.24),
#'     dist_meas = 1,
#'     boot_gof = 10)
#'
#'
#' @export
#'
################################################################################

gof <- function(prey_sigs, prey_loc, mean_sigs, diet_est, conv, obj_func,
                dist_meas = 1, gamma = 1, boot_gof = 500){


  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.  The return objects pred_sig and prey_sig
  # are not included because they are input arguments and involved in the
  # following error checks.
  gof_ss <- NA
  p_val <- NA


  # Check that prey_sigs is a numeric matrix.
  if(!(is.numeric(prey_sigs) & is.matrix(prey_sigs))){
    err_code <- 1
    err_message <- "The argument prey_sigs is not a numeric matrix!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that prey_sigs are non-negative.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(prey_sigs) < 0) | is.na(min(prey_sigs))){
    err_code <- 2
    err_message <- "One or more prey signatures are invalid!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that the numbers of individuals in prey_sigs and prey_loc are
  # consistent.
  if(ncol(prey_sigs) != prey_loc[nrow(prey_loc),2]){
    err_code <- 3
    err_message <- "Number of prey in prey_sigs and prey_loc differ!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that mean_sigs is a numeric matrix.
  if(!(is.numeric(mean_sigs) & is.matrix(mean_sigs))){
    err_code <- 4
    err_message <- "The argument mean_sigs is not a numeric matrix!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that mean_sigs are non-negative.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(mean_sigs) < 0) | is.na(min(mean_sigs))){
    err_code <- 5
    err_message <- "One or more mean prey signatures are invalid!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that the numbers of prey types mean_sigs and prey_loc are
  # consistent.
  if(nrow(prey_loc) != ncol(mean_sigs)){
    err_code <- 6
    err_message <- "Number of prey types in mean_sigs and prey_loc differ!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }



    # Check that diet_est is a numeric matrix.
  if(!(is.numeric(diet_est) & is.matrix(diet_est))){
    err_code <- 7
    err_message <- "The argument diet_est is not a numeric matrix!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that diet_est are non-negative and their sums do not exceed 1.
  if((min(diet_est) < 0) | is.na(min(diet_est))){
    err_code <- 8
    err_message <- "One or more diet estimates are invalid!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check that number of prey types in diet_est and mean_sigs are equal.
  if(nrow(diet_est) != ncol(mean_sigs)){
    err_code <- 9
    err_message <- "Number of prey types in diet_est and mean_sigs differ!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check conv.
  if(!(is.logical(conv) & is.vector(conv))){
    err_code <- 10
    err_message <- "The argument conv must be a logical vector!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check obj_func.
  if(!(is.numeric(obj_func) & is.vector(obj_func))){
    err_code <- 11
    err_message <- "The argument conv must be a logical vector!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Check the value of dist_meas.
  if(!(dist_meas %in% 1:3)){
    err_code <- 12
    err_message <- "The argument dist_meas must equal 1, 2, or 3!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check length of predator inputs.
  if(length(conv) != length(obj_func)){
    err_code <- 13
    err_message <- "Number of predators differs in conv and obj_func!"

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check value of gamma.
  if((dist_meas == 3) & (is.na(gamma) | gamma <= 0 | gamma > 1)){
    err_code <- 14
    err_message <- paste("If dist_meas equals 3, gamma must exceed 0 and",
                         "cannot exceed 1!",
                         sep=" ")

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }


  # Check bootstrap value.
  if(boot_gof < 2){
    err_code <- 15
    err_message <- paste("The number of bootstrap replications must be an",
                         "integer greater than exceed 1!",
                         sep = " ")

    return(list(gof_ss = gof_ss,
                p_val = p_val,
                err_code <- err_code,
                err_message <- err_message))
  }



  # Prepare for analysis -------------------------------------------------------

  # Compute constants.
  n_pred <- ncol(diet_est)
  n_prey_types <- nrow(prey_loc)
  n_fa <- nrow(prey_sigs)


  # Determine estimates that converged.
  conv_list <- 1:n_pred
  conv_list <- conv_list[conv == TRUE]
  n_conv <- length(conv_list)


  # Allocate memory.
  prey_samp <- matrix(data = 0, nrow = nrow(prey_sigs), ncol = n_prey_types)

  gof_ss <- vector(mode = "integer", length = length(conv))
  names(gof_ss) <- colnames(diet_est)

  p_val <- vector(mode = "numeric", length = length(conv))
  names(p_val) <- colnames(diet_est)


  # Define function constraining diet proportions to sum to 1.
  # Note: the optimizer Rsolnp::solnp() requires the variables passed to the
  # objective and constraint functions to be identical.
  sum_constr <- function(diet, obs_sig, mean_sigs, dist_meas=dist_meas,
                         gamma=gamma){
    return(sum(diet))
  }


  # Initialize optimization variables.
  guess <- rep(1/n_prey_types, n_prey_types)
  low_bound <- rep(0, n_prey_types)
  up_bound <- rep(1, n_prey_types)



  # Start bootstrap loop -------------------------------------------------------
  for(li1 in 1:boot_gof){

    # Sample one of each prey type
    for(li2 in 1:n_prey_types){
      this_sig <- sample(x = prey_loc[li2,1]:prey_loc[li2,2], size = 1)
      prey_samp[,li2] <- prey_sigs[,this_sig]
    }


    # Consider each predator.
    for(li2 in 1:n_conv){

      # Model this predator signature.
      diet_mat <- matrix(diet_est[,conv_list[li2]], nrow = n_fa,
                         ncol = n_prey_types, byrow = TRUE)
      mod_sig <- apply(X = diet_mat*prey_samp, MARGIN = 1, FUN = sum)


      # Estimate this predator's diet.
      this_pred <- conv_list[li2]
      this_est <- Rsolnp::solnp(pars = guess, fun = diet_obj_func,
                                eqfun = sum_constr, eqB = 1, LB = low_bound,
                                UB = up_bound, obs_sig = mod_sig,
                                mean_sigs = mean_sigs, dist_meas = dist_meas,
                                gamma = gamma, control = list(trace=0))

      # Process results.
      if(this_est$convergence == 0){
        gof_ss[this_pred] <- gof_ss[this_pred] + 1
        if(this_est$values[length(this_est$values)] > obj_func[li2]){
          p_val[this_pred] <- p_val[this_pred] + 1
        }
      }
    }
  }




  # Clean up and return --------------------------------------------------------
  have_est <- (conv == TRUE & gof_ss > 0)
  p_val[have_est] <- p_val[have_est]/gof_ss[have_est]

  have_est <- !have_est
  gof_ss[have_est] <- NA
  p_val[have_est] <- NA


  err_code <- 0
  err_message <- "Success!"

  return(list(gof_ss = gof_ss,
              p_val = p_val,
              err_code <- err_code,
              err_message <- err_message))
}
