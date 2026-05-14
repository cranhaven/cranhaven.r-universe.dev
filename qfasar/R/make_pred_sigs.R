#' Simulate predator signatures
#'
#' \code{make_pred_sigs} generates predator signatures based on a specified
#'  predator diet composition and bootstrap sampling signatures from a prey
#'  library.
#'
#' @param prey_sigs A matrix of prey signatures in the prey space, intended
#'   to be the object \code{sig_scale} returned by a call to the function
#'   \code{\link{prep_sig}}, or the object \code{sig_part} returned by a call to
#'   the function \code{\link{make_prey_part}}.
#' @param prey_loc A matrix giving the first and last locations of the
#'   signatures of each prey type within \code{prey_sigs}, intended to be the
#'   object \code{loc} returned by a call to one of the functions
#'   \code{\link{prep_sig}} or \code{\link{make_prey_part}}.
#' @param cc A numeric vector containing the calibration coefficients.
#' @param diet A numeric vector specifying the predator diet composition as
#'   proportions.
#' @param prey_ss An integer vector specifying the bootstrap sample size to use
#'   for each prey type.
#' @param n_pred An integer specifying the number of predator signatures to
#'   generate.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{sim_pred_sigs}{A numeric matrix containing simulated predator
#'   signatures in the predator space.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string containing a brief summary of the results.}
#' }
#'
#' @section Details:
#' QFASA simulation studies often require the generation of predator signatures
#' given a specified diet, against which subsequent estimates of diet
#' composition can then be compared (e.g., Bromaghin et al. 2016). Given a
#' specified diet, a bootstrap sample of each prey type is drawn and mean
#' prey-type signatures are computed.  A predator signature is then generated
#' by multiplying the mean bootstrapped prey signatures by the diet proportions.
#' Finally, the calibration coefficients are then used to transform the predator
#' signatures to the predator space (Bromaghin et al. 2015).
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty acid
#'   signature proportions sum to 1 for diet estimation? \emph{Ecological
#'   Research} 31:597-606.
#'
#' Bromaghin, J.F., K.D. Rode, S.M. Budge, and G.W. Thiemann. 2015. Distance
#'   measures and optimization spaces in quantitative fatty acid signature
#'   analysis. \emph{Ecology and Evolution} 5:1249-1262.
#'
#' @examples
#' make_pred_sigs(prey_sigs = matrix(c(0.06, 0.09, 0.31, 0.54,
#'                                   0.05, 0.09, 0.30, 0.56,
#'                                   0.03, 0.10, 0.30, 0.57,
#'                                   0.08, 0.07, 0.30, 0.55,
#'                                   0.09, 0.05, 0.33, 0.53,
#'                                   0.09, 0.06, 0.34, 0.51,
#'                                   0.09, 0.07, 0.34, 0.50,
#'                                   0.08, 0.11, 0.35, 0.46,
#'                                   0.06, 0.14, 0.36, 0.44), ncol = 9),
#'                prey_loc = matrix(c(1, 4, 7, 3, 6, 9), ncol=2),
#'                cc = c(0.75, 1.05, 0.55, 1.75),
#'                diet = c(0.25, 0.25, 0.50),
#'                prey_ss = c(5, 3, 7),
#'                n_pred = 50)
#'
#' @export
#'
################################################################################

make_pred_sigs <- function(prey_sigs, prey_loc, cc, diet, prey_ss, n_pred){


  # Check inputs ---------------------------------------------------------------

  # Initialize objects to be returned.
  sim_pred_sigs <- NA


  # Check that numbers of prey match.
  if(ncol(prey_sigs) != prey_loc[length(prey_loc)]){
    err_code <- 1
    err_message <- "The number of prey in prey_sigs and prey_loc differ!"

    return(list(sim_pred_sigs = sim_pred_sigs,
                err_code = err_code,
                err_message = err_message))
  }


  # Check numbers of prey types.
  if((nrow(prey_loc) != length(diet)) |
     (length(diet) != length(prey_ss))){
    err_code <- 2
    err_message <- paste("The number of prey types in prey_loc, diet, and/or",
                         "prey_ss differ!",
                         sep = " ")

    return(list(sim_pred_sigs = sim_pred_sigs,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that numbers of fatty acids match.
  if(nrow(prey_sigs) != length(cc)){
    err_code <- 3
    err_message <- "The number of fatty acids in prey_sigs and cc differ!"

    return(list(sim_pred_sigs = sim_pred_sigs,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that number of predators.
  if(n_pred < 1){
    err_code <- 4
    err_message <- paste("The number of predator signatures to generate must",
                         "be at least 1!",
                         sep = " ")

    return(list(sim_pred_sigs = sim_pred_sigs,
                err_code = err_code,
                err_message = err_message))
  }



  # Initialize variables -------------------------------------------------------

  # Number of fatty acids and prey types.
  n_fa <- nrow(prey_sigs)
  n_prey_types <- nrow(prey_loc)


  # Diet matrix.
  diet_mat <- matrix(data = diet, nrow = n_fa, ncol = n_prey_types,
                     byrow = TRUE)


  # cc matrix.
  cc_mat <- matrix(data = cc, nrow = n_fa, ncol = n_pred)


  # Allocate memory.
  sim_pred_sigs <- matrix(data = 0, nrow = n_fa, ncol = n_pred)
  prey_means <- matrix(data = 0, nrow = n_fa, ncol = n_prey_types)



  # Simulate predator signatures -----------------------------------------------


  # Consider each predator.
  for(li1 in 1:n_pred){

    # Consider each prey type.
    for(li2 in 1:n_prey_types){

      if(prey_ss[li2] > 0){

        # Sample prey.
        rand_prey <- sample(x = prey_loc[li2,1]:prey_loc[li2,2],
                            size = prey_ss[li2], replace=TRUE)

        # Compute mean prey signature.
        if(prey_ss[li2] == 1){
          prey_means[,li2] <- prey_sigs[,rand_prey]
        } else {
          prey_means[,li2] <-  apply(X = prey_sigs[,rand_prey], MARGIN = 1,
                                     FUN = mean)
        }
      }
    }


    # Compute predator signature.
    sim_pred_sigs[,li1] <- apply(X = diet_mat*prey_means, MARGIN = 1, FUN = sum)

  }



  # Transform signatures to the predator space ---------------------------------
  sim_pred_sigs <- sim_pred_sigs*cc_mat
  temp <- matrix(data = colSums(sim_pred_sigs), nrow = n_fa, ncol = n_pred,
                 byrow = TRUE)
  sim_pred_sigs <- sim_pred_sigs/temp



  # Clean up and return --------------------------------------------------------
  rownames(sim_pred_sigs) <- rownames(prey_sigs)
  colnames(sim_pred_sigs) <- paste("Pred", 1:n_pred, sep = "_")

  err_code <- 0
  err_message <- "Success!"

  return(list(sim_pred_sigs = sim_pred_sigs,
              err_code = err_code,
              err_message = err_message))
}

