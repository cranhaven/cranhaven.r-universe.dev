#' Pool diet estimates to combined prey types
#'
#' \code{diet_pool} pools estimated diets and variance matrices to a smaller
#' number of combined prey types. If \code{\link{est_diet}} is used to estimate
#' predator diet composition using a partitioned prey library
#' (\code{\link{make_prey_part}}), \code{diet_pool} pools the partitioned
#' results back to the original, unpartitioned prey types.
#'
#' @param rep_grp The post-multiplication matrix returned by a call to
#'   \code{\link{make_prey_part}} as the object \code{pool_post}, or a
#'   user-defined matrix for custom pooling. Each column defines a prey type
#'   to which estimates should be pooled.
#' @param est_ind A numeric matrix of the estimated diet compositions of
#'   individual predators using a partitioned prey library, intended to be the
#'   object est_ind returned by a call to \code{\link{est_diet}}.
#' @param var_ind A numeric array containing the estimated variance matrix for
#'   the estimated diet of each predator, intended to be the object
#'   var_ind returned by a call to \code{\link{est_diet}}. Optional.
#' @param est_mean A numeric matrix containing the estimated mean diet of each
#'     predator type, intended to be the object est_mean returned by a call to
#'     \code{\link{est_diet}}. Optional.
#' @param var_mean A numeric array containing the estimated variance matrix for
#'     the estimated mean diet of each predator type, intended to be the object
#'     var_mean returned by a call to \code{\link{est_diet}}. Optional.
#'
#' @return A list containing the following elements, all of which are organized
#'   on the basis of the original, unpartitioned prey types: \describe{
#'   \item{est_ind}{A numeric matrix of the estimated diet compositions of
#'   individual predators.}
#'   \item{var_ind}{A numeric array containing the estimated variance matrix for
#'   the estimated mean diet of each predator.}
#'   \item{est_mean}{A numeric matrix containing the estimated mean diet of each
#'     predator type.}
#'   \item{var_mean}{A numeric array containing the estimated variance matrix
#'     for the estimated mean diet of each predator type.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string containing a brief summary of the results.}
#' }
#'
#' @section Details:
#' The function \code{\link{dimac}} explores the prey library for additional
#' structure with identified prey types. If significant structure is found
#' within a library, estimating diet composition on the basis of a
#' partitioned prey library may lead to estimates with less bias and possibly
#' less variation through reduced prey confounding (Bromaghin et al. 2016). The
#' function \code{\link{make_prey_part}} takes the clustering results returned
#' by \code{\link{dimac}} and user specification of the number of clusters in
#' which to partition each prey type and returns a partitioned prey library
#' that is ready for use in diet estimation.
#'
#' However, when estimating diet composition using a partitioned prey library
#' one may still wish to pool partitioned estimates back to the original,
#' unpartitioned prey types for reporting purposes. That is the purpose of the
#' function \code{diet_pool}.
#'
#' NOTE: \code{diet_pool} can also be used to pool estimates into a smaller
#' number of combined prey types for reporting purposes. For example, imagine a
#' prey library with a large number of prey types.  If subsets of the prey
#' types have similar ecological function, their signatures may share some
#' similarities (prey confounding, Bromaghin et al. 2016).  In such a case, one
#' may wish to estimate diet on the basis of the full prey library, but
#' subsequently pool the resulting estimates to a smaller number of combined
#' prey types for reporting purposes (reporting groups, Bromaghin 2008) to
#' reduce the effect of prey confounding. \code{diet_pool} can also be used for
#' this purpose, though the user would need to manually construct the reporting
#' group matrix \code{rep_grp}.
#'
#' @section References:
#' Bromaghin, J.F. 2008. BELS: Backward elimination locus selection for studies
#'   of mixture composition or individual assignment. \emph{Molecular Ecology
#'   Resources} 8:568-571.
#'
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research} 31:597-606.
#'
#' @examples
#' diet_pool(rep_grp = matrix(c(1, 0, 0, 0, 0, 0, 0,
#'                              0, 1, 0, 0, 0, 0, 0,
#'                              0, 1, 0, 0, 0, 0, 0,
#'                              0, 0, 1, 0, 0, 0, 0,
#'                              0, 0, 0, 1, 0, 0, 0,
#'                              0, 0, 0, 1, 0, 0, 0,
#'                              0, 0, 0, 0, 1, 0, 0,
#'                              0, 0, 0, 0, 0, 1, 0,
#'                              0, 0, 0, 0, 0, 1, 0,
#'                              0, 0, 0, 0, 0, 0, 1),
#'                            nrow = 10, byrow = TRUE),
#'          est_ind = matrix(c(0.116, 0.315,
#'                             0.028, 0.073,
#'                             0.000, 0.000,
#'                             0.131, 0.120,
#'                             0.000, 0.000,
#'                             0.000, 0.000,
#'                             0.723, 0.452,
#'                             0.000, 0.000,
#'                             0.000, 0.000,
#'                             0.002, 0.040),
#'                           nrow = 10, byrow = TRUE))
#'
#'
#' @export
#'
################################################################################


diet_pool <- function(rep_grp, est_ind, var_ind = NA, est_mean = NA,
                      var_mean = NA){


  # Check arguments ------------------------------------------------------------

  # Initialize objects to be returned.
  bin_est_ind <- NA
  bin_var_ind <- NA
  bin_est_mean <- NA
  bin_var_mean <- NA


  # Is rep_grp a matrix?
  if(!(is.matrix(rep_grp) & (is.numeric(rep_grp) | is.integer(rep_grp)))){
    err_code <- 1
    err_message <- "The argument rep_grp must be an integer or numeric matrix!"

    return(list(est_ind = bin_est_ind,
                var_ind = bin_var_ind,
                est_mean = bin_est_mean,
                var_mean = bin_var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Does rep_grp contain 0s and 1s only?
  if(sum(rep_grp == 0 | rep_grp == 1) != length(rep_grp)){
    err_code <- 2
    err_message <- "The argument rep_grp must contain 0s and 1s only!"

    return(list(est_ind = bin_est_ind,
                var_ind = bin_var_ind,
                est_mean = bin_est_mean,
                var_mean = bin_var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Is est_ind a numeric matrix?
  if(!(is.matrix(est_ind) & is.numeric(est_ind))){
    err_code <- 3
    err_message <- "The argument est_ind must be a numeric matrix!"

    return(list(est_ind = bin_est_ind,
                var_ind = bin_var_ind,
                est_mean = bin_est_mean,
                var_mean = bin_var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # Compare dimensions of est_ind.
  if(nrow(rep_grp) != nrow(est_ind)){
    err_code <- 4
    err_message <- paste("The number of rows in arguments rep_grp and est_ind",
                         "are unequal!",
                         sep = " ")

    return(list(est_ind = bin_est_ind,
                var_ind = bin_var_ind,
                est_mean = bin_est_mean,
                var_mean = bin_var_mean,
                err_code = err_code,
                err_message = err_message))
  }


  # If var_ind is passed.
  if(length(var_ind) > 1){

    # Is var_ind a numeric array with three dimensions?
    if(!(is.array(var_ind) & is.numeric(var_ind) & length(dim(var_ind)) == 3)){
      err_code <- 5
      err_message <- paste("The argument var_ind must be a numeric array with",
                           "three dimensions!",
                           sep = " ")

      return(list(est_ind = bin_est_ind,
                  var_ind = bin_var_ind,
                  est_mean = bin_est_mean,
                  var_mean = bin_var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }


    # Compare dimensions of est_ind and var_ind.
    if(nrow(est_ind) != dim(var_ind)[1] |
       nrow(est_ind) != dim(var_ind)[2] |
       ncol(est_ind) != dim(var_ind)[3]){
      err_code <- 6
      err_message <- paste("The dimensions of the arguments est_ind and",
                           "var_ind are inconsistent!",
                           sep = " ")

      return(list(est_ind = bin_est_ind,
                  var_ind = bin_var_ind,
                  est_mean = bin_est_mean,
                  var_mean = bin_var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }
  }


  # If est_mean is passed.
  if(length(est_mean) > 1){

    # Compare dimensions of est_ind.
    if(nrow(rep_grp) != nrow(est_mean)){
      err_code <- 7
      err_message <- paste("The number of rows in arguments rep_grp and",
                           "est_mean are unequal!",
                           sep = " ")

      return(list(est_ind = bin_est_ind,
                  var_ind = bin_var_ind,
                  est_mean = bin_est_mean,
                  var_mean = bin_var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }
  }


  # If est_mean and var_mean are passed.
  if(length(est_mean) > 1 & length(var_mean) > 1){

    # Is var_mean a numeric array with three dimensions?
    if(!(is.array(var_mean) & is.numeric(var_mean) &
         length(dim(var_mean)) == 3)){
      err_code <- 8
      err_message <- paste("The argument var_mean must be a numeric array with",
                           "three dimensions!",
                           sep = " ")

      return(list(est_ind = bin_est_ind,
                  var_ind = bin_var_ind,
                  est_mean = bin_est_mean,
                  var_mean = bin_var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }


    # Compare dimensions of est_mean and var_mean.
    if(nrow(est_mean) != dim(var_mean)[1] |
       nrow(est_mean) != dim(var_mean)[2] |
       ncol(est_mean) != dim(var_mean)[3]){
      err_code <- 9
      err_message <- paste("The dimensions of the arguments est_mean and",
                           "var_mean are inconsistent!",
                           sep = " ")

      return(list(est_ind = bin_est_ind,
                  var_ind = bin_var_ind,
                  est_mean = bin_est_mean,
                  var_mean = bin_var_mean,
                  err_code = err_code,
                  err_message = err_message))
    }
  }




  # Pool individual diet estimates ---------------------------------------------
  bin_est_ind <- t(rep_grp) %*% est_ind



  # Pool variance of individuals -----------------------------------------------
  if(length(var_ind) > 1){

    # Allocate memory.
    n_prey_types <- ncol(rep_grp)
    n_preds <- dim(var_ind)[3]
    bin_var_ind <- array(data = 0, dim = c(n_prey_types, n_prey_types, n_preds),
                         dimnames = list(colnames(rep_grp), colnames(rep_grp),
                                         dimnames(var_ind)[[3]]))

    # Compute variance matrix.
    for(li1 in 1:n_preds){
      bin_var_ind[,,li1] <- t(rep_grp) %*% var_ind[,,li1] %*% rep_grp
    }
  }



  # Pool mean diet estimates ---------------------------------------------------
  if(length(est_mean) > 1){
    bin_est_mean <- t(rep_grp) %*% est_mean
  }



  # Pool variances of mean diet estimates --------------------------------------
  if(length(var_mean) > 1){

    # Allocate memory.
    n_prey_types <- ncol(rep_grp)
    n_means <- dim(var_mean)[3]
    bin_var_mean <- array(data = 0, dim = c(n_prey_types, n_prey_types,
                                            n_means),
                          dimnames = list(colnames(rep_grp), colnames(rep_grp),
                                          dimnames(var_mean)[[3]]))

    # Compute variance matrix.
    for(li1 in 1:n_means){
      bin_var_mean[,,li1] <- t(rep_grp) %*% var_mean[,,li1] %*% rep_grp
    }

  }



  # Return ---------------------------------------------------------------------
  err_code <- 0
  err_message <- "Success!"

  return(list(est_ind = bin_est_ind,
              var_ind = bin_var_ind,
              est_mean = bin_est_mean,
              var_mean = bin_var_mean,
              err_code = err_code,
              err_message = err_message))
}
