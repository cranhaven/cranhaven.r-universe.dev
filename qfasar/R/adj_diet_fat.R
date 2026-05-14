#' Adjust diet composition estimates for prey fat content
#'
#' The function \code{est_diet} estimates diet composition in terms of the
#' mass of fatty acids consumed. The function \code{adj_diet_fat} uses
#' estimates of fatty acid mass per prey type to transform estimates of diet
#' composition in terms of fatty acid mass to another scale.
#'
#' @param prey_fat A numeric vector of the mean fatty acid mass for each prey
#'   type.
#' @param diet_est A numeric vector or matrix of the estimated diet
#'   composition(s) of individual predator(s) or predator type(s). Intended to
#'   be the object \code{est_ind} or \code{est_mean}returned by the function
#'   \code{\link{est_diet}}.
#' @param diet_var A numeric matrix or array containing the estimated variance
#'   matrix for the estimated diet(s) of individual predator(s) or predator
#'   type(s). Intended to be the object \code{var_ind} or \code{var_mean)}
#'   returned by the function \code{\link{est_diet}}. Optional.
#'
#' @return A list containing the following elements: \describe{
#'   \item{diet_est}{A numeric vector or matrix of the estimated diet
#'     composition of individual predator(s) or predator type(s) in terms of
#'     the scale represented by \code{prey_fat}.}
#'   \item{diet_var}{A numeric matrix or array containing the estimated variance
#'     matrix for the estimated diet of individual predator(s) or predator
#'     type(s), in terms of the scale represented by \code{prey_fat}.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#' @section Details:
#' The function \code{est_diet} estimates diet composition in terms of the
#' mass of fatty acids consumed. Such estimates may be of direct ecological
#' interest, especially for ecosystems in which lipids are particularly
#' important. In other cases, one may wish to transform estimates to a different
#' scale. For example, if the units of \code{prey_fat} are (fatty acid mass)/
#' (animal mass), the function \code{adj_diet_fat} returns diet composition
#' estimates in terms of the relative prey mass consumed. Alternatively, if the
#' units of \code{prey_fat} are (fatty acid mass)/(animal), the function
#' \code{adj_diet_fat} returns diet composition estimates in terms of the
#' relative numbers of prey animals consumed.
#'
#' \code{adj_diet_fat} uses the fat transformation of Iverson et al. (2004).
#' Variance matrices are estimated using the delta method (Seber 1982).
#'
#' NOTE: values of mass per prey type are treated as known constants without
#' variance.
#'
#' @section References:
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#'   Quantitative fatty acid signature analysis: A new method of
#'   estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' Seber, G.A.F. 1982. The Estimation of Animal Abundance and Related
#'   Parameters, second edition. Macmillan Publishing Co., New York.
#'
#' @examples
#' adj_diet_fat(prey_fat = c(0.5, 1, 2),
#'              diet_est = c(0.3, 0.2, 0.5),
#'              diet_var = matrix(c( 0.030,  0.004, -0.003,
#'                                  0.004,  0.025, -0.007,
#'                                 -0.003, -0.007,  0.045),
#'                                nrow = 3, ncol = 3))
#'
#' adj_diet_fat(prey_fat = c(0.5, 1, 2),
#'              diet_est = c(0.3, 0.2, 0.5))
#'
#' @export
#'
################################################################################

adj_diet_fat <- function(prey_fat, diet_est, diet_var = NA){


  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.
  fat_est <- NA
  fat_var <- NA


  # Check that prey_fat is a numeric vector.
  if(!(is.vector(prey_fat) & is.numeric(prey_fat))){
    err_code <- 1
    err_message <- "The argument prey_fat is not a numeric vector!"

    return(list(diet_est = fat_est,
                diet_var = fat_var,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that prey_fat contains non-zero data only.
  dum1 <- min(prey_fat) > 0
  if(dum1 == 0 | is.na(dum1)){
    err_code <- 2
    err_message <- "The argument prey_fat contains invalid values!"

    return(list(diet_est = fat_est,
                diet_var = fat_var,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that diet_est is a numeric vector or matrix.
  if(!(is.numeric(diet_est) & (is.vector(diet_est) | is.matrix(diet_est)))){
    err_code <- 3
    err_message <- "The argument diet_est is not a numeric vector or matrix!"

    return(list(diet_est = fat_est,
                diet_var = fat_var,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that diet_est contains non-negative data only.
  dum1 <- max(diet_est) < 0
  if(dum1 == 1 | is.na(dum1)){
    err_code <- 4
    err_message <- "The argument diet_est contains invalid values!"

    return(list(diet_est = fat_est,
                diet_var = fat_var,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the number of prey types is equal in prey_fat and diet_est.
  if(is.vector(diet_est)){
    if(length(prey_fat) != length(diet_est)){
      err_code <- 5
      err_message <- paste("The arguments prey_fat and diet_est have different",
                           "numbers of prey types!",
                           sep = " ")

      return(list(diet_est = fat_est,
                  diet_var = fat_var,
                  err_code = err_code,
                  err_message = err_message))
    }
  } else{
    if(length(prey_fat) != nrow(diet_est)){
      err_code <- 5
      err_message <- paste("The arguments prey_fat and diet_est have different",
                           "numbers of prey types!",
                           sep = " ")

      return(list(diet_est = fat_est,
                  diet_var = fat_var,
                  err_code = err_code,
                  err_message = err_message))

    }
  }


  # If a variance matrix or array is passed.
  if(length(diet_var) > 1){
    have_var <- TRUE
  } else{
    have_var <- FALSE
  }

  if(have_var > 0){

    # Check that the diet_var is a numeric matrix or array.
    if(!(is.numeric(diet_var) & (is.matrix(diet_var) | is.array(diet_var)))){
      err_code <- 5
      err_message <- "The argument diet_var is not a numeric matrix or array!"

      return(list(diet_est = fat_est,
                  diet_var = fat_var,
                  err_code = err_code,
                  err_message = err_message))
    }


    # Check that diet_var does not contain any missing values.
    if(max(is.na(diet_var)) == 1){
      err_code <- 6
      err_message <- "The argument diet_var contains missing values!"

      return(list(diet_est = fat_est,
                  diet_var = fat_var,
                  err_code = err_code,
                  err_message = err_message))
    }


    # Check that the diet_var is square.
    if(is.matrix(diet_var)){
      if(nrow(diet_var) != ncol(diet_var)){
        err_code <- 7
        err_message <- "The argument diet_var is not square!"

        return(list(diet_est = fat_est,
                    diet_var = fat_var,
                    err_code = err_code,
                    err_message = err_message))
      }
    } else{
      if(dim(diet_var)[1] != dim(diet_var)[2]){
        err_code <- 7
        err_message <- "The argument diet_var is not square!"

        return(list(diet_est = fat_est,
                    diet_var = fat_var,
                    err_code = err_code,
                    err_message = err_message))
      }
    }


    # Check that the number of predators is equal.
    if((is.vector(diet_est) & length(dim(diet_var)) != 2) |
       (is.matrix(diet_est) & length(dim(diet_var)) != 3)){
      err_code <- 8
      err_message <- paste("The number of predators in the arguments",
                           "diet_est and diet_var differ!",
                           sep = " ")

      return(list(diet_est = fat_est,
                  diet_var = fat_var,
                  err_code = err_code,
                  err_message = err_message))
    }

    if(is.matrix(diet_est) & is.array(diet_var)){
      if(ncol(diet_est) != dim(diet_var)[3]){
        err_code <- 8
        err_message <- paste("The number of predators in the arguments",
                             "diet_est and diet_var differ!",
                             sep = " ")

        return(list(diet_est = fat_est,
                    diet_var = fat_var,
                    err_code = err_code,
                    err_message = err_message))
      }
    }
  }



  # Transform from biomass to animals ------------------------------------------

  if(is.vector(diet_est)){

    ## Have only one predator or predator type.

    # Compute dimensions.
    n_prey <- length(diet_est)


    # Transform diet estimate.
    fat_est <- diet_est/prey_fat
    sum_fat_est <- sum(fat_est)
    dum1 <- rep(sum_fat_est, n_prey)
    fat_est <- fat_est/dum1


    if(have_var > 0){

      # Compute the derivative matrix.
      deriv <- matrix(data = 0, nrow = n_prey, ncol = n_prey)
      dum2 <- 1/(prey_fat*dum1)
      for(li1 in 1:n_prey){
        for(li2 in 1:n_prey){
          if(li1 == li2){
            deriv[li1,li2] <- dum2[li1]*(1 - diet_est[li1]*dum2[li1])
          } else{
            deriv[li1,li2] <- -diet_est[li1]*dum2[li1]*dum2[li2]
          }
        }
      }


      # Transform variance.
      fat_var <- diet_var
      for(li1 in 1:n_prey){
        for(li2 in 1:n_prey){
          fat_var[li1,li2] <- 0
          for(li3 in 1:n_prey){
            for(li4 in 1:n_prey){
              fat_var[li1,li2] <- fat_var[li1,li2] +
                                diet_var[li3,li4]*deriv[li1,li3]*deriv[li2,li4]
            }
          }
        }
      }
    }
  } else{

    ## Have multiple estimates.

    # Compute dimensions.
    n_prey <- nrow(diet_est)
    n_pred <- ncol(diet_est)


    # Allocate memory.
    fat_est <- diet_est
    if(have_var){
      fat_var <- diet_var
      deriv <- matrix(data = 0, nrow = n_prey, ncol = n_prey)
    }


    # Consider each predator or predator type in turn.
    for(li1 in 1:n_pred){

      # Transform diet estimate.
      fat_est[,li1] <- diet_est[,li1]/prey_fat
      sum_fat_est <- sum(fat_est[,li1])
      dum1 <- rep(sum_fat_est, n_prey)
      fat_est[,li1] <- fat_est[,li1]/dum1


      if(have_var){

        # Compute the derivative matrix.
        dum2 <- 1/(prey_fat*dum1)
        for(li2 in 1:n_prey){
          for(li3 in 1:n_prey){
            if(li2 == li3){
              deriv[li2,li3] <- dum2[li2]*(1 - diet_est[li2,li1]*dum2[li2])
            } else{
              deriv[li2,li3] <- -diet_est[li2,li1]*dum2[li2]*dum2[li3]
            }
          }
        }


        # Transform variance.
        fat_var[1:n_prey,1:n_prey,li1] <- 0
        for(li2 in 1:n_prey){
          for(li3 in 1:n_prey){
            for(li4 in 1:n_prey){
              for(li5 in 1:n_prey){
                fat_var[li2,li3,li1] <- fat_var[li2,li3,li1] +
                                 diet_var[li4,li5,li1]*deriv[li2,li4]*deriv[li3,li5]
              }
            }
          }
        }
      }
    }
  }



  # Return.
  err_code <- 0
  err_message <- "Success!"

  return(list(diet_est = fat_est,
              diet_var = fat_var,
              err_code = err_code,
              err_message = err_message))
}





