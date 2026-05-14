#' Generate a regular grid of diet compositions
#'
#' The function \code{make_diet_grid} generates a systematic grid of
#' regularly-spaced diet compositions with a user-specified resolution.
#'
#' @param uniq_types A factor of unique prey-type names.
#' @param inv_inc The integer inverse of the resolution between consecutive
#' diet compositions.
#'
#' @return A list containing the following elements: \describe{
#'   \item{diet_grid}{A numeric matrix of grid diet compositions, in
#'     column-major format.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#' @section Details:
#' The function \code{make_diet_grid} generates a systematic grid of
#' regularly-spaced diet compositions throughout the space of all possible
#' diets for a given number of prey types. Such a diet composition grid may be
#' useful in some simulation studies of estimator performance (e.g., Bromaghin
#' et al. 2016). Given a diet composition, predator fatty acid signatures can be
#' generated using the function \code{\link{make_pred_sigs}}. The diets of such
#' simulated predators can then be estimated, and the resulting estimates can be
#' compared to the known diet composition to evaluate bias, variance, and
#' perhaps other properties.
#'
#' The algorithm starts with a diet proportion of 1.0 assigned to the first prey
#' type, and therefore 0.0 for the other prey types. The algorithm then begins
#' an iterative loop in which an increment of diet proportion is repeatedly
#' shifted to the other prey types, stopping when the last prey type has a diet
#' proportion of 1.0. The user controls the resolution of the grid by specifying
#' the integer inverse of the desired diet increment.  For example, an inverse
#' increment of 10 would produce diet compositions with proportions shifted by
#' an increment of 0.1. See Bromaghin et al. (2016) for a small example with
#' three prey types and a diet increment of 0.25.  However, note that unlike
#' Bromaghin et al. (2016), \code{make_diet_grid} retains diet compositions
#' comprised of a single prey type.
#'
#' It is critical that the prey-type names match those in the prey library. The
#' easiest way to ensure this happens is to pass the object uniq_types returned
#' a call to the function \code{\link{prep_sig}} as the uniq_types argument.
#' Alternatively, and more risky, a vector of unique prey names can be created
#' using the concatenate function and cast as a factor, i.e.,
#' uniq_types <- as.factor(c("Prey_1", "Prey_2", ..., "Prey_P)).
#'
#' NOTE: The number of possible diets grows quickly as the number of prey types
#' increases and the diet increment decreases, and may exceed memory limits.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty acid
#' signature proportions sum to 1 for diet estimation? Ecological Research
#' 31:597-606.
#'
#' @examples
#' make_diet_grid(uniq_types = as.factor(c("Bearded",
#'                                         "Beluga",
#'                                         "Bowhead",
#'                                         "Ribbon",
#'                                         "Ringed",
#'                                         "Spotted",
#'                                         "Walrus")),
#'                inv_inc = 10)
#'
#' @export
#'
################################################################################

make_diet_grid <- function(uniq_types, inv_inc){

  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.
  diet_grid <- NA


  # Check that uniq_types is a factor.
  if(!is.factor(uniq_types)){
    err_code <- 1
    err_message <- "The argument uniq_types must be a factor!"

    return(list(diet_grid = diet_grid,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the number of prey types exceeds 1.
  n_prey_types <- length(uniq_types)
  if(n_prey_types < 2){
    err_code <- 2
    err_message <- "The number of prey types must exceed 1!"

    return(list(diet_grid = diet_grid,
                err_code = err_code,
                err_message = err_message))

  }


  # Check that inv_inc exceeds 1.
  inv_inc <- as.integer(inv_inc)
  if(inv_inc< 2){
    err_code <- 3
    err_message <- "The argument inv_inc must be an integer exceeding 1!"

    return(list(diet_grid = diet_grid,
                err_code = err_code,
                err_message = err_message))

  }



  # Generate the diet grid -----------------------------------------------------

  # Initialize values.
  n_prey_types <- length(uniq_types)

  diet_grid <- matrix(0, ncol=2, nrow=n_prey_types)
  diet_grid[1,1] <- inv_inc
  diet_grid[1,2] <- inv_inc - 1
  diet_grid[2,2] <- 1
  n_diets <- 2

  row_ind <- 3



  # Start loop.
  keep_going_1 <- TRUE
  while(keep_going_1){

    # Run current increment forward to last prey type.
    for(li1 in row_ind:n_prey_types){
      this_diet <- diet_grid[,n_diets]
      this_diet[li1-1] <- this_diet[li1-1] - 1
      this_diet[li1] <- this_diet[li1] + 1

      diet_grid <- cbind(diet_grid, this_diet)
      n_diets <- n_diets + 1
    }
    row_ind <- n_prey_types


    # Check for termination based on the entire diet being shifted to the last
    # prey type.
    if(diet_grid[n_prey_types,n_diets] == inv_inc){
      keep_going_1 <- FALSE
    } else{

      # See if there are any prey types with 0 immediately to the left of the last
      # prey, and shift prey type to the left if there are adjacent zeros.
      row_ind <- n_prey_types
      keep_going_2 <- TRUE
      while(keep_going_2){
        if(diet_grid[row_ind-1,n_diets] == 0){
          row_ind <- row_ind - 1
        } else {
          keep_going_2 <- FALSE
        }
      }

      if(row_ind < n_prey_types){
        this_diet <- diet_grid[,n_diets]
        this_diet[row_ind] <- this_diet[n_prey_types] + 1
        this_diet[n_prey_types] <- 0
        this_diet[row_ind-1] <- this_diet[row_ind-1] - 1
        diet_grid <- cbind(diet_grid, this_diet)
        n_diets <- n_diets + 1
        row_ind <- row_ind + 1
      }
    }
  }



  # Convert counts of increments to proportions.
  diet_grid <- diet_grid/inv_inc



  # Clean up and return.
  rownames(diet_grid) <- uniq_types
  colnames(diet_grid) <- paste("diet", 1:n_diets, sep = "_")

  err_code <- 0
  err_message <- "Success!"

  return(list(diet_grid = diet_grid,
              err_code = err_code,
              err_message = err_message))
}
