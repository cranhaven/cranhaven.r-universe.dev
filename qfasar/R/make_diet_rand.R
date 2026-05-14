#' Generate random diet compositions
#'
#' The function \code{make_diet_rand} generates a user-specified number of
#' random diet compositions.
#'
#' @param uniq_types A factor of unique prey-type names.
#' @param n_diet The integer number of diet compositions to generate.
#'
#' @return A list containing the following elements: \describe{
#'   \item{diet_rand}{A numeric matrix of random diet compositions, in
#'     column-major format.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#' @section Details:
#' The function \code{make_diet_rand} generates a specified number of random
#' diet compositions to support simulation-based research of the performance
#' of QFASA diet estimation procedures. Given a diet composition, predator
#' fatty acid signatures can be generated using the
#' function \code{\link{make_pred_sigs}}. The diets of such simulated predators
#' can then be estimated, and the diet estimates can be compared to the
#' known diet composition to evaluate bias, variance, and perhaps other
#' properties.
#'
#' The algorithm starts by generating a uniformly distributed random number
#' between 0 and 1 as the diet proportion for the first prey type. The algorithm
#' then considers each additional prey type in turn, generating a uniform random
#' number between zero and 1 minus the sum of the proportions assigned to the
#' preceding prey types. The diet proportion for the last prey type is 1 minus
#' the sum of the other diet proportions. As a hedge against limitations in the
#' random number generator, the proportions are then randomly ordered among prey
#' types.
#'
#' It is critical that the prey-type names match those in the prey library. The
#' easiest way to ensure this happens is to pass the object uniq_types returned
#' a call to the function \code{\link{prep_sig}} as the uniq_types argument.
#' Alternatively, and more risky, a vector of unique prey names can be created
#' using the concatenate function and cast as a factor, i.e.,
#' uniq_types <- as.factor(c("Prey_1", "Prey_2", ..., "Prey_P)).
#'
#' @examples
#' make_diet_rand(uniq_types = as.factor(c("Bearded",
#'                                         "Beluga",
#'                                         "Bowhead",
#'                                         "Ribbon",
#'                                         "Ringed",
#'                                         "Spotted",
#'                                         "Walrus")),
#'                n_diet = 100)
#'
#' @export
#'
################################################################################


make_diet_rand <- function(uniq_types, n_diet){

  # Check inputs for errors ----------------------------------------------------

  # Initialize objects for return.
  diet_rand <- NA


  # Check that uniq_types is a factor.
  if(!is.factor(uniq_types)){
    err_code <- 1
    err_message <- "The argument uniq_types must be a factor!"

    return(list(diet_rand = diet_rand,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the number of prey types exceeds 1.
  n_prey_types <- length(uniq_types)
  if(n_prey_types < 2){
    err_code <- 2
    err_message <- "The number of prey types must exceed 1!"

    return(list(diet_rand = diet_rand,
                err_code = err_code,
                err_message = err_message))

  }


  # Check that the number of diets to generate exceeds 0.
  if(n_diet < 1){
    err_code <- 3
    err_message <- "The number of diets to generate must exceed 0!"

    return(list(diet_rand = diet_rand,
                err_code = err_code,
                err_message = err_message))

  }



  # Generate random diets ------------------------------------------------------

  # Initialize constants.
  n_ran <- 2*n_prey_types - 1



  # Allocate memory.
  diet_rand <- matrix(data = 0, nrow = n_prey_types, ncol = n_diet)
  rownames(diet_rand) <- uniq_types
  colnames(diet_rand) <- paste(rep("diet", n_diet), 1:n_diet, sep = "_")



  # Consider each predator
  for(li1 in 1:n_diet){

    # Generate random numbers for this predator.
    ran_num <- stats::runif(n = n_ran)


    # Construct diet.
    sum_left = 1
    for(li2 in 1:(n_prey_types - 1)){
      diet_rand[li2,li1] = sum_left*ran_num[li2]
      sum_left <- sum_left - diet_rand[li2,li1]
    }
    diet_rand[n_prey_types,li1] <- sum_left


    # Randomly order as a precaution against random number generator weaknesses.
    diet_rand[,li1] <- diet_rand[order(ran_num[n_prey_types:n_ran]),li1]
  }



  # Return.
  err_code <- 0
  err_message <- "Success!"

  return(list(diet_rand = diet_rand,
              err_code = err_code,
              err_message = err_message))
}
