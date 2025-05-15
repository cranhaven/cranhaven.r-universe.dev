#' To create and store calculated values of the objective function
#'
#' This function creates a matrix whose rows and columns depend on the
#' range or distance of the data and the number of strata solutions that the 
#' user is seeking to compute. The matrix stores the objective function values
#' calculated by the algorithm only to be accessed later for the purpose of
#' presenting the OSB.
#'
#' @param my_env The environment my_env has various constants stored
#' from earlier operations dealing with information on the data
#'
#' @return \code{} stores numerical quantities of the objective function
#' and stores in the two matrices inside the my_env to be accessed by other
#' functions
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
create.mat <- function(my_env)
  {
  h <- as.character(my_env$h)
  stages <- my_env$stages
  e <- my_env$e
  
  cat('The program is running, it\'ll take some time!\n')

  # matrix to store 6dp minimum fxn
  my_env$minkf2 <- matrix(-9999, nrow = stages, ncol = e, byrow = TRUE)
  # matrix to store minimum d for 6dp calcs
  my_env$dk2 <- matrix(-9999, nrow = stages, ncol = e, byrow = TRUE)
}
########################################################################