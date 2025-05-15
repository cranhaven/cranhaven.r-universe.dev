#' To calculate the modal value of the data
#'
#' This function calculates the value of the mode of the data
#' that is provided
#'
#' @param x The data that is provided
#'
#' @return \code{} Gives the mode
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
mode.val <- function(x) {
      uniqx <- unique(x)
      uniqx[which.max(tabulate(match(x, uniqx)))]
}
#########################################################
