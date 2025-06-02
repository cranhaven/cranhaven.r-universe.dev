#' @title Calculates the multiplication of gaussian kernel densities
#' @description Returns a vector with the values of the product of densities.
#' @param x a vector with the values of the variable x.
#' @param y a vector with the values of the variable y.
#' @param parameters a vector with the values of the mean and the standard
#' deviation
#' @examples


kernel.multiplication <- function(x, y, parameters) {
  f.x <- dkden(x=x, kerncentres=parameters[[1]], bw=parameters[[3]])
  f.y <- dkden(x=y, kerncentres=parameters[[2]], bw=parameters[[4]])
  value <- f.x * f.y
  return(value)
}
