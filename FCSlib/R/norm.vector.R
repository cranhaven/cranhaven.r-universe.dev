#' @title Min-Max Feature scaling normalization
#' 
#' @description Normalizes a vector using the Min-Max Feature scaling method (a.k.a unity-based normalization)
#' @aliases norm.vector
#' @usage norm.vector(x, a.b = NULL)
#' @param x A vector
#' @param a.b A vector that indicates the minimum and maximum scaling values
#' @details Feature scaling is used to bring all values into the range [0,1]. This is also called unity-based normalization.
#' When 'a.b = NULL' (default), the highest and lowest values in 'x' will turn to 1 and 0, respectively, while all values in between will be re-scaled.
#' Defining 'a.b' will bring all values into the range [a,b].
#' 
#' @export
#' @return A normalized vector
#' @author Alejandro Linares
#' 
#' @examples
#' \donttest{
#' x <- seq(from = 1, to = 100, by = 0.1)
#' y <- sin(sqrt(x))
#' plot(y~x, type = "l")
#' 
#' y.n <- norm.vector(y)
#' plot(y.n~x, type = "l")
#' 
#' y.ab <- norm.vector(y, a.b = c(5,20))
#' plot(y.ab~x, type = "l")
#' }

norm.vector <- function(x, a.b = NULL){
  if (!is.vector(x)){
    stop("'x' must be a vector")
  }
  if (!is.null(a.b)){
    x <- a.b[1] + ((x-min(x))*(a.b[2]-a.b[1]))/(max(x)-min(x))
  } else x <- (x-min(x))/(max(x)-min(x))
  return(x)
}
