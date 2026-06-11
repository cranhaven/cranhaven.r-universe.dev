#' Trim Tail Values off of a Vector
#' 
#' Returns input vector with tail values trimmed off of it. User can specify 
#' tail probability to trim or lower and upper cutpoints for values to retain.
#' 
#' @param x Numeric vector.
#' @param p Numeric value giving tail probability to trim from \code{x}. Can 
#' leave as \code{NULL} if you specify \code{cutpoints}.
#' @param tails Numeric value indicating which tail should be trimmed. Possible 
#' values are \code{"both"}, \code{"lower"}, and \code{"upper"}.
#' @param cutpoints Numeric vector indicating what range of values should be 
#' retained. For example, set to \code{c(0, 1)} to trim all values below 0 or 
#' greater than 1. Can leave as \code{NULL} if you specify \code{p}.
#' @param keep.edge Logical value indicating whether values in \code{x} that are 
#' on the edge of being trimmed (i.e. equal to one of the endpoints) should be 
#' retained.
#' 
#' @return Numeric vector.
#' 
#' @seealso \code{\link{inside}}
#' 
#' @examples
#' # Generate data from N(0, 1) and then trim the lower and upper 1\%
#' x <- rnorm(1000)
#' y <- trim(x, p = 0.01)
#' 
#' # Generate data from N(0, 1) and then trim values outside of (-1.5, 1.5)
#' x <- rnorm(100000)
#' y <- trim(x, cutpoints = c(-1.5, 1.5))
#' 
#' @export
trim <- function(x, p = NULL, tails = "both", cutpoints = NULL, 
                 keep.edge = TRUE) {
  
  # If p specified, calculate cutpoints
  if (! is.null(p)) {
    if (tails == "both") {
      probs <- c(p, 1 - p)
    } else if (tails == "lower") {
      probs <- c(p, 1)
    } else if (tails == "upper") {
      probs <- c(0, 1 - p)
    }
    cutpoints <- quantile(x, probs = probs)
  }
  
  # Trim tails
  x.trimmed <- x[inside(x = x, ends = cutpoints, inclusive = keep.edge)]
  return(x.trimmed)
  
}
