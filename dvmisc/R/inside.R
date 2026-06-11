#' Check Whether Numeric Value Falls Inside Two Other Numeric Values
#' 
#' Returns \code{TRUE} if \code{x} falls inside range defined by \code{ends}
#' and \code{FALSE} otherwise. Also works for multiple sets of values and/or
#' endpoints.
#' 
#' @param x Numeric value or vector of numeric values.
#' @param ends Numeric vector of length 2 specifying the endpoints for the 
#' interval, or a 2-column numeric matrix where each row specifies a pair of 
#' endpoints.
#' @param inclusive Logical value indicating whether endpoints should be 
#' included.
#' 
#' @return Logical value or vector.
#' 
#' @examples
#' # Check whether 2 is inside [0, 2.5]
#' inside(1, c(0, 2.5))
#' 
#' # Check whether 2 and 3 are inside (0, 3)
#' inside(c(2, 3), c(0, 3), inclusive = FALSE)
#' 
#' # Check whether 1 is inside [1, 2] and [3, 4]
#' inside(1, rbind(c(1, 2), c(3, 4)))
#' 
#' @export
inside <- function(x, ends, inclusive = TRUE) {
  
  # Check whether x is inside specified interval(s)
  if (! is.matrix(ends)) {
    
    if (inclusive) {
      out <- x >= ends[1] & x <= ends[2]
    } else {
      out <- x > ends[1] & x < ends[2]
    }
    
  } else {
    
    mat <- cbind(x, ends)
    if (inclusive) {
      out <- apply(mat, 1, function(x) x[1] >= x[2] & x[1] <= x[3])
    } else {
      out <- apply(mat, 1, function(x) x[1] > x[2] & x[1] < x[3])
    }
    
  }
  
  # Return logical
  return(out)
  
}
