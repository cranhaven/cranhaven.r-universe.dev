#' Pooled Sample Variance
#' 
#' Calculates pooled sample variance used in equal variance two-sample t-test.
#' 
#' @param x,y Integer or numeric vectors.
#' @param integer Logical value for whether \code{x} and \code{y} are integer 
#' vectors.
#' 
#' @return Numeric value.
#'
#' @export
pooled_var <- function(x, y, integer = FALSE) {
  n1 <- length(x)
  n2 <- length(y)
  return(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
}