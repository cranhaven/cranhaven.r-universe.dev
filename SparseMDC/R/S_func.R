#'The soft thresholding operator
#'
#'Function to solve the soft thresholding problem
#'@param x The data value
#'@param a The lambda value
#'@return The solution to the soft thresholding operator.
#'@export
#'
S_func <- function(x, a) {  # Soft Thresholding Operator
  return((abs(x) - a) * sign(x) * (abs(x) > a))
}
