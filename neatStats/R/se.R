#'@title Standard Error of Mean
#'
#'@description Simply calculates the standard error of a vector of numbers.
#'@param x Numeric vector.
#'@return Standard error.
#' @seealso \code{\link{mean_ci}}, \code{\link{plot_neat}}
#' @examples
#' se( c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 ) )
#'
#' @export
se = function(x) {
    return(stats::sd(x, na.rm = TRUE) / sqrt(length(stats::na.omit(x))))
}
