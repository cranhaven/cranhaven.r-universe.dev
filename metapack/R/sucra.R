#' get surface under the cumulative ranking curve (SUCRA)
#' @param object the output model from fitting a network meta analysis/regression model
#' @return a list containing SUCRA and the discrete rank probability matrix of size T by T
#' @export
"sucra" <- function(object) {
	UseMethod("sucra", object)
} 