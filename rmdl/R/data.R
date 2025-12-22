#' Data summarization and classification methods
#'
#' These related functions are intended to analyze a single data vector (e.g.
#' column from a dataset) and help predict its classification, or other relevant
#' attributes. These are simple yet opionated convenience functions.
#'
#' @details
#' The functions that are currently supported are:
#'
#' - `number_of_missing()` returns the number of missing values in a vector
#'
#' - `is_dichotomous()` returns TRUE if the vector is dichotomous, FALSE otherwise
#'
#' @param x A vector of any of the atomic types (see [[base::vector()]])
#'
#' @return Returns a single value determined by the individual functions
#' @name data_helpers
NULL

#' @rdname data_helpers
#' @export
number_of_missing <- function(x) {
	if (is.vector(x)) {
		sum(is.na(x))
	}
}

#' @rdname data_helpers
#' @export
is_dichotomous <- function(x) {

	n <- length(stats::na.omit(levels(factor(x))))
	if (n == 2) {
		TRUE
	} else {
		FALSE
	}

}
