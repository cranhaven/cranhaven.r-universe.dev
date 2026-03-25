


#' @title Final Marking
#'
#' @description Get the final marking of a \code{\link{marked_petrinet}}
#'
#' @param PN A \code{\link{marked_petrinet}}
#'
#' @export final_marking

final_marking <- function(PN) {
	UseMethod("final_marking")
}


final_marking <- function(PN) {

	return(PN$final_marking)
}
