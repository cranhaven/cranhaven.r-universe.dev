


#' @title Initial Marking
#'
#' @description Get the initial marking of a \code{\link{marked_petrinet}}
#'
#' @param PN A \code{\link{marked_petrinet}}
#'
#' @export initial_marking

initial_marking <- function(PN) {
	UseMethod("initial_marking")
}


initial_marking.marked_petrinet <- function(PN) {

	return(PN$initial_marking)
}
