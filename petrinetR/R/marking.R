
#' @title Marking
#'
#' @description Get the current marking of a Petri Net
#'
#' @param PN A Petri Net
#'
#' @export marking


marking <- function(PN) {

	lifecycle::deprecate_warn(
		when = "0.3.0",
		what = "marking()",
		with = "initial_marking()")


	return(PN$marking)
}
