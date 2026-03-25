#' Transitions
#' @description Extracts the transitions from a Petri Net
#'
#' @inheritParams flows
#'
#' @export transitions

transitions <- function(PN) {
	UseMethod("transitions")
}

#' @describeIn transitions Transitions of petrinet
#' @export
transitions.petrinet <- function(PN) {
	return(PN$transitions)
}

#' @describeIn transitions Transitions of marked petrinet
#' @export
transitions.marked_petrinet <- function(PN) {
	transitions(PN$petrinet)
}
