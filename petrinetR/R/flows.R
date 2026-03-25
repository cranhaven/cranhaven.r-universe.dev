#' @title Flows
#'
#' @description Extracts the flows from a (marked) Petri Net
#'
#' @param PN \code{\link{petrinet}} or \code{\link{marked_petrinet}}
#'
#' @return A data.frame containing the flows of the petri net.
#'
#' @export flows

flows <- function(PN) {
	UseMethod("flows")
}

#' @describeIn flows Flow of petrinet
#' @export

flows.petrinet <- function(PN) {
	return(PN$flows)
}

#' @describeIn flows Flow of marked petrinet
#' @export

flows.marked_petrinet <- function(PN) {
	flows(PN$petrinet)
}
