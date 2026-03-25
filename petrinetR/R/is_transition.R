#' @title Is transition
#'
#' @description Check if a transition is part of a petri net.
#'
#' @param transition \code{\link{character}} of length one: the transition id to check.
#' @inheritParams is_node
#' @return logical that indicates whether \code{transition} is a transition in \code{PN}
#' @export is_transition

is_transition <- function(transition, PN) {
	if(transition %in% transitions(PN)$label | transition %in% transitions(PN)$id)
		return(T)
	else
		return(F)
}
