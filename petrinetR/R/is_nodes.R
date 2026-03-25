
#' @title Is node
#'
#' @description Check if a node is part of a petri net
#'
#' @param node \code{\link{character}} of length one: the node id to check.
#' @param PN \code{\link{petrinet}} or \code{\link{marked_petrinet}}
#'
#' @return logical that indicates whether \code{node} is a node in \code{PN}
#' @export is_node

is_node <- function(node, PN) {

	if(node %in% transitions(PN)$id)
		return(T)
	else if(node %in% places(PN)$id)
		return(T)
	else
		return(F)
}

