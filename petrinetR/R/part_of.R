
#' @title Part of
#'
#' @description Check if a node is part of a petri net
#'
#' @param node A node
#' @param PN A Petri Net
#'
#' @export part_of

part_of <- function(node, PN) {

	lifecycle::deprecate_warn(
		when = "0.3.0",
		what = "part_of()",
		with = "is_node()")

	if(node %in% transitions(PN)$id)
		return(T)
	else if(node %in% places(PN)$id)
		return(T)
	else
		return(F)
}

