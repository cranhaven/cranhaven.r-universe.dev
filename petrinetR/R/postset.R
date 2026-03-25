
#' @title Postset
#'
#' @description Get the postset of a transition or place in a Petri Net
#'
#' @param node \code{\link{character}} of length one: the node id for which to get the postset.
#' @inheritParams flows

#' @export post_set

post_set <- function(PN, node) {
	UseMethod("post_set")
}

post_set.petrinet <- function(PN, node) {
	(flows(PN) %>% filter(from == node))$to %>% return()
}
