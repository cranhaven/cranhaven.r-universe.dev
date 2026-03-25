
#' @title Preset
#'
#' @description Get the preset of a transition or place in a Petri Net
#'
#' @param node \code{\link{character}} of length one: the node id for which to get the postset.
#' @inheritParams flows
#'
#' @export pre_set

pre_set <- function(PN, node) {
	UseMethod("pre_set")
}

pre_set.petrinet <- function(PN, node) {
	(flows(PN) %>% filter(to == node))$from %>% return()
}
