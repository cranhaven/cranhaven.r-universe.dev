

#' Get nodes from (marked) petrinet
#'
#' @inheritParams flows
#' @export
#'

nodes <- function(PN) {
	UseMethod("nodes")
}

#' @export
nodes.petrinet <- function(PN) {
	places <- places(PN) %>% mutate(type = "place")
	transitions <- transitions(PN) %>% mutate(type = "transition")

	places %>%
		bind_rows(transitions)
}
