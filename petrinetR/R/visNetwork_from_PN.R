#' @title VisNetwork from PN
#' @description Visualize a Petri Net with an interactive network
#'
#' @inheritParams flows
#' @export visNetwork_from_PN


visNetwork_from_PN <- function(PN) {
	nodes <- bind_rows(
		PN$places %>% mutate(shape = "dot"),
		PN$transitions %>% mutate(shape = "square")
	) %>% suppressWarnings()
	edges <- PN$flows %>%
		mutate(arrows = "to")

	if(!is.null(marking(PN))){
		nodes %>% mutate(color = ifelse(id %in% marking(PN), "darkred", NA)) -> nodes
	}
	visNetwork(nodes, edges) %>%
		visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
		return()
}
