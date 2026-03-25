

#' @title Read .PNML file
#' @param file Path to .pnml file
#' @param add_final_marking \code{\link{logical}} (default: \code{TRUE}): add final marking. If \code{TRUE}, all places without outgoing flows are considered part of a single final marking. Overwrite with set_final_marking() if needed. If \code{FALSE}, final_marking is set to \code{NULL}

#' @import xml2
#' @import purrr
#' @return A code{\link{marked_petrinet}}
#'
#' @export
#'
#'
read_PN <- function(file, add_final_marking = TRUE) {

	node <- NULL
	children <- NULL
	initial_marking <- NULL
	final_marking <- NULL
	node_name <- NULL
	target <- NULL
	name <- NULL

		read_xml(file) %>%
			xml_child("net") %>%
			xml_child("page") %>%
			xml_children() -> t


	tibble(name = xml_name(t),
		   id = xml_attr(t, "id"),
		   label = xml_child(t, "name") %>% xml_text(),
		   initial_marking = xml_child(t, "initialMarking") %>% xml_text(),
		   source = xml_attr(t, "source"),
		   target = xml_attr(t, "target")) -> data

	data %>%
		filter(name == "arc") %>%
		rename(from = source,
			   to = target) %>%
		select(-initial_marking, -label) -> flows


	data %>% filter(name == "place") %>% select(id, label, initial_marking) -> places
	data %>% filter(name == "transition") %>% select(id, label) -> transitions

	nodes <- bind_rows(places, transitions) %>% select(id, label)


	#, nodes = nodes, flows = flows)

	create_PN(places = places,
			  transitions = transitions,
			  flows = flows) -> PN

	initial_marking <- places %>% filter(initial_marking == 1) %>% pull(id)

	# final_marking
	if (add_final_marking) {

		# final_marking = any place_id not in arc_source
		place_ids <- places %>% pull(id)
		arc_sources <- data %>%
			filter(name == "arc") %>%
			pull(source)

		final_marking <- setdiff(place_ids, arc_sources)


	} else {
		final_marking <- NULL
	}
	create_PN(places = places,
			  transitions = transitions,
			  flows = flows)

	create_marked_PN(PN = PN, initial_marking = initial_marking, final_marking = final_marking)
}






