


#' @title Render Petri Net
#'
#' @description Visualize Petri Net with bipartite graph.
#'
#' @inheritParams flows
#' @import DiagrammeR
#' @export

render_PN <- function(PN) {
	node_id <- NULL
	label <- NULL

	nodes <- nodes(PN) %>%
		mutate(node_id = 1:n()) %>%
		mutate(color = "blue")

	if ("label" %in% names(nodes)) {
		nodes <- nodes %>%
			mutate(label = if_else(is.na(label), id, label))
	} else {
		nodes <- nodes %>%
			mutate(label = id)
	}

	flows <- flows(PN)

	create_node_df(n = n_nodes(PN),
				   label = nodes$label,
				   color = nodes$color,
				   shape = ifelse(nodes$type == "place", "circle","rectangle")) -> node_df

	flows %>%
		select(from, to) %>%
		left_join(nodes, by = c("from" = "id")) %>%
		rename(from_id = node_id) %>%
		left_join(nodes, by = c("to" = "id")) %>%
		rename(to_id = node_id) -> flows

	create_edge_df(from = flows$from_id,
				   to = flows$to_id) -> edge_df

	create_graph(node_df, edge_df) %>%
		add_global_graph_attrs("layout", "dot","graph") %>%
		add_global_graph_attrs("rankdir", "LR", "graph") %>%
		render_graph()
}

