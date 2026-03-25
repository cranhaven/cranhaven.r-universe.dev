

get_layout2 <- function(flows) {

  original_id <- NULL
  x <- NULL
  y <- NULL

  edges <- flows

  # Transforms edges to long format and gives each unique id a number from 1 to n_edges, using as.numeric(factor())
  # (This "node_id" is needed for DiagrammeR.)
  edge_list_long <- edges %>%
    gather("key", "original_id", "sourceRef", "targetRef") %>%
    mutate(node_id = as.numeric(factor(.data$original_id)))

  # Creates key table that maps "original_id" to "node_id"
  node_keys <- edge_list_long %>%
    select(.data$original_id, .data$node_id) %>%
    unique()

  # Removes old id ("original_id") and recreates wide format of edges with new id ("node_id")
  edges <- edge_list_long %>%
    select(-.data$original_id) %>%
    spread(.data$key, .data$node_id)

  # Uses "sourceRef" and "targetRef" (which are now simple ids from 1 till n) to build edge data.frame
  edge_df <-
    create_edge_df(from = edges$sourceRef, to = edges$targetRef)

  # Creates node data.frame with correct number of nodes (which is the number of rows in "node_keys")
  node_df <- create_node_df(nrow(node_keys))

  # Creates graph, sets appropriate layout options, renders graph and saves SVG/dot notation
  dot <- create_graph(node_df, edge_df) %>%
    add_global_graph_attrs(attr = "rankdir",
                           value = "LR",
                           attr_type = "graph") %>%
    add_global_graph_attrs(attr = "layout",
                           value = "dot",
                           attr_type = "graph") %>%
    render_graph(layout = "neato") %>%
    export_svg()

  g_elements <- read_html(dot) %>%
    html_nodes("g")

  # Subsets nodes from "g_elements" (which are elements that have a node attribute)
  nodes_info <-
    g_elements[map_lgl(g_elements, ~ ("node" %in% html_attrs(.x)))]

  # Creates table with the numerical node id and coordinates from the node
  coordinates <-
    tibble(
      node_id = as.numeric(nodes_info %>% html_node("title") %>% html_text()),
      x = as.numeric(nodes_info %>% html_node("ellipse") %>% html_attr("cx")),
      y = as.numeric(nodes_info %>% html_node("ellipse") %>% html_attr("cy")),
      .name_repair = "unique"
    )

  node_keys %>%
    inner_join(coordinates, by = "node_id") %>%
    select(id = original_id, x, y)
}
