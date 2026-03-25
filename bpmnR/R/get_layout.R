# # graph <- gr
# # block_positions <- tibble()
# # block_dimensions <- tibble(block_id, length = numeric(), width = numeric())
# get_layout <- function(graph) {
#   block_positions <- tibble()
#   block_dimensions <- tibble(block_id = numeric(), length = numeric(), width = numeric())
#
#   get_blocks(graph) -> blocks
#
#   while(nrow(blocks) > 0) {
#
#     blocks %>%
#       filter(n_contained_blocks == 0) %>%
#       slice(1) -> current_block
#
#     block_id <- paste0("block_", current_block$split[1])
#
#
#     current_block %>%
#       mutate(block_graph = map(block_activities, ~subgraph(graph, .x))) %>%
#       mutate(block_length = map2_dbl(block_graph, split, get_graph_length, block_dimensions)) %>%
#       get_block_positions(block_dimensions) %>%
#       mutate(block_id = block_id) %>%
#       bind_rows(block_positions, .) -> block_positions
#
#     block_positions %>%
#       group_by(block_id) %>%
#       summarize(width = max(y_offset), length = max(x_offset)) -> block_dimensions
#
#
#     graph %>%
#       delete.vertices(current_block$block_activities[[1]]) %>%
#       add_vertices(1, name = block_id) %>%
#       add_edges(edges = c(names(neighbors(graph, current_block$split[1], "in")), block_id,
#                           block_id, names(neighbors(graph, current_block$join[1], "out")))) -> graph
#
#     # graph %>% add_layout_(as_tree()) %>% plot()
#     # print(block_positions)
#     # print(block_dimensions)
#     get_blocks(graph) -> blocks
#   }
#   startpoint <- V(graph)[map_lgl(V(graph), ~length(neighbors(graph, .x, mode = "in")) == 0)]
#
#   graph_length <- get_graph_length(graph, startpoint, block_dimensions)
#
#   tibble(block_graph = list(graph), block_length = graph_length, block_activities = list(names(V(graph)))) %>%
#     get_block_positions(block_dimensions) %>%
#     mutate(block_id = "process") %>%
#     bind_rows(block_positions, .) -> block_positions
#
#
#   block_positions %>%
#     filter(node %in% block_positions$block_id) -> block_placements
#
#   block_positions %>%
#     left_join(block_placements, by = c("block_id" = "node"), suffix = c("node","block")) %>%
#     replace_na(list(x_offsetblock = 0, y_offsetblock = 0)) %>%
#     mutate(x_offsetnode = x_offsetnode + x_offsetblock,
#            y_offsetnode = y_offsetnode - y_offsetblock) %>%
#     return()
#
# }
