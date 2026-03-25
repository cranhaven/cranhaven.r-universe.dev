# first_common_node <- function(split, gr) {
#   neighbors(gr, split) %>%
#     map(~subcomponent(gr, .x, mode = "out")) %>%
#     intersect_list() %>%
#     .[1]
# }
# get_block_activities <- function(split, join, gr) {
#   intersect(names(subcomponent(gr, split, "out")),
#             names(subcomponent(gr, join, "in")))
# }
# block_contains_splits <- function(block_activities, splits, current_split) {
#   intersect(block_activities, splits) %>% setdiff(current_split)
# }
# get_blocks <- function(gr) {
#   splits <-  V(gr)[map_lgl(V(gr), ~length(neighbors(gr, .x)) > 1)]
#
#   tibble(split = names(splits), join = map_chr(splits, first_common_node, gr)) %>%
#     mutate(block_activities = map2(split, join, get_block_activities, gr)) %>%
#     mutate(contained_blocks = map2(block_activities, split, ~block_contains_splits(.x, split, .y))) %>%
#     mutate(n_contained_blocks = map_dbl(contained_blocks, length))
# }
# x_position <- function(node, gr, block_length, block_dimensions) {
#
#   if(node %in% block_dimensions$block_id)
#     node_length <- block_dimensions %>% filter(block_id == node) %>% pull(length) + 1
#   else
#     node_length <- 1
#
#   if(length(neighbors(gr, node, mode = "out")) == 0) {
#     return(block_length)
#   } else if (length(neighbors(gr, node, mode = "out")) > 1) {
#     return(1)
#   } else{
#     follow <- names(neighbors(gr, node, mode = "out"))
#
#     return(x_position(follow, gr, block_length, block_dimensions)  - node_length)
#   }
# }
#
#
# y_position <- function(node, gr, block_dimensions) {
#   neighbors(gr, node, mode = "out") -> n_out
#   neighbors(gr, node, mode = "in") -> n_in
#   if(length(n_out) != 1 | length(n_in) == 0) {
#     return(0)
#   } else {
#     if(length(neighbors(gr, n_in, mode = "out")) > 1) {
#
#       branches <- names(neighbors(gr, n_in, mode = "out"))
#     branch_widths <- map(branches, ~ifelse(.x %in% block_dimensions$block_id,
#                                           block_dimensions %>%
#                                             filter(block_id %in% .x) %>%
#                                             pull(width),
#                                           1))
#     offsets <- cumsum(branch_widths)
#     offsets[which(names(neighbors(gr, n_in, mode = "out")) == node)] + 0.5
#
#     } else {
#       y_position(names(n_in), gr, block_dimensions)
#     }
#
#
#   }
# }
# intersect_list <- function(my_list) {
#   results <- names(my_list[[1]])
#   for(i in 2:length(my_list)) {
#     results <- intersect(results, names(my_list[[i]]))
#   }
#   return(results)
# }
#
# get_block_positions <- function(block, block_dimensions) {
#   tibble(
#     node = block$block_activities[[1]],
#     x_offset = map_dbl(block$block_activities[[1]], x_position, block$block_graph[[1]], block$block_length[[1]], block_dimensions),
#     y_offset = map_dbl(block$block_activities[[1]], y_position, block$block_graph[[1]], block_dimensions))
# }
#
# get_path_length <- function(path, block_dimensions) {
#   sum(!(path %in% block_dimensions$block_id)) -> act_length
#   block_dimensions %>% filter(block_id %in% path) %>% pull(length) %>% sum() -> block_length
#
#   act_length + block_length
# }
#
# get_graph_length <- function(graph, split, block_dimensions) {
#   graph %>%
#     all_simple_paths(split) %>%
#     map(names) %>%
#     map_dbl(get_path_length, block_dimensions) %>%
#     max()
# }
