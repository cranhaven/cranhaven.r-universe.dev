# visualiseOperationalisations <- function(shortNames,
#                                          longNames = shortNames) {
#   res <-
#       DiagrammeR::add_full_graph(DiagrammeR::create_graph(directed=FALSE),
#                                  n = length(shortNames),
#                                  type = "connected",
#                                  label = shortNames,
#                                  rel = "connected_to");
#   print(DiagrammeR::render_graph(res));
#   invisible(res);
# }
#
# visualiseOperationalisations(LETTERS[1:20])
