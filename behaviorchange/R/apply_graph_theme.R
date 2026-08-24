#' Apply multiple DiagrammeR global graph attributes
#'
#' @param graph The [DiagrammeR::DiagrammeR] graph to apply the attributes to.
#' @param ... One or more character vectors of length three, where the first element is
#' the attribute, the second the value, and the third, the attribute type (`graph`,
#' `node`, or `edge`).
#'
#' @return The [DiagrammeR::DiagrammeR] graph.
#' @examples abcd_complete  <- behaviorchange::abcd(behaviorchange::abcd_specs_complete)$output$graph;
#' abcd_complete <- apply_graph_theme(abcd_complete,
#'                                    c("penwidth", 5, "node"),
#'                                    c("penwidth", 15, "edge"));
#' @export
apply_graph_theme <- function(graph,
                              ...) {
  for (currentSetting in list(...)) {
    if ((length(currentSetting) != 3) && is.character(currentSetting)) {
      stop("Only provide character vectors of length 3 in the dots (...) argument!");
    } else {
      graph <-
        DiagrammeR::add_global_graph_attrs(graph,
                                           currentSetting[1],
                                           currentSetting[2],
                                           currentSetting[3]);
    }
  }
  return(graph);
}
