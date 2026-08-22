#' Apply multiple DiagrammeR global graph attributes
#'
#' @param graph The [DiagrammeR::DiagrammeR] graph to apply the attributes to.
#' @param ... One or more character vectors of length three, where the first element is
#' the attribute, the second the value, and the third, the attribute type (`graph`,
#' `node`, or `edge`).
#'
#' @return The [DiagrammeR::DiagrammeR] graph.
#' @examples ### Create an example source
#' exampleSource <- '
#' ---
#' codes:
#'   -
#'     id: parentCode
#'     label: Parent code
#'     children:
#'       -
#'         id: childCode1
#'       -
#'         id: childCode2
#'   -
#'     id: childCode3
#'     label: Child Code
#'     parentId: parentCode
#'     children: [grandChild1, grandChild2]
#' ---
#' ';
#'
#' ### Parse it
#' parsedSource <-
#'   rock::parse_source(
#'     text = exampleSource
#'   );
#'
#' ### Extract the deductive code tree from
#' ### the parsed source
#' deductiveCodeTree <-
#'   parsedSource$deductiveCodeTrees;
#'
#' ### Convert it to a DiagrammeR graph
#' miniGraph <-
#'   data.tree::ToDiagrammeRGraph(
#'     deductiveCodeTree
#'   );
#'
#' ### Show the graph
#' \donttest{DiagrammeR::render_graph(
#'   miniGraph
#' );}
#'
#' ### Apply a "theme" (three attributes)
#' miniGraph_themed <-
#'   rock::apply_graph_theme(
#'     miniGraph,
#'     c("rankdir", "TB", "graph"),
#'     c("shape", "square", "node"),
#'     c("style", "solid", "node"),
#'     c("fontname", "Arial", "node"),
#'     c("fontcolor", "#0000BB", "node"),
#'     c("color", "#BB0000", "node")
#'   );
#'
#' ### Show the updated graph
#' \donttest{DiagrammeR::render_graph(
#'   miniGraph_themed
#' );}
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
