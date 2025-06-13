#' Save graph
#'
#' Save the graph generated from \code{\link{graph_from_matrix}}
#' or \code{\link{graph_from_links_nodes}} or
#' \code{\link{compare_graphs}}.
#'
#' @param graph : the graph
#' @param filename (optional) : the name of the file and format.
#' Default is "foodingraph_*.png".
#' @param width (optional) : width of the image in cm. Default is 25 cm
#' for a single graph or a comparison in a vertical position.
#' For a comparison in an horizontal position, 40cm.
#' @param height (optional) : height of the image in cm. Default is 20 cm
#' for a single graph, 25cm for a comparison in an horizontal position.
#' For a comparison in a vertical position, 40cm.
#' @param dpi (optional) : the resolution of the image in dpi. Default is 300
#' @param ... : other parameters to pass to the \code{ggsave} ggplot2 function
#' @examples
#' adj_matrix <- cor(iris[,-5])
#' legend <- data.frame(name = colnames(iris[,-5]),
#'                      title = colnames(iris[,-5]))
#' graph_iris <- graph_from_matrix(adj_matrix, legend, main_title = "Iris graph")
#'
#' # Save to a in a temporary file location
#' save_graph(graph_iris, tempfile(fileext = ".png"))
#' @importFrom ggplot2 ggsave
#' @importFrom igraph graph_id
#' @export
save_graph <- function(graph,
                       filename = "foodingraph_%03d.png",
                       width = NULL,
                       height = NULL,
                       dpi = 300,
                       ...) {

  if ( !inherits(graph, "foodingraph") &&
       !inherits(graph, "foodingraph_vertical") &&
       !inherits(graph, "foodingraph_horizontal")) {
    stop("Use graphs created by graph_from_matrix() or graph_from_links_nodes()
         or compare_graphs(). For other situations, use ggsave()")
  }

  if (is.null(width) || is.null(height)) {
    if (inherits(graph, "foodingraph_vertical")) {
      width <- 25
      height <- 40
    } else if (inherits(graph, "foodingraph_horizontal")) {
      width <- 40
      height <- 25
    } else {
      graph <- graph$net
      width <- 25
      height <- 20
    }
  } else if (inherits(graph, "foodingraph")) {
    graph <- graph$net
  }

  ggsave(filename, graph, width = width, height = height, units = "cm", dpi = dpi, ...)
}
