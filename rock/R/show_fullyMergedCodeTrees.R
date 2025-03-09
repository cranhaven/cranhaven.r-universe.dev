#' Show the fully merged code tree(s)
#'
#' @param x A parsed source(s) object.
#'
#' @return The result of a call to [DiagrammeR::render_graph()].
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Load example source
#' loadedExample <- rock::parse_source(exampleFile);
#'
#' ### Show merged code tree
#' show_fullyMergedCodeTrees(loadedExample);
show_fullyMergedCodeTrees <- function(x) {
  if (!is.null(x$fullyMergedCodeTrees) && inherits(x$fullyMergedCodeTrees, "Node")) {

    graph <-
      data.tree::ToDiagrammeRGraph(
        x$fullyMergedCodeTrees
      );

    graph <-
      do.call(
        rock::apply_graph_theme,
        c(list(graph = graph),
          rock::opts$get("theme_codeTreeDiagram"))
    );

    return(DiagrammeR::render_graph(graph));

  } else {
    stop("No fully merged code tree present in `x`!");
  }
}
