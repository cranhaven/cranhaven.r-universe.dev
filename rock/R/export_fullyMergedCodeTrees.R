#' Export the fully merged code tree(s)
#'
#' @param x A parsed source(s) object.
#' @param file The file to save to.
#'
#' @return Invisibly, `NULL`.
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
#' tempFile <- tempfile(fileext = ".svg");
#'
#' ### Export merged code tree
#' export_fullyMergedCodeTrees(
#'   loadedExample,
#'   tempFile
#' );
export_fullyMergedCodeTrees <- function(x, file) {
  if (!is.null(x$fullyMergedCodeTrees) && inherits(x$fullyMergedCodeTrees, "Node")) {

    if (!requireNamespace("rsvg", quietly = TRUE)) {
      stop("To export diagrams, you need to have the {rsvg} package installed. ",
           "You can install it with:\n\n    install.packages('rsvg');")
    }

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

    DiagrammeR::export_graph(
      graph,
      file
    );

    return(invisible());

  } else {
    stop("No fully merged code tree present in `x`!");
  }
}
