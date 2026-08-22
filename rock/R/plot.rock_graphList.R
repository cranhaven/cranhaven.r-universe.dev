#' Plot the graphs in a list of graphs
#'
#' @param x The list of graphs
#' @param ... Any other arguments are passed to [DiagrammeR::render_graph()].
#'
#' @return x, invisibly
#'
#' @method print rock_graphList
#' @export
print.rock_graphList <- function(x, ...) {

  if (is.null(x)) {
    return(invisible(x));
  }

  if (length(x) == 0) {
    return(invisible(x));
  }

  for (i in seq_along(x)) {
    print(
      DiagrammeR::render_graph(
        x[[i]],
        ...
      )
    );
  }

  return(invisible(x));

}
