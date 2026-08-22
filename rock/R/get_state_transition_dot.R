#' Get the Dot code for a state transition graph
#'
#' @param x A state transition table as produced by a call
#' to [get_state_transition_table()].
#' @param labelFun A function to apply to the edge labels in preprocessing.
#' @param labelFunArgs Arguments to specify to `labelFun` in addition to the
#' first argument (the edge weight, a number).
#'
#' @return The Dot code for a state transition graph.
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "state-example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Show the state transition probabilities
#' exampleTable <- rock::get_state_transition_table(
#'   parsedExample
#' );
#'
#' exampleStateDf <- rock::get_state_transition_df(
#'   exampleTable
#' );
#'
#' exampleDotCode <- rock::get_state_transition_dot(
#'   exampleStateDf
#' );
#'
#' DiagrammeR::grViz(exampleDotCode);
#'
#' @export
get_state_transition_dot <- function(x,
                                     labelFun = base::round,
                                     labelFunArgs = list(digits = 2)) {

  if (!inherits(x, "rock_stateTransitionDf")) {
    stop("As `x`, pass an object of class `rock_stateTransitionDf`, as produced ",
         "by a call to `rock::get_state_transition_df()`. You passed an object of class ",
         vecTxtQ(class(x)), ".");
  }

  x <- x[x$nrOfTransitions > 0, ];

  x[, 'label'] <- x[, 'propOfTransitions'];

  if (!is.null(labelFun)) {
    if (is.function(labelFun)) {
      x[, 'label'] <-
        do.call(
          labelFun,
          c(list(x[, 'propOfTransitions']),
            labelFunArgs)
        );
    }
  }

  res <-
    paste0(
      "digraph {\n",
      "  node[fontname=Arial]\n\n",
      "  edge[fontname=Arial]\n\n",
      paste0(
        apply(
          x,
          1,
          function(row) {
            return(
              paste0(
                "  ",
                row['fromState'],
                " -> ",
                row['toState'],
                " [label=\"  ",
                row['label'],
                "    \", penwidth=",
                3 * as.numeric(row['propOfTransitions']),
                "];"
              )
            );
          }
        ),
        collapse = "\n"
      ),
      "\n}\n"
    );

  class(res) <- c("rock_stateTransitionDot", class(res));

  return(res);

}

#' @export
print.rock_stateTransitionDot <- function(x, ...) {
  print(DiagrammeR::grViz(x, ...));
  return(invisible(x));
}
