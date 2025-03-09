#' Get the state transition data frame
#'
#' @param x A state transition table as produced by a call
#' to [get_state_transition_table()].
#'
#' @return A dataframe with columns `fromState`, `toState`,
#' and `nrOfTransitions`.
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
get_state_transition_dot <- function(x) {

  if (!inherits(x, "rock_stateTransitionDf")) {
    stop("As `x`, pass an object of class `rock_stateTransitionDf`, as produced ",
         "by a call to `rock::get_state_transition_df()`. You passed an object of class ",
         vecTxtQ(class(x)), ".");
  }

  x <- x[x$nrOfTransitions > 0, ];

  res <-
    paste0(
      "digraph {\n",
      paste0(
        apply(
          x,
          1,
          function(row) {
            return(
              paste0(
                "  ",
                row[1],
                " -> ",
                row[2],
                " [label='",
                row[4],
                "', penwidth=",
                3 * as.numeric(row[4]),
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
