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
#' @export
get_state_transition_df <- function(x) {

  if (!inherits(x, "rock_stateTransitionTable")) {
    stop("As `x`, pass an object of class `rock_stateTransitionTable`, as produced ",
         "by a call to `rock::get_state_transition_table()`. You passed an object of class ",
         vecTxtQ(class(x)), ".");
  }

  totalTransitionsFromState <-
    rep(rowSums(x), each = nrow(x));

  res <- data.frame(
    fromState =
      rep(rownames(x), each = ncol(x)),
    toState =
      rep(colnames(x), times = nrow(x)),
    nrOfTransitions = as.vector(t(x)),
    propOfTransitions = as.vector(t(x)) / totalTransitionsFromState
  );

  class(res) <- c("rock_stateTransitionDf", class(res));

  return(res);

}
