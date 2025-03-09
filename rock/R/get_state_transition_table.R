#' Get the state transition table
#'
#' @param x A parsed source document as provided by [parse_source()].
#' @param classIdentifier The identifier of the class that has the states to
#' looks at.
#'
#' @return A table, with the 'from' states as rows and the 'to' states as columns
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
#' rock::get_state_transition_table(
#'   parsedExample
#' );
#'
#' @export
get_state_transition_table <- function(x,
                                       classIdentifier = "state") {

  if (!inherits(x, "rock_parsedSource")) {
    stop("As `x`, pass an object of class `rock_parsedSource`, as produced ",
         "by a call to `rock::parse_source()`. You passed an object of class ",
         vecTxtQ(class(x)), ".");
  }

  colName <- paste0(classIdentifier, "_raw");

  if (!(colName %in% names(x$mergedSourceDf))) {
    stop("Column doesn't exist!");
  }

  if (length(unique(x$mergedSourceDf[[colName]])) == 1) {
    stop("No state transitions!");
  }

  states <- x$mergedSourceDf[[colName]];

  states <- states[!is.na(states)];

  ### https://stackoverflow.com/questions/53641705/how-can-i-count-the-number-of-transitions-from-one-state-to-another
  res <- table(states[-length(states)], states[-1]);

  class(res) <- c("rock_stateTransitionTable", class(res));

  return(res);

}
