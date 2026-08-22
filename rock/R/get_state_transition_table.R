#' Get the state transition table
#'
#' @param x A parsed source document as provided by [parse_source()].
#' @param rawClassIdentifierCol The identifier of the column in `x`'s QDT with
#' the raw class codings of the class that has the states to look at.
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
                                       rawClassIdentifierCol = "state_raw") {

  if (!inherits(x, "rock_parsedSource")) {
    stop("As `x`, pass an object of class `rock_parsedSource`, as produced ",
         "by a call to `rock::parse_source()`. You passed an object of class ",
         vecTxtQ(class(x)), ".");
  }

  if (!(rawClassIdentifierCol %in% names(x$qdt))) {
    stop("Column doesn't exist!");
  }

  if (length(unique(x$qdt[[rawClassIdentifierCol]])) == 1) {
    stop("No state transitions!");
  }

  states <- x$qdt[[rawClassIdentifierCol]];

  states <- states[!is.na(states)];

  ### https://stackoverflow.com/questions/53641705/how-can-i-count-the-number-of-transitions-from-one-state-to-another
  res <- table(states[-length(states)], states[-1]);

  if (ncol(res) > nrow(res)) {
    rowNames <- rownames(res);
    missingRows <-
      setdiff(colnames(res), rowNames);
    res <- rbind(
      res,
      matrix(rep(0, ncol(res) * length(missingRows)),
             nrow = length(missingRows))
    );
    rownames(res) <- c(rowNames, missingRows);
    res <- res[colnames(res), ];
  } else if (nrow(res) > ncol(res)) {
    colNames <- colnames(res);
    missingCols <-
      setdiff(rownames(res), colNames);
    res <- cbind(
      res,
      matrix(rep(0, nrow(res) * length(missingCols)),
             ncol = length(missingCols))
    );
    colnames(res) <- c(colNames, missingCols);
    res <- res[, rownames(res)];
  }

  class(res) <- c("rock_stateTransitionTable", class(res));

  return(res);

}
