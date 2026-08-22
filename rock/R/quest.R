#' Qualitative/Unified Exploration of State Transitions
#'
#' @param x A parsed source document as provided by [parse_source()].
#' @param rawClassIdentifierCol The identifier of the column in `x`'s QDT with
#' the raw class codings of the class that has the states to look at.
#' @param labelFun A function to apply to the edge labels in preprocessing.
#' @param labelFunArgs Arguments to specify to `labelFun` in addition to the
#' first argument (the edge weight, a number).
#'
#' @return A [DiagrammeR::grViz()] object, which will print to show the
#' QUEST graph.
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
#' ### Show a QUEST graph
#' rock::quest(
#'   parsedExample
#' );
#'
#' @export
quest <- function(x,
                  rawClassIdentifierCol = "state_raw",
                  labelFun = base::round,
                  labelFunArgs = list(digits = 2)) {

  ### Show the state transition probabilities
  state_transition_table <- rock::get_state_transition_table(
    x,
    rawClassIdentifierCol = rawClassIdentifierCol
  );

  state_transition_df <- rock::get_state_transition_df(
    state_transition_table
  );

  state_transition_dot <- rock::get_state_transition_dot(
    state_transition_df,
    labelFun = labelFun,
    labelFunArgs = labelFunArgs
  );

  return(
    DiagrammeR::grViz(state_transition_dot)
  );

}
