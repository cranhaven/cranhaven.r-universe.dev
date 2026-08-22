#' Collapse the occurrences in utterances into groups
#'
#' This function collapses all occurrences into groups
#' sharing the same identifier, by default the `stanzaId`
#' identifier (`[[sid=..]]`).
#'
#' @param parsedSource The parsed sources as provided by [parse_source()].
#' @param collapseBy The column in the `sourceDf` (in the `parsedSource` object)
#' to collapse by (i.e. the column specifying the groups to collapse).
#' @param columns The columns to collapse; if unspecified (i.e. `NULL`), all
#' codes stored in the `code` object in the `codings` object in the
#' `parsedSource` object are taken (i.e. all used codes in the `parsedSource`
#' object).
#' @param logical Whether to return the counts of the occurrences (`FALSE`) or
#' simply whether any code occurreded in the group at all (`TRUE`).
#'
#' @return A dataframe with one row for each value of of `collapseBy` and columns
#' for `collapseBy` and each of the `columns`, with in the cells the counts (if
#' `logical` is `FALSE`) or `TRUE` or `FALSE` (if `logical` is `TRUE`).
#'
#' @examples ### Get path to example source
#' exampleFile <-
#'   system.file("extdata", "example-1.rock", package="rock");
#'
#' ### Parse example source
#' parsedExample <-
#'   rock::parse_source(exampleFile);
#'
#' ### Collapse logically, using a code (either occurring or not):
#' collapsedExample <-
#'   rock::collapse_occurrences(parsedExample,
#'                              collapseBy = 'childCode1');
#'
#' ### Show result: only two rows left after collapsing,
#' ### because 'childCode1' is either 0 or 1:
#' collapsedExample;
#'
#' ### Collapse using weights (i.e. count codes in each segment):
#' collapsedExample <-
#'   rock::collapse_occurrences(parsedExample,
#'                              collapseBy = 'childCode1',
#'                              logical=FALSE);
#'
#'
#' @export
collapse_occurrences <- function(parsedSource,
                                 collapseBy = "stanzaId",
                                 columns = NULL,
                                 logical = FALSE) {

  if (!("rock_parsedSource" %in% class(parsedSource))) {
    stop("As argument `parsedSource`, you must specify a parsed source, ",
         "as provided by the `rock::parse_source` function!");
  }

  if (is.null(columns)) {
    columns <-
      unlist(parsedSource$convenience$codingLeaves);
  }

  if (!(all(columns %in% names(parsedSource$sourceDf)))) {
    missingCols <-
      columns[which(!(columns %in% names(parsedSource$sourceDf)))];
    stop("Not all columns specified in the `columns` argument exist in ",
         "the `sourceDf` in the `parsedSource` object you provided.\n\n",
         "Specifically, these columns were missing: ",
         vecTxtQ(missingCols), "\n\nAll existing columns are ",
         vecTxtQ(names(parsedSource$sourceDf)), ").");
  }

  if (!(collapseBy %in% names(parsedSource$sourceDf))) {
    stop("The columns specified in the `collapseBy` argument does not exist in ",
         "the `sourceDf` in the `parsedSource` object you provided!");
  }

  sourceDf <-
    parsedSource$sourceDf[, c(collapseBy,
                              columns),
                          drop=FALSE];

  if (logical) {
    res <-
      stats::aggregate(sourceDf[, columns],
                       sourceDf[, collapseBy, drop=FALSE],
                       function(x) {
                         if (sum(x) == 0) {
                           return(0);
                         } else {
                           return(1);
                         }
                       });
  } else {
    res <-
      stats::aggregate(sourceDf[, columns],
                       sourceDf[, collapseBy, drop=FALSE],
                       sum);
  }

  names(res)[1] <-
    collapseBy;

  return(res);

}
