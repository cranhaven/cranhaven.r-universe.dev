#' Code frequencies separate by a variable
#'
#' @param x The object with parsed sources.
#' @param by The variables on which to split when computing code frequencies.
#' @param codes A regular expression specifying the codes fo which to compute
#' the code frequencies.
#' @param returnTidyDf When `TRUE`, return a tidy data frame with the counts
#' in one column, the `by` variable in another, and the code for which the
#' counts are provided in another column. Otherwise, return a 'wide' data
#' frame with the `by` variable in one column, the codes in the other columns,
#' and the counts in the cells.
#'
#' @return A data frame with the code frequencies
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
#' ### Show code frequencies
#' code_freq_by(loadedExample, "nestingLevel");
code_freq_by <- function(x,
                         by,
                         codes = ".*",
                         returnTidyDf = FALSE) {

  if (((!inherits(x, "rock_parsedSources")) &&
       (!inherits(x, "rock_parsedSource"))) ||
      (is.null(x$qdt) || (!is.data.frame(x$qdt)))) {
    stop("As `x`, please provide an object of parsed sources.");
  }

  allCodes <- x$convenience$codingLeaves;
  selectedCodes <- grep(codes, allCodes, value=TRUE);

  if (!by %in% names(x$qdt)) {
    stop("You said to compute frequences by `", by, "`, but I cannot ",
         "find that in the Qualitative Data Table.");
  }

  if (length(selectedCodes) == 0) {
    stop("You said to apply regex `", codes, "` to select codes from the full ",
         "set of ", vecTxtQ(allCodes), ". However, no codes match that regex.");
  }

  byObj <- by(x$qdt[, selectedCodes],
              x$qdt[, by],
              colSums);

  byDfs <-
    lapply(
      as.list(unclass(byObj)),
      function(x) {
        return(as.data.frame(t(x)))
      }
    );

  res <- rock::rbind_df_list(byDfs);

  if (returnTidyDf) {

    colNames <- names(res);

    countsCol <-
      data.frame(
        freq = unlist(res)
      );

    codesCol <- rep(names(res), each = length(names(byDfs)));

    byVarCol <-
      data.frame(
        rep(
          names(byDfs),
          length(colNames)
        )
      );
    names(byVarCol) <- by;

    res <- cbind(byVarCol, codesCol, countsCol);

    row.names(res) <- NULL;

  } else {

    byVarDf <-
      data.frame(names(byDfs));
    names(byVarDf) <- by;

    res <- cbind(byVarDf, res);

  }

  return(res);

}
