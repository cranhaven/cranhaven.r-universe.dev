#' Count code occurrences
#'
#' @param x A parsed source(s) object.
#' @param codes A regular expression to select codes to include, or,
#' alternatively, a character vector with literal code idenfitiers.
#' @param matchRegexAgainstPaths Whether to match the `codes` regular expression
#' against the full code paths or only against the code identifier.
#'
#' @return A [data.frame()].
#' @export
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-3.rock");
#'
#' ### Load example source
#' loadedExample <- rock::parse_source(exampleFile);
#'
#' ### Show code occurrences
#' rock::count_occurrences(
#'   loadedExample
#' );
count_occurrences <- function(x,
                             codes = ".*",
                             matchRegexAgainstPaths = TRUE) {

  if ((!inherits(x, "rock_parsedSources")) && (!inherits(x, "rock_parsedSource"))) {

    stop("As `x`, you have to pass an object with one or more parsed sources, ",
         "as produced by a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. This object should have class ",
         "`rock_parsedSource` or `rock_parsedSources`, but the object ",
         "you passed has class(es) ", vecTxtQ(class(x)), ".");

  }

  if (length(codes) > 1) {
    codes <- paste(codes, collapse="|");
  }

  if (matchRegexAgainstPaths) {
    codesToInclude <-
      names(x$convenience$codingPaths)[
        grepl(
          codes,
          x$convenience$codingPaths,
          perl = TRUE
        )
      ];
  } else {
    codesToInclude <-
      x$convenience$codingLeaves[
        grepl(
          codes,
          x$convenience$codingLeaves,
          perl = TRUE
        )
      ];
  }

  if (inherits(x, "rock_parsedSource")) {

    counts_total <-
      apply(
        x$qdt[, codesToInclude],
        2,
        sum
      );

    totalUtterances <- nrow(x$qdt);

    totalCodings <- sum(x$qdt[, x$convenience$codingLeaves]);

    totalCodedUtterances <-
      sum(
        as.numeric(
          apply(
            x$qdt[, x$convenience$codingLeaves],
            1,
            function(row) {
              return(any(as.logical(row)));
            }
          )
        )
      );

    proportions_totalCodedUtterances <-
      counts_total / totalCodedUtterances;

    res <- data.frame(
      codeId = codesToInclude,
      count = counts_total,
      totalCodedUtterances = totalCodedUtterances,
      totalUtterances = totalUtterances
    );

  } else if (inherits(x, "rock_parsedSources")) {
    stop("not implemented yet");
  } else {
    stop("As `x`, you have to pass one or more parsed sources, as ",
         "produced by a call to rock::parse_source() or rock::parse_sources(). ",
         "However, the object you passed has class ", rock::vecTxtQ(class(x)), ".");
  }

  return(res);

}

#' @export
print.rock_snoe_plot <- function(x, ...) {
  print(x$plot);
  return(invisible(x));
}
