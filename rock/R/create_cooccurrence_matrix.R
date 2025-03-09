#' Create a co-occurrence matrix
#'
#' This function creates a co-occurrence matrix based on one or more
#' coded sources. Optionally, it plots a heatmap, simply by calling
#' the [stats::heatmap()] function on that matrix.
#'
#' @param x The parsed source(s) as provided by `rock::parse_source` or `rock::parse_sources`.
#' @param codes The codes to include; by default, takes all codes.
#' @param plotHeatmap Whether to plot the heatmap.
#'
#' @return The co-occurrence matrix; a `matrix`.
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Parse a selection of example sources in that directory
#' parsedExamples <-
#'   rock::parse_sources(
#'     examplePath,
#'     regex = "(test|example)(.txt|.rock)"
#'   );
#'
#' ### Create cooccurrence matrix
#' rock::create_cooccurrence_matrix(parsedExamples);
#'
create_cooccurrence_matrix <- function(x,
                                       codes = x$convenience$codingLeaves,
                                       plotHeatmap = FALSE) {

  if (!("rock_parsedSource" %in% class(x)) &&
      !("rock_parsedSources" %in% class(x))) {
    stop(glue::glue("The object you provided (as argument `x`) has class '{vecTxtQ(class(x))}', ",
                    "but I can only process objects obtained by parsing one or more sources (with ",
                    "`rock::parse_source` or `rock::parse_sources`), which have class 'rock_parsedSource' ",
                    "or 'rock_parsedSources'."));
  }

  simpleMatrix <-
    as.matrix(x$mergedSourceDf[, codes]);

  res <- crossprod(simpleMatrix);

  if (plotHeatmap) {

    df <- as.data.frame(as.table(res));

    plot <-
      rock::heatmap_basic(
        data = df,
        x = "Var1",
        y = "Var2",
        fill = "Freq",
        xLab = NULL,
        yLab = NULL,
        fillLab = NULL
      );

    print(plot);

  }

  return(res);

}
