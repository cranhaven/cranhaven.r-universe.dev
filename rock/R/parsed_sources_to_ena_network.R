#' Create an ENA network out of one or more parsed sources
#'
#' @param x The parsed source(s) as provided by `rock::parse_source` or `rock::parse_sources`.
#' @param unitCols The columns that together define units (e.g. utterances in each
#' source that belong together, for example because they're about the same topic).
#' @param conversationCols The columns that together define conversations (e.g. separate
#' sources, but can be something else, as well).
#' @param codes The codes to include; by default, takes all codes.
#' @param metadata The columns in the merged source dataframe that contain the
#' metadata. By default, takes all read metadata.
#'
#' @return The result of a call to `rENA::ena.plot.network()`, if that is
#' installed.
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
#' ### Add something to indicate which units belong together; normally,
#' ### these would probably be indicated using one of the identifier,
#' ### for example the stanza identifiers, the sid's
#' nChunks <- nrow(parsedExamples$mergedSourceDf) %/% 10;
#' parsedExamples$mergedSourceDf$units <-
#'   c(rep(1:nChunks, each=10), rep(max(nChunks), nrow(parsedExamples$mergedSourceDf) - (10*nChunks)));
#'
#' ### Generate ENA plot
#' \donttest{
#' enaPlot <-
#'   rock::parsed_sources_to_ena_network(parsedExamples,
#'                                       unitCols='units');
#'
#' ### Show the resulting plot
#' print(enaPlot);
#' }
#'
parsed_sources_to_ena_network <- function(x,
                                          unitCols,
                                          conversationCols = 'originalSource',
                                          codes = x$convenience$codingLeaves,
                                          metadata = x$convenience$attributesVars) {

  message("This function is temporarily disables as rENA was archived from CRAN.");

  # if (requireNamespace("rENA", quietly = TRUE)) {
  #
  #   if (!("rock_parsedSource" %in% class(x)) &&
  #       !("rock_parsedSources" %in% class(x))) {
  #     stop(glue::glue("The object you provided (as argument `x`) has class '{vecTxtQ(class(x))}', ",
  #                     "but I can only process objects obtained by parsing one or more sources (with ",
  #                     "`rock::parse_source` or `rock::parse_sources`), which have class 'rock_parsedSource' ",
  #                     "or 'rock_parsedSources'."));
  #   }
  #
  #   allCols <-
  #     c(unitCols,
  #       conversationCols,
  #       codes,
  #       metadata);
  #
  #   if (!all(allCols %in% names(x$mergedSourceDf))) {
  #     stop(glue::glue("Not all columns you specified exist in the 'mergedSourceDf' in the object you provided! Specifically, you provided:\n\n",
  #                     "unitCols = {vecTxtQ(unitCols)}\n",
  #                     "conversationCols = {vecTxtQ(conversationCols)}\n",
  #                     "codes = {vecTxtQ(codes)}\n",
  #                     "metadata = {vecTxtQ(metadata)}\n\n",
  #                     "However, the following columns were not found: {vecTxtQ(allCols[!(allCols %in% names(x$mergedSourceDf))])}."));
  #   }
  #
  #   dat <-
  #     x$mergedSourceDf[, allCols];
  #
  #   ENA_accumulated_data <-
  #     rENA::ena.accumulate.data(
  #       units = dat[, unitCols, drop=FALSE],
  #       conversation = dat[, conversationCols, drop=FALSE],
  #       codes = dat[, codes, drop=FALSE],
  #       metadata = dat[, metadata, drop=FALSE],
  #       window = "Conversation"
  #     );
  #
  #   ENA_set <-
  #     rENA::ena.make.set(enadata=ENA_accumulated_data);
  #
  #   ENA_plot <-
  #     rENA::ena.plot(ENA_set,
  #                    rotation.by = rENA::ena.rotate.by.mean);
  #
  #   ENA_network_plot <-
  #     rENA::ena.plot.network(ENA_plot,
  #                            network=colSums(ENA_set$line.weights));
  #
  #   return(ENA_network_plot);
  #
  # } else {
  #   stop("To create ENA networks, the \"rENA\" package is required. ",
  #        "Please install it using `install.packages('rENA');`.",
  #        call. = FALSE);
  # }


}
