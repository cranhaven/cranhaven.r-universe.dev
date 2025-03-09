#' Convert a QNA network to Linear Topic Map format
#'
#' The Linear Topic Map format, LTM (<https://ontopia.net/download/ltm.html>),
#' allows specification of networks in a human-readable format.
#'
#' @param x The parsed source object (as produced by [rock::parse_source()]),
#' or an object holding multiple parsed sources (as produced
#' by [rock::parse_sources()]).
#'
#' @return If `x` is a single parsed source: a character vector holding the
#' Linear Topic Map specification; or, if multiple network coding schemes were
#' used in parallel, each in a list. If `x` contains multiple parseds sources,
#' a list of such objects (i.e., a list of vectors, or a list of lists of
#' vectors).
#' @param topicmapId,topicmapTitle The topic map's identifier and title.
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Read a souce coded with the Qualitative Network Approach
#' qnaExample <-
#'   rock::parse_source(
#'     file.path(
#'       examplePath,
#'       "network-example-1.rock"
#'     )
#'   );
#'
#' ### Convert and show the topic map
#' cat(
#'   rock::qna_to_tlm(
#'     qnaExample
#'   ),
#'   sep="\n"
#' );
#'
#' @export
qna_to_tlm <- function(x,
                       topicmapId = "rock_qna_topicmap",
                       topicmapTitle = "A ROCK QNA Topic Map") {

  if (inherits(x, "rock_parsedSources")) {
    res <- lapply(
      x,
      qna_to_tlm
    );
  } else if (inherits(x, "rock_parsedSource")) {

    res <- lapply(
      x$networkCodes,
      function(currentNetworkCode) {

        if (!('coded_df' %in% names(currentNetworkCode))) {
          return(NULL);
        } else if (is.null(currentNetworkCode$coded_df) ||
                   (nrow(currentNetworkCode$coded_df) == 0)) {
          return(NULL);
        } else {

          df <- currentNetworkCode$coded_df;

          uniqueTopics <- unique(unlist(df$from, df$to));
          uniqueAssocationTypes <- unique(df$type);

          topicsBit <- paste0("[", uniqueTopics, "]");
          associationTypesBit <- paste0("[", uniqueAssocationTypes, "]");

          associationsBit <-
            apply(
              df,
              1,
              function(x) {
                return(
                  paste0(
                    x['type'], "(", x['from'], ", ", x['to'], ")"
                  )
                );
              }
            );

          res <- c(
            '#VERSION "1.3"',
            paste0('#TOPICMAP ~ ', topicmapId),
            '',
            paste0('[', topicmapId, ' = "', topicmapTitle, '"]'),
            paste0('{rock_qna_topicmap, creation-date, [[', Sys.Date(), ']]}'),
            "",
            "/* topics */",
            "",
            topicsBit,
            "",
            "/* association types */",
            "",
            associationTypesBit,
            "",
            "/* associations */",
            "",
            associationsBit,
            "",
            "/* end */"
          );

        }

      }
    );

    if (length(res) == 1) {
      res <- res[[1]];
    } else if (length(res) == 0) {
      res <- NULL;
    } else {
      names(res) <- names(x$networkCodes);
    }

  } else {
    stop("As `x`, you must pass an object of class `rock_parsedSource` or of ",
         "class `rock_parsedSources`. You passed an object of class(es) ",
         vecTxtQ(class(x)), " instead.");
  }

  return(res);

}
