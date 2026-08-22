#' Get utterances and codes from source
#'
#' This is a convenience function to use when displaying a source. It returns
#' an object with the raw and clean utterances in a source, as well as the
#' utterance identifiers and a list with vectors of the codes for each
#' utterance.
#'
#' @param x Either the result of a call to [rock::parse_source()], or
#' a path or text to pass to [rock::parse_source()].
#' @param ... Arguments to [rock::parse_source()], which is called to
#' parse the source.
#'
#' @return A list containing `$utterances_raw`, `$utterances_clean`, `$uids$`,
#' `$codeMatches`, and `$codesPerUtterance`.
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
#' ### Parse single example source
#' res <-
#'   rock::get_utterances_and_codes_from_source(
#'     exampleFile
#'   );
get_utterances_and_codes_from_source <- function(x, ...) {

  if (inherits(x, "rock_parsedSource")) {
    parsedSource <- x;
  } else {
    parsedSource <- rock::parse_source(x, ...);
  }

  codeMatches <- lapply(parsedSource$codeProcessing, `[[`, "matches");

  if (length(codeMatches) == 1) {
    codesPerUtterance <- codeMatches[[1]];
  } else if (all(unlist(lapply(codeMatches, is.list)))) {
    nrOfLists <- length(codeMatches);
    nrOfElements <- length(codeMatches[[1]]);
    res <- list();
    for (i in nrOfElements) {
      res[[i]] <- c();
      for (j in nrOfLists) {
        res[[i]] <- c(res[[i]],
                      codeMatches[[j]][[i]]);
      }
    }
    codesPerUtterance <- res;
  } else if (sum(unlist(lapply(codeMatches, is.list))) == 1) {
    codesPerUtterance <-
      codeMatches[[
        which(unlist(lapply(codeMatches, is.list)))
      ]];
  }

  res <-
    list(utterances_raw = parsedSource$mergedSourceDf$utterances_raw,
         utterances_clean = parsedSource$mergedSourceDf$utterances_clean,
         uids = parsedSource$mergedSourceDf$uids,
         codeMatches = codeMatches,
         codesPerUtterance = codesPerUtterance
    );

  return(res);

}

