#' Prepending unique utterance identifiers
#'
#' This function prepends unique utterance identifiers to each
#' utterance (line) in a source. Note that you'll probably want
#' to clean the sources using [clean_sources()] first.
#'
#' @param input The filename or contents of the source
#' for `prepend_ids_to_source`; and the directory containing the
#' sources, or a list of character vectors, for `prepend_ids_to_sources`.
#' @param output The filename where to write the resulting file for
#' `prepend_ids_to_source` and the directory where to write the
#' resulting files for `prepend_ids_to_sources`
#' @param origin The time to use for the first identifier.
#' @param follow A vector of one or more UIDs (or a list; lists are
#' recursively `unlist()`ed); the highest UID will be taken, converted
#' to a timestamp, and used as `origin` (well, 0.01 second later), so that the
#' new SQUIDs will follow that sequence (see [squids::squids()]).
#' @param followBy When following a vector of UIDs, this can be used to
#' specify the distance between the two vectors (see [squids::squids()]).
#' @param preventOverwriting Whether to overwrite existing files (`FALSE`)
#' or prevent that from happening (`TRUE`).
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param encoding The encoding of the file(s).
#' @param silent Whether to be chatty or quiet.
#'
#' @return The source with prepended uids, either invisible (if `output`
#' if specified) or visibly (if not).
#' @aliases prepending_uids
#' @rdname prepending_uids
#' @examples ### Simple example
#' rock::prepend_ids_to_source(
#'   "brief\nexample\nsource"
#' );
#'
#' ### Example including fake YAML fragments
#' longerExampleText <-
#'   c(
#'     "---",
#'     "First YAML fragment",
#'     "---",
#'     "So this is an utterance (i.e. outside of YAML)",
#'     "This, too.",
#'     "---",
#'     "Second fragment",
#'     "---",
#'     "Another real utterance outside of YAML",
#'     "Another one outside",
#'     "Last 'real utterance'"
#'   );
#'
#' rock::prepend_ids_to_source(
#'   longerExampleText
#' );
#'
#' @export
prepend_ids_to_source <- function(input,
                                  output = NULL,
                                  origin=Sys.time(),
                                  follow = NULL,
                                  followBy = NULL,
                                  rlWarn = rock::opts$get(rlWarn),
                                  preventOverwriting=rock::opts$get(preventOverwriting),
                                  encoding=rock::opts$get(encoding),
                                  silent=rock::opts$get(silent)) {

  delimiterRegEx <- rock::opts$get(delimiterRegEx);
  ignoreOddDelimiters <- rock::opts$get(ignoreOddDelimiters);

  if ((length(input) == 1) && file.exists(input) && (!dir.exists(input))) {
    textToProcess <- readLines(
      input,
      encoding=encoding,
      warn = rlWarn
    );
  } else {
    textToProcess <- input;
    if ((length(textToProcess) == 1) && grepl('\n', textToProcess)) {
      textToProcess <-
        strsplit(textToProcess,
                 "\n")[[1]];
    }
  }

  non_YAML_indices <-
    unlist(
      yum::find_yaml_fragment_indices(
        text=textToProcess,
        delimiterRegEx=delimiterRegEx,
        ignoreOddDelimiters=ignoreOddDelimiters,
        invert = TRUE
      )
    );

  uids <-
    generate_uids(length(non_YAML_indices),
                  origin=origin,
                  follow=follow,
                  followBy = followBy);

  res <- textToProcess;

  res[non_YAML_indices] <- paste0(uids, " ", res[non_YAML_indices]);

  if (is.null(output)) {
    return(res);
  } else {

    writingResult <-
      writeTxtFile(
        x = res,
        output = output,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent
      );

    if (writingResult) {
      msg("I just wrote a file with a source with prepended utterance identifiers (uids) to '",
          output,
          "'. Note that this file may be overwritten if this ",
          "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
          "Therefore, make sure to copy it to ",
          "another directory, or rename it, before starting to code this source!",
          silent = silent);
    } else {
      warning("Could not write output file to `",
              output, "`.");
    }

    return(invisible(res));
  }

}
