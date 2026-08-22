#' Wordwrapping a source
#'
#' This function wordwraps a source.
#'
#' @param input For `clean_source` and `search_and_replace_in_source`, either a character
#' vector containing the text of the relevant source *or* a path to a file that contains
#' the source text; for `clean_sources` and `search_and_replace_in_sources`, a path to a
#' directory that contains the sources to clean.
#' @param output For `clean_source` and `search_and_replace_in_source`, if not `NULL`,
#' this is the name (and path) of the file in which to save the processed source (if it
#' *is* `NULL`, the result will be returned visibly). For `clean_sources` and
#' `search_and_replace_in_sources`, `output` is mandatory and is the path to the
#' directory where to store the processed sources. This path will be created with a
#' warning if it does not exist. An exception is if "`same`" is specified - in that
#' case, every file will be written to the same directory it was read from.
#' @param length At how many characters to word wrap.
#' @param removeNewlines Whether to remove all newline characters from the source before
#' starting to clean them. **Be careful**: if the source contains YAML fragments, these
#' will also be affected by this, and will probably become invalid!
#' @param removeTrailingNewlines Whether to remove trailing newline characters
#' (i.e. at the end of a character value in a character vector);
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param encoding The encoding of the source(s).
#' @param silent Whether to suppress the warning about not editing the cleaned source.
#' @param utteranceMarker The character(s) between utterances (i.e. marking where
#' one utterance ends and the next one starts). By default, this is a line
#' break, and only change this if you know what you are doing.
#'
#' @return A character vector.
#'
#' @export
#' @examples exampleText <-
#'   paste0(
#'     "Lorem ipsum dolor sit amet, consectetur ",
#'     "adipiscing elit. Nunc non commodo ex, ac ",
#'     "varius mi. Praesent feugiat nunc eget urna ",
#'     "euismod lobortis. Sed hendrerit suscipit ",
#'     "nisl, ac tempus magna porta et. ",
#'     "Quisque libero massa, tempus vel tristique ",
#'     "lacinia, tristique in nulla. Nam cursus enim ",
#'     "dui, non ornare est tempor eu. Vivamus et massa ",
#'     "consectetur, tristique magna eget, viverra elit."
#'   );
#'
#' ### Show example text
#' cat(exampleText);
#'
#' ### Show preprocessed example text
#' cat(
#'   paste0(
#'     rock::wordwrap_source(
#'       exampleText
#'     ),
#'     collapse = "\n"
#'   )
#' );
wordwrap_source <- function(input,
                            output = NULL,
                            length = 40,
                            removeNewlines = FALSE,
                            removeTrailingNewlines = TRUE,
                            rlWarn = rock::opts$get(rlWarn),
                            preventOverwriting = rock::opts$get('preventOverwriting'),
                            encoding = rock::opts$get(encoding),
                            silent = rock::opts$get(silent),
                            utteranceMarker = rock::opts$get('utteranceMarker')) {

  if ((length(input) == 1) && file.exists(input) && (!dir.exists(input))) {
    text <- readLines(input,
                      encoding=encoding,
                      warn=rlWarn);

    if (removeNewlines) {
      text <-
        paste0(text, collapse="");
    } else {
      text <-
        paste0(text, collapse="\n");
    }
  } else {
    text <- input;
    if (removeNewlines) {
      text <-
        paste0(text, collapse="");
        gsub("\\n", "", text);
    }
  }

  non_YAML_indices <-
    unlist(
      yum::find_yaml_fragment_indices(
        text=text,
        delimiterRegEx=rock::opts$get('delimiterRegEx'),
        ignoreOddDelimiters=rock::opts$get('ignoreOddDelimiters'),
        invert = TRUE
      )
    );

  ### Store full source and get only those lines we want to replace
  fullSource <-
    text;

  if ((length(non_YAML_indices) == 1) && (is.numeric(non_YAML_indices))) {
    ### If no YAML fragments are present, non_YAML_indices is just c(1)
    res <- fullSource
  } else {
    res <- fullSource[non_YAML_indices];
  }

  res <- splitString(res, "\\n");

  res <- rock::split_long_lines(
    x = res,
    length = length,
    splitString = utteranceMarker
  );

  if ((length(non_YAML_indices) == 1) && (is.numeric(non_YAML_indices))) {
    ### If no YAML fragments are present, non_YAML_indices is just c(1)
    fullResult <- res;
  } else {
    ### Insert lines that were potentially cleaned back in
    fullResult <- fullSource;
    fullResult[non_YAML_indices] <- res;
  }

  res <- fullResult;

  if (removeTrailingNewlines) {
    res <- gsub(
      "(.*)\\n",
      "\\1",
      res
    );
  }

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
      msg("I just wrote a word wrapped source to file '",
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
    invisible(res);
  }

}
