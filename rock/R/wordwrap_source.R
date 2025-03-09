#' Cleaning & editing sources
#'
#' These functions can be used to 'clean' one or more sources or perform search and
#' replace taks. Cleaning consists of two operations: splitting the source at
#' utterance markers, and conducting search and replaces using regular expressions.
#'
#' The cleaning functions, when called with their default arguments, will do the following:
#'
#' - Double periods (`..`) will be replaced with single periods (`.`)
#' - Four or more periods (`...` or `.....`) will be replaced with three periods
#' - Three or more newline characters will be replaced by one newline character (which
#' will become more, if the sentence before that character marks the end of an
#' utterance)
#' - All sentences will become separate utterances (in a semi-smart manner;
#' specifically, breaks in speaking, if represented by three periods, are not
#' considered sentence ends, wheread ellipses ("…" or unicode 2026, see the example) *are*.
#' - If there are comma's without a space following them, a space will be inserted.
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
#' @param replacementsPre,replacementsPost Each is a list of two-element vectors,
#' where the first element in each vector contains a regular expression to search for
#' in the source(s), and the second element contains the replacement (these are passed
#' as `perl` regular expressions; see \code{\link{regex}} for more information).
#' Instead of regular expressions, simple words or phrases can also be entered of
#' course (since those are valid regular expressions). `replacementsPre` are executed
#' before the `utteranceSplits` are applied; `replacementsPost` afterwards.
#' @param extraReplacementsPre,extraReplacementsPost To perform more replacements
#' than the default set, these can be conveniently specified in `extraReplacementsPre`
#'  and `extraReplacementsPost`. This prevents you from having to
#' manually copypaste the list of defaults to retain it.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param utteranceSplits This is a vector of regular expressions that specify where to
#' insert breaks between utterances in the source(s). Such breakes are specified using
#' `utteranceMarker`.
#' @param length At how many characters to word wrap.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param removeNewlines Whether to remove all newline characters from the source before
#' starting to clean them. **Be careful**: if the source contains YAML fragments, these
#' will also be affected by this, and will probably become invalid!
#' @param removeTrailingNewlines Whether to remove trailing newline characters
#' (i.e. at the end of a character value in a character vector);
#' @param encoding The encoding of the source(s).
#' @param utteranceMarker The character(s) between utterances (i.e. marking where
#' one utterance ends and the next one starts). By default, this is a line
#' break, and only change this if you know what you are doing.
#' @param silent Whether to suppress the warning about not editing the cleaned source.
#'
#' @return A character vector for `clean_source`, or a list of character vectors,
#' for `clean_sources`.
#' @rdname cleaning_sources
#'
#' @examples exampleSource <-
#' "Do you like icecream?
#'
#'
#' Well, that depends\u2026 Sometimes, when it's..... Nice. Then I do,
#' but otherwise... not really, actually."
#'
#' ### Default settings:
#' cat(clean_source(exampleSource));
#'
#' ### First remove existing newlines:
#' cat(clean_source(exampleSource,
#'                  removeNewlines=TRUE));
#'
#' ### Example with a YAML fragment
#' exampleWithYAML <-
#' c(
#'   "Do you like icecream?",
#'   "",
#'   "",
#'   "Well, that depends\u2026 Sometimes, when it's..... Nice.",
#'   "Then I do,",
#'   "but otherwise... not really, actually.",
#'   "",
#'   "---",
#'   "This acts as some YAML. So this won't be split.",
#'   "Not real YAML, mind... It just has the delimiters, really.",
#'   "---",
#'   "This is an utterance again."
#' );
#'
#' cat(
#'   rock::clean_source(
#'     exampleWithYAML
#'   ),
#'   sep="\n"
#' );
#'
#' @export
wordwrap_source <- function(input,
                            output = NULL,
                            length = 60,
                            removeNewlines = FALSE,
                            removeTrailingNewlines = TRUE,
                            rlWarn = rock::opts$get(rlWarn),
                            preventOverwriting = rock::opts$get('preventOverwriting'),
                            encoding = rock::opts$get(encoding),
                            silent = rock::opts$get(silent),
                            utteranceMarker = rock::opts$get('utteranceMarker')) {

  if ((length(input) == 1) && file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding,
                     warn=rlWarn);

    if (removeNewlines) {
      res <-
        paste0(res, collapse="");
    } else {
      # res <-
      #   paste0(res, collapse="\n");
    }
  } else {
    res <- input;
    if (removeNewlines) {
      res <-
        paste0(res, collapse="");
      res <-
        gsub("\\n", "", res);
    }
  }

  non_YAML_indices <-
    unlist(
      yum::find_yaml_fragment_indices(
        text=res,
        delimiterRegEx=rock::opts$get('delimiterRegEx'),
        ignoreOddDelimiters=rock::opts$get('ignoreOddDelimiters'),
        invert = TRUE
      )
    );

  ### Store full source and get only those lines we want to replace
  fullSource <-
    res;

  res <- fullSource[non_YAML_indices];

  res <- rock::split_long_lines(
    x = res,
    length = length,
    splitString = utteranceMarker
  );

  ### Insert lines that were potentially cleaned back in
  fullResult <- fullSource;
  fullResult[non_YAML_indices] <- res;
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
