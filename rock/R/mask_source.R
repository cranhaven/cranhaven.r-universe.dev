#' Masking sources
#'
#' These functions can be used to mask a set of utterances or one or more sources.
#'
#' @param input For `mask_utterance`, a character vector where each element is one
#' utterance; for `mask_source`, either a character vector containing the text of the
#' relevant source *or* a path to a file that contains the source text; for `mask_sources`,
#' a path to a directory that contains the sources to mask.
#' @param output For `mask_source`, if not `NULL`, this is the name (and path) of the
#' file in which to save the processed source (if it *is* `NULL`, the result will be
#' returned visibly). For `mask_sources`, `output` is mandatory and is the path to the
#' directory where to store the processed sources. This path will be created with a
#' warning if it does not exist. An exception is if "`same`" is specified - in that
#' case, every file will be written to the same directory it was read from.
#' @param proportionToMask The proportion of utterances to mask, from 0 (none) to
#' 1 (all).
#' @param maskRegex A regular expresssion (regex) specifying the characters to
#' mask (i.e. replace with the masking character).
#' @param maskChar The character to replace the character to mask with.
#' @param perl Whether the regular expression is a perl regex or not.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param encoding The encoding of the source(s).
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param silent Whether to suppress the warning about not editing the cleaned source.
#'
#' @return A character vector for `mask_utterance` and `mask_source`, or a list of
#' character vectors, for `mask_sources`.
#' @rdname masking_sources
#'
#' @export
mask_source <- function(input,
                        output = NULL,
                        proportionToMask = 1,
                        preventOverwriting = rock::opts$get(preventOverwriting),
                        encoding = rock::opts$get(encoding),
                        rlWarn = rock::opts$get(rlWarn),
                        maskRegex = "[[:alnum:]]",
                        maskChar = "X",
                        perl = TRUE,
                        silent = rock::opts$get(silent)) {

  utteranceMarker <- rock::opts$get(utteranceMarker);

  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding,
                     warn = rlWarn);
  } else {
    res <- input;
  }

  ### Collapse everything into one string
  res <-
    paste0(res, collapse="\n");

  ### Split by utterance marker
  res <- strsplit(res,
                  utteranceMarker,
                  perl = TRUE)[[1]];

  ### Mask all utterances
  res <- mask_utterances(res,
                         proportionToMask = proportionToMask,
                         maskRegex = maskRegex,
                         maskChar = maskChar,
                         perl = perl);

  if (is.null(output)) {
    return(res);
  } else {
    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    if (file.exists(output) && preventOverwriting) {
      if (!silent) {
        message("File '",
                output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                "write the masked source to disk.");
      }
    } else {
      con <- file(description=output,
                  open="w",
                  encoding=encoding);
      writeLines(text=res,
                 con=con);
      close(con);
    }
    if (!silent) {
      message("I just wrote a masked source to file '",
              output,
              "'.");
    }
    invisible(res);
  }

}
