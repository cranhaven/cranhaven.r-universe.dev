#' Efficiently preprocess data
#'
#' @param input The source
#' @param clean Whether to clean
#' @param cleaningArgs Arguments to use for cleaning
#' @param wordwrap Whether to wordwrap
#' @param wrappingArgs Arguments to use for word wrapping
#' @param prependUIDs Whether to prepend UIDs
#' @param UIDArgs Arguments to use for prepending UIDs
#'
#' @inheritParams wordwrap_source
#'
#' @returns The preprocessed source as character vector.
#' @export
#'
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
#' rock::preprocess_source(
#'   exampleText
#' );
preprocess_source <- function(input,
                              output = NULL,
                              clean = TRUE,
                              cleaningArgs = NULL,
                              wordwrap = TRUE,
                              wrappingArgs = NULL,
                              prependUIDs = TRUE,
                              UIDArgs = NULL,
                              preventOverwriting = rock::opts$get("preventOverwriting"),
                              encoding = rock::opts$get("encoding"),
                              rlWarn = rock::opts$get("rlWarn"),
                              silent = rock::opts$get("silent")) {

  if ((length(input) == 1) && file.exists(input) && (!dir.exists(input))) {
    input <- readLines(input,
                       encoding=encoding,
                       warn=rlWarn);
  }

  ### Clean data if requested
  if (clean) {
    res <-
      do.call(
        clean_source,
        c(list(input = input),
          cleaningArgs)
      );
  } else {
    res <- input;
  }

  ### Wordwrap data if requested
  if (wordwrap) {
    res <-
      do.call(
        wordwrap_source,
        c(list(input = res),
          wrappingArgs)
      );
  }

  ### Word wrap data if requested
  if (prependUIDs) {
    res <-
      do.call(
        prepend_ids_to_source,
        c(list(input = res),
          UIDArgs)
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
      msg("I just wrote a preprocessed source to file '",
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
