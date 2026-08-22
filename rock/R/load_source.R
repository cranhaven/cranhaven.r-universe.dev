#' Load a source from a file or a string
#'
#' These functions load one or more source(s) from a file or
#' a string and store it in memory for further processing.
#' Note that you'll probably want to clean the sources
#' first, using one of the [clean_sources()] functions,
#' and you'll probably want to add utterance identifiers
#' to each utterance using one of the [prepending_uids()]
#' functions.
#'
#' @param input The filename or contents of the source
#' for `load_source` and the directory containing the
#' sources for `load_sources`.
#' @param encoding The encoding of the file(s).
#' @param silent Whether to be chatty or quiet.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param diligentWarnings Whether to display very diligent warnings.
#'
#' @return Invisibly, an R character vector of
#' classes `rock_source` and `character`.
#' @rdname loading_sources
#' @aliases load_source load_sources loading_sources
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' loadedSource <- rock::load_source(exampleFile);
#' @export
load_source <- function(input,
                        encoding = rock::opts$get('encoding'),
                        silent = rock::opts$get('silent'),
                        rlWarn = rock::opts$get(rlWarn),
                        diligentWarnings = rock::opts$get('diligentWarnings')) {

  if ("rock_source" %in% class(input)) {
    ### All done
    return(input);
  }

  if ((length(input) == 1) && file.exists(input) && (!dir.exists(input))) {
    if (!silent) {
      cat0("Loading source from file '", input, "'.\n");
    }

    res <- readLines(input,
                     encoding=encoding,
                     warn = rlWarn);
  } else {
    res <- input;
    if ((length(input) == 1) && diligentWarnings) {
      warning("As `input`, you specified a single-value character vector ",
              "('", input,
              "') that is not the path to an existing file - I therefore ",
              "loaded it as a literal source. (Use argument ",
              "`diligentWarnings` to deactive this warning.)");
    } else if (!silent){
      if (!silent) {
        cat0("Loading source from the character vector with ",
             length(input), " elements that was passed as input.\n");
      }
    }
  }

  res <- cleaned_source_to_utterance_vector(res);

  class(res) <- c("rock_source", "character");

  return(invisible(res));

}
