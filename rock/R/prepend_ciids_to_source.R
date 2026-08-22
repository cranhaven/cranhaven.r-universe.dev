#' Prepend lines with one or more class instance identifiers to one or more sources
#'
#' These functions add lines with class instance identifiers to the beginning
#' of one or more sources that were read with one of the
#' `loading_sources` functions.
#'
#' @param input The source, or list of sources, as
#' produced by one of the `loading_sources` functions.
#' @param ciids A named character vector, where each element's name
#' is the class identifier (e.g. "codeId" or "participantId") and each
#' element is the class instance identifier.
#' @param output If specified, the coded source will be written here.
#' @param allOnOneLine Whether to add all class instance identifiers to one
#' line (`TRUE`) or add then on successive lines (`FALSE`).
#' @param designationSymbol The symbol to use to designate an instance
#' identifier for a class (can be "`=`" or "`:`" as per the ROCK standard).
#' @param preventOverwriting Whether to prevent overwriting existing files.
#' @param encoding The encoding to use.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the coded source object.
#' @rdname adding_ciids_to_sources
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' loadedExample <-
#'   rock::load_source(exampleFile);
#'
#' ### Add a coder identifier
#' loadedExample <-
#'   rock::prepend_ciids_to_source(
#'     loadedExample,
#'     c("codeId" = "iz0dn96")
#'   );
#'
#' ### Show lines 1-5
#' cat(loadedExample[1:5]);
#'
#' @export
prepend_ciids_to_source <- function(input,
                                    ciids,
                                    output = NULL,
                                    allOnOneLine = FALSE,
                                    designationSymbol = "=",
                                    preventOverwriting = rock::opts$get('preventOverwriting'),
                                    rlWarn = rock::opts$get(rlWarn),
                                    encoding = rock::opts$get('encoding'),
                                    silent = rock::opts$get('silent')) {

  ### Read input, if it's a file
  if ((length(input) == 1) && file.exists(input) && (!dir.exists(input))) {
    input <- readLines(input,
                       encoding=encoding,
                       warn = rlWarn);
    input <- cleaned_source_to_utterance_vector(input);
  } else if ("character" %in% class(input)) {
    input <- cleaned_source_to_utterance_vector(input);
  } else if (!("rock_source" %in% class(input))) {
    stop("With the `input` argument you must pass either the ",
         "path to a file with a source, a character vector ",
         "containing the source, or a ROCK source ",
         "as loaded with load_source or load_sources.\n");
  }

  codeDelimiters <- rock::opts$get(codeDelimiters);

  ciidsToPrepend <-
    paste0(
      codeDelimiters[1],
      names(ciids),
      designationSymbol,
      (ciids),
      codeDelimiters[2]
    )

  if (allOnOneLine) {
    ciidsToPrepend <- paste(ciidsToPrepend, collapse=" ");
  }

  res <-
    c(ciidsToPrepend,
      "",
      input
    );

  if (is.null(output)) {
    class(res) <- c("rock_source", "character");
    return(res);
  } else {

    writingResult <-
      writeTxtFile(
        x = input,
        output = output,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent
      );

    if (writingResult) {
      msg("I just wrote a source with added class instance identifiers to file '",
          output,
          "'.",
          silent = silent);
    } else {
      warning("Could not write output file to `",
              output, "`.");
    }

    class(input) <- c("rock_source", "character");
    return(invisible(input));

  }

}
