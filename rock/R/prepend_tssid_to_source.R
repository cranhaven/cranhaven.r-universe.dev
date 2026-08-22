#' Prepend a line with a TSSID to a source
#'
#' This function adds a line with a TSSID (a time-stamped source identifier)
#' to the beginning of a source that was read with one of
#' the `loading_sources` functions. When combined with UIDs, TSSIDs are
#' virtually unique references to a specific data fragment.
#'
#' TSSIDs are a date and time in the UTC timezone, consisting of eight digits
#' (four for the year, two for the month, and two for the day), a `T`, four
#' digits (two for the hour and two for the minute), and a `Z` (to designate
#' that the time is specified in the UTC timezone). TSSIDs are valid ISO8601
#' standard date/times.
#'
#' @param input The source, as produced by one of the `loading_sources`
#' functions, or a path to an existing file that is then imported.
#' @param moment Optionally, the moment as a character value of the form
#' `2025-05-28 11:30 CEST` (so, `YYYY-MM-DD HH-MM`).
#' @param output If specified, the coded source will be written here.
#' @param designationSymbol The symbol to use to designate an instance
#' identifier for a class (can be "`=`" or "`:`" as per the ROCK standard).
#' @param preventOverwriting Whether to prevent overwriting existing files.
#' @param encoding The encoding to use.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the coded source object.
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
#'   rock::prepend_tssid_to_source(
#'     loadedExample,
#'     moment = "2025-05-28 11:30 CEST"
#'   );
#'
#' ### Show the first line
#' cat(loadedExample[1]);
#'
#' @export
prepend_tssid_to_source <- function(input,
                                    moment = format(Sys.time(), "%Y-%m-%d %H:%M"),
                                    output = NULL,
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

  tssidToPrepend <-
    rock::generate_tssid(
      format(
        as.POSIXct(
          moment
        ),
        "%Y-%m-%d %H:%M:%S %Z",
        tz="UTC"
      )
    );

  tssidToPrepend <-
    paste0(
      codeDelimiters[1],
      "tssid",
      designationSymbol,
      tssidToPrepend,
      codeDelimiters[2]
    );

  res <-
    c(tssidToPrepend,
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
