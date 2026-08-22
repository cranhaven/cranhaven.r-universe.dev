#' Move one or more codes to a different parent
#'
#' These functions move a code to a different parent (and therefore,
#' ancestry) in one or more sources.
#'
#' @param input One of 1) a character string specifying the path to a file
#' with a source; 2) an object with a loaded source as produced by a call
#' to [load_source()]; 3) a character string specifying the path to a directory
#' containing one or more sources; 4) or an object with a list of loaded
#' sources as produced by a call to [load_sources()].
#' @param codes A character vector with codes to move.
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param decisionLabel A description of the (recoding) decision that was taken.
#' @param output If specified, the recoded source(s) will be written here.
#' @param newAncestry The new parent code, optionally including the partial
#' or full ancestry (i.e. the path of parent codes all the way up to the root).
#' @param justification The justification for this action.
#' @param justificationFile If specified, the justification is appended to
#' this file. If not, it is saved to the `justifier::workspace()`. This can
#' then be saved or displayed at the end of the R Markdown file or R script
#' using `justifier::save_workspace()`.
#' @param preventOverwriting Whether to prevent overwriting existing files
#' when writing the files to `output`.
#' @param encoding The encoding to use.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the changed source(s) or source(s) object.
#' @inheritParams generic_recoding
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Load example source
#' loadedExample <- rock::load_source(exampleFile);
#'
#' ### Move two codes to a new parent, showing progress
#' recoded_source <-
#'   rock::recode_move(
#'     loadedExample,
#'     codes=c("childCode2", "childCode1"),
#'     newAncestry = "parentCode2",
#'     silent=FALSE
#'   );
#' @export
recode_move <- function(input,
                        codes,
                        newAncestry,
                        filter = TRUE,
                        output = NULL,
                        filenameRegex = ".*",
                        outputPrefix = "",
                        outputSuffix = "_rcMoved",
                        decisionLabel = NULL,
                        justification = NULL,
                        justificationFile = NULL,
                        preventOverwriting = rock::opts$get('preventOverwriting'),
                        encoding = rock::opts$get('encoding'),
                        silent = rock::opts$get('silent')) {

  return(
    invisible(
      generic_recoding(
        input = input,
        codes = codes,
        filter = filter,
        func = changeSource_newAncestry,
        output = output,
        filenameRegex = filenameRegex,
        outputPrefix = outputPrefix,
        outputSuffix = outputSuffix,
        decisionLabel = decisionLabel,
        justification = justification,
        justificationFile = justificationFile,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent,
        newAncestry = newAncestry
      )
    )
  );

}

changeSource_newAncestry <- function(input,
                                     codes,
                                     newAncestry,
                                     filter,
                                     silent = rock::opts$get('silent')) {

  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  if (length(codes) > 1) {

    ### Sequentially move codes
    if (!silent) {
      cat0("Multiple codes to move have been specified: starting ",
           "sequential moving of ", length(codes), " codes.\n");
    }

    for (i in seq_along(codes)) {
      input <-
        changeSource_newAncestry(
          input = input,
          codes = codes[i],
          filter = filter,
          newAncestry = newAncestry,
          silent = silent
        );
    }

  } else {
    ### `codes` has length 1

    ### Get clean code, removing any delimiters if they were added
    cleanCode <- cleanCode(codes);

    ### Remove leading '>' if it's there
    newAncestry <- sub(paste0("^", inductiveCodingHierarchyMarker),
                       "",
                       newAncestry);

    ### Add trailing '>' if it's not there
    newAncestry <- sub(paste0(inductiveCodingHierarchyMarker, "?$"),
                       inductiveCodingHierarchyMarker,
                       newAncestry);

    if (!silent) {
      cat0("Moving all occurrences of code '",
           cleanCode,
           "' to new ancestry '",
           newAncestry, "'.\n");
    }

    ### Select elements to check

    filteredUtterances <- input[filter];

    utterancesWithMatches <-
      grep(
        cleanCode,
        filteredUtterances
      );

    if (!silent) {
      cat0("Out of the ", length(input), " utterances in the provided source, ",
           sum(filter), " are selected by the filter, ",
           length(utterancesWithMatches), " of which contain the code text.\n");
    }

    ### Create the regular expression to change ancestry
    regexToChangeAncestry <-
      paste0(
        "(\\s?", ### Optional leading space, start capturing expression 1
        escapeRegexCharacterClass(codeDelimiters[1]),
        ")",     ### Stop capturing expression 1 after opening delimiter
        "(",     ### Not really want to capture; just want to force a hierarchy
                 ### marker after any character that occur (if the target code isn't root)
        validCodeCharacters,   ### Start of current ancestry
        "*",      ### End of current ancestry
        inductiveCodingHierarchyMarker,
        ")?",     ### Allow absence of ancestry
        "(",     ### Start capturing code and descendancy
        cleanCode,
        inductiveCodingHierarchyMarker,
        "?",
        validCodeCharacters,
        "*",
        escapeRegexCharacterClass(codeDelimiters[2]),
        ")"      ### End capturing of code and descendancy
      );

    if (!silent) {
      cat0("Using regular expression '", regexToChangeAncestry, "'.\n");
    }

    for (i in seq_along(utterancesWithMatches)) {

      currentUtterance <- filteredUtterances[utterancesWithMatches[i]];

      ### First simply replace occurrences without ancestry/descendancy
      filteredUtterances[utterancesWithMatches[i]] <-
        gsub(
          regexToChangeAncestry,
          paste0("\\1", newAncestry, "\\3"),
          filteredUtterances[utterancesWithMatches[i]]
        );

      if (!silent) {
        if (identical(currentUtterance, filteredUtterances[utterancesWithMatches[i]])) {
          cat0("--UNCHANGED: ", currentUtterance, "\n");
        } else {
          cat0("--------PRE: ", currentUtterance, "\n",
               "       POST: ", filteredUtterances[utterancesWithMatches[i]], "\n");
        }
      }

    }

    ### Replace processed rows in the input source
    oldInput <- input;
    input[filter] <- filteredUtterances;
    diffCount <- sum(input != oldInput);

    if (!silent) {
      cat0("Moved ", diffCount, " code instances for code '", codes, "'.\n\n");
    }

  }

  return(input);

}
