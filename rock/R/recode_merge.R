#' Merge two or more codes
#'
#' This function merges two or more codes into one.
#'
#' @param input One of 1) a character string specifying the path to a file
#' with a source; 2) an object with a loaded source as produced by a call
#' to [load_source()]; 3) a character string specifying the path to a directory
#' containing one or more sources; 4) or an object with a list of loaded
#' sources as produced by a call to [load_sources()].
#' @param codes A character vector with the codes to merge.
#' @param mergeToCode A single character vector with the merged code.
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param output If specified, the recoded source(s) will be written here.
#' @param decisionLabel A description of the (recoding) decision that was taken.
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
#'   rock::recode_merge(
#'     loadedExample,
#'     codes=c("childCode2", "grandchildCode2"),
#'     mergeToCode="mergedCode",
#'     silent=FALSE
#'   );
#' @export
recode_merge <- function(input,
                         codes,
                         mergeToCode,
                         filter = TRUE,
                         output = NULL,
                         filenameRegex = ".*",
                         outputPrefix = "",
                         outputSuffix = "_rcMerged",
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
        mergeToCode = mergeToCode,
        filter = filter,
        func = changeSource_mergeCodes,
        output = output,
        filenameRegex = filenameRegex,
        outputPrefix = outputPrefix,
        outputSuffix = outputSuffix,
        decisionLabel = decisionLabel,
        justification = justification,
        justificationFile = justificationFile,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent
      )
    )
  );

}

changeSource_mergeCodes <- function(input,
                                    codes,
                                    mergeToCode,
                                    filter,
                                    silent = rock::opts$get('silent')) {

  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  if (length(mergeToCode) > 1) {

    stop("You can only merge to one code at a time!");

  } else {
    ### `mergeToCode` has length 1

    ### Get clean codes, removing any delimiters if they were added
    cleanOldCodes <-
      unlist(
        lapply(
          codes,
          cleanCode
        )
      );
    cleanMergeToCode <- cleanCode(mergeToCode);

    if (!silent) {
      cat0("Merging all occurrences of codes ",
           vecTxtQ(cleanOldCodes),
           " into '",
           cleanMergeToCode, "'.\n");
    }

    ### Select elements to check

    filteredUtterances <- input[filter];

    utterancesWithMatches <-
      which(
        multigrepl(
          patterns=cleanOldCodes,
          x = filteredUtterances,
          returnMatchesForPatterns = FALSE
        )
      );

    msg("Out of the ", length(input), " utterances in the provided source, ",
        sum(filter), " are selected by the filter, ",
        length(utterancesWithMatches), " of which contain the code texts.\n",
        silent=silent);

    ### Create the regular expressions to change the code
    regexToMergeCode <-
      paste0(
        ### The code has to be preceded by either a delimiter or a marker
        "(",
        escapeRegexCharacterClass(codeDelimiters[1]),
        "|",
        inductiveCodingHierarchyMarker,
        ")(",
        ### The old codes
        paste0(cleanOldCodes, collapse="|"),
        ### The code also has to be succeeded by either a delimiter or a marker
        ")(",
        escapeRegexCharacterClass(codeDelimiters[2]),
        "|",
        inductiveCodingHierarchyMarker,
        ")"
      );

    msg("Using regular expression '", regexToMergeCode, "'.\n",
        silent = silent);

    for (i in seq_along(utterancesWithMatches)) {

      currentUtterance <- filteredUtterances[utterancesWithMatches[i]];

      ### First simply replace occurrences without ancestry/descendancy
      filteredUtterances[utterancesWithMatches[i]] <-
        gsub(
          regexToMergeCode,
          paste0("\\1", cleanMergeToCode, "\\3"),
          filteredUtterances[utterancesWithMatches[i]]
        );

      if (identical(currentUtterance, filteredUtterances[utterancesWithMatches[i]])) {
        msg("--UNCHANGED: ", currentUtterance, "\n",
            silent = silent);
      } else {
        msg("--------PRE: ", currentUtterance, "\n",
            "       POST: ", filteredUtterances[utterancesWithMatches[i]], "\n",
            silent = silent);
      }

    }

    ### Replace processed rows in the input source
    oldInput <- input;
    input[filter] <- filteredUtterances;
    diffCount <- sum(input != oldInput);

    msg("Merged ", diffCount, " code instances of codes ",
        vecTxtQ(codes), " into code '", cleanMergeToCode, "'.\n\n",
        silent=silent);

  }

  return(input);

}
