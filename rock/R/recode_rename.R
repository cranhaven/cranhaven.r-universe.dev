#' Rename one or more codes
#'
#' These functions rename one or more codes in one or more sources.
#'
#' @param input One of 1) a character string specifying the path to a file
#' with a source; 2) an object with a loaded source as produced by a call
#' to [load_source()]; 3) a character string specifying the path to a directory
#' containing one or more sources; 4) or an object with a list of loaded
#' sources as produced by a call to [load_sources()].
#' @param codes A named character vector with codes to rename. Each element
#' should be the new code, and the element's name should be the old code (so
#' e.g. `codes = c(oldcode1 = 'newcode1', oldcode2 = 'newcode2')`).
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param decisionLabel A description of the (recoding) decision that was taken.
#' @param output If specified, the recoded source(s) will be written here.
#'
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
#' @rdname moving_codes
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
#'   rock::recode_rename(
#'     loadedExample,
#'     codes=c(childCode2 = "grownUpCode2",
#'             grandchildCode2 = "almostChildCode2"),
#'     silent=FALSE
#'   );
#' @export
recode_rename <- function(input,
                          codes,
                          filter = TRUE,
                          output = NULL,
                          filenameRegex = ".*",
                          outputPrefix = "",
                          outputSuffix = "_rcRenamed",
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
        func = changeSource_renameCodes,
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

changeSource_renameCodes <- function(input,
                                     codes,
                                     filter,
                                     silent = rock::opts$get('silent')) {

  if (is.null(names(codes))) {
    stop("The `codes` argument has to be a named character vector. ",
         "Each element should be the new code, and the element's name ",
         "should be the old code, for example:\n\n",
         "codes = c(oldcode1 = 'newcode1', oldcode2 = 'newcode2')");
  }

  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  if (length(codes) > 1) {

    ### Sequentially rename codes
    msg("Multiple codes to rename have been specified: starting ",
        "sequential renaming of ", length(codes), " codes.\n\n",
        silent=silent);

    for (i in seq_along(codes)) {
      input <-
        changeSource_renameCodes(
          input = input,
          codes = codes[i],
          filter = filter,
          silent = silent
        );
    }

  } else {
    ### `codes` has length 1

    ### Get clean code, removing any delimiters if they were added
    cleanOldCode <- cleanCode(names(codes));
    cleanNewCode <- cleanCode(codes);

    if (!silent) {
      cat0("Renaming all occurrences of code '",
           cleanOldCode,
           "' to '",
           cleanNewCode, "'.\n");
    }

    ### Select elements to check

    filteredUtterances <- input[filter];

    utterancesWithMatches <-
      grep(
        cleanOldCode,
        filteredUtterances
      );

    if (!silent) {
      cat0("Out of the ", length(input), " utterances in the provided source, ",
           sum(filter), " are selected by the filter, ",
           length(utterancesWithMatches), " of which contain the code text.\n");
    }

    ### Create the regular expressions to change the code
    regexToRenameCode <-
      paste0(
        ### The code has to be preceded by either a delimiter or a marker
        "(",
        escapeRegexCharacterClass(codeDelimiters[1]),
        "|",
        inductiveCodingHierarchyMarker,
        ")",
        ### The code itself - no need to capture that
        cleanOldCode,
        ### The code also has to be succeeded by either a delimiter or a marker
        "(",
        escapeRegexCharacterClass(codeDelimiters[2]),
        "|",
        inductiveCodingHierarchyMarker,
        ")"
      );

    if (!silent) {
      cat0("Using regular expression '", regexToRenameCode, "'.\n");
    }

    for (i in seq_along(utterancesWithMatches)) {

      currentUtterance <- filteredUtterances[utterancesWithMatches[i]];

      ### First simply replace occurrences without ancestry/descendancy
      filteredUtterances[utterancesWithMatches[i]] <-
        gsub(
          regexToRenameCode,
          paste0("\\1", cleanNewCode, "\\2"),
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
      cat0("Renamed ", diffCount, " code instances of code '",
           codes, "' to '", names(codes), "'.\n\n");
    }

  }

  return(input);

}
