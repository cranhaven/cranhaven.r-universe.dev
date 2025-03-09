#' Split a code into multiple codes
#'
#' This function conditionally splits a code into multiple codes. Note that you
#' may want to use [rock::recode_addChildCodes()] instead to not lose the
#' original coding.
#'
#' @param input One of 1) a character string specifying the path to a file
#' with a source; 2) an object with a loaded source as produced by a call
#' to [load_source()]; 3) a character string specifying the path to a directory
#' containing one or more sources; 4) or an object with a list of loaded
#' sources as produced by a call to [load_sources()].
#' @param codes A single character value with the code to split.
#' @param splitToCodes A named list with specifying when to split to which
#' new code. Each element of this list is a filtering criterion that will be
#' passed on to [rock::get_source_filter()] to create the actual filter that
#' will be applied. The name of each element is the code that will be applied
#' to utterances matching that filter. When calling `recode_split()` for a
#' single source, instead of passing the filtering criterion, it is also
#' possible to pass a filter (i.e. the result of the call to
#' [rock::get_source_filter()]), which allows more finegrained control. Note
#' that these split filters and the corresponding codes are processed
#' sequentially in the order specified in `splitToCodes`. This means that once
#' an utterance that was coded with `codes` has been matched to one of these
#' 'split filters' (and so, recoded with the corresponding 'split code', i.e.,
#' with the name of that split filter in `splitToCodes`), it will not be
#' recoded again even if it also matches with other split filters down the
#' line. Any utterances coded with the code to split up (i.e. specified in
#' `codes`) that do not match with any of the split filters specified as the
#' `splitToCodes` elements will not be recoded and so remain coded with
#' `codes`. To create a catch-all ('else') category, pass `".*"` or `TRUE` as
#' a filter (see the example).
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param decisionLabel A description of the (recoding) decision that was taken.
#' @param output If specified, the recoded source(s) will be written here.
#' @param justification The justification for this action.
#' @param justificationFile If specified, the justification is appended to
#' this file. If not, it is saved to the [justifier::workspace()]. This can
#' then be saved or displayed at the end of the R Markdown file or R script
#' using [justifier::save_workspace()].
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
#' ### Split a code into two codes, showing progress
#' recoded_source <-
#'   rock::recode_split(
#'     loadedExample,
#'     codes="childCode1",
#'     splitToCodes = list(
#'       and_REPLACED = " and ",
#'       book_REPLACED = "book",
#'       else_REPLACED = TRUE
#'     ),
#'     silent=FALSE
#'   );
#' @export
recode_split <- function(input,
                         codes,
                         splitToCodes,
                         filter = TRUE,
                         output = NULL,
                         filenameRegex = ".*",
                         outputPrefix = "",
                         outputSuffix = "_recoded",
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
        splitToCodes = splitToCodes,
        filter = filter,
        func = changeSource_splitCode,
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

changeSource_splitCode <- function(input,
                                   codes,
                                   splitToCodes,
                                   filter,
                                   silent = rock::opts$get('silent')) {

  if (is.null(names(splitToCodes))) {
    stop("The `splitToCodes` argument has to be a named list. ",
         "Each element should be a filtering criterion that can be passed ",
         "to rock::get_source_filter(), and every element's name the code ",
         "to be applied when that filtering criterion matches, for example:\n\n",
         "splitToCodes = list(newCode1 = 'positive', newCode2 = 'negative')");
  } else {

    ### Check whether any are filters already
    alreadyFilters <-
      unlist(
        lapply(
          splitToCodes,
          is_source_filter
        )
      );
    if (any(alreadyFilters)) {
      ### If so, get indices
      alreadyFilters <- which(alreadyFilters);
      ### And then check whether they have the correct length
      for (i in alreadyFilters) {
        if (length(splitToCodes[[alreadyFilters]]) != length(input)) {
          stop("As argument `splitToCodes`, you passed ",
               length(alreadyFilters), " filters as produced by ",
               "rock::get_source_filter(). However, of those, at least ",
               "one, specifically the one named '", names(splitToCodes)[i],
               "', is not the same length as the source (",
               length(splitToCodes[[alreadyFilters]]), " versus ",
               length(input), ", respectively). This can happen, for ",
               "example, if you tried to split codes in multiple ",
               "sources with one command. If you do that, you can't pass ",
               "a source filter (as produced by a call to ",
               "rock::get_source_filter()), because source filters ",
               "correspond to the source for which they were constructed (and ",
               "so are always equally long). Instead, if you want to split ",
               "multiple codes in multiple sources wiht one command, you ",
               "have to pass a text string to search for, or a regular ",
               "expression to match against the utterances in the source, ",
               "in `splitToCodes`.");
        }
      }
    }

    noFiltersYet <- which(!alreadyFilters);

    if (length(noFiltersYet) > 0) {
      msg("Creating ", length(noFiltersYet), " source filters.\n",
          silent=silent);
    }

    ### Set filters for those that didn't have them yet
    for (i in noFiltersYet) {
      splitToCodes[[i]] <-
        get_source_filter(source = input,
                          filter = splitToCodes[[i]]);
    }

  }


  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  if (length(codes) > 1) {

    stop("You can only split one code at a time!");

  } else {
    ### `codes` has length 1

    ### Get clean code, removing any delimiters if they were added
    cleanNewCodes <- cleanCode(names(splitToCodes));
    cleanOldCode <- cleanCode(codes);

    msg("Splitting filtered/matching occurrences of code '",
        cleanOldCode,
        "' into ",
        vecTxtQ(cleanNewCodes), ".\n",
        silent=silent);

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

    msg("Using regular expression '", regexToRenameCode, "'.\n",
        silent=silent);

    ### To prevent applying multiple split filters sequentially
    keepingTrackFilter = rep(TRUE, length(filter));

    ### To be able to count how many utterances were recoded
    oldInput <- input;

    for (currentCodeSplit in names(splitToCodes)) {

      ### To document changes in this loop
      keepingTrackFilter_updated <- keepingTrackFilter;

      ### Select utterances to check (filter 'supercedes' the split filters
      ### so is always required)
      filteredUtterances <-
        input[filter & keepingTrackFilter & splitToCodes[[currentCodeSplit]]];

      ### Check which utterances are coded with the code to split
      utterancesWithMatches <-
        grep(
          regexToRenameCode,
          filteredUtterances
        );

      msg("\nOut of the ", length(input), " utterances in the provided source, ",
          sum(filter & splitToCodes[[currentCodeSplit]]),
          " match both the general filter and the split filter for '",
          currentCodeSplit, "' and have not yet been matched by a previous ",
          "split filter. Of these, ",
          length(utterancesWithMatches), " have been coded with ",
          "code '", cleanOldCode, "' and will now be coded with code '",
          currentCodeSplit, "'.\n",
          silent=silent);

      for (i in seq_along(utterancesWithMatches)) {

        currentUtterance <-
          filteredUtterances[utterancesWithMatches[i]];

        ### Replace the code
        filteredUtterances[utterancesWithMatches[i]] <-
          gsub(
            regexToRenameCode,
            paste0("\\1", currentCodeSplit, "\\2"),
            filteredUtterances[utterancesWithMatches[i]]
          );

        if (!silent) {
          if (identical(currentUtterance,
                        filteredUtterances[utterancesWithMatches[i]])) {
            cat0("--UNCHANGED: ", currentUtterance, "\n");
          } else {
            cat0("--------PRE: ", currentUtterance, "\n",
                 "       POST: ", filteredUtterances[utterancesWithMatches[i]], "\n");
            ### Also remove the index of this utterance from the general
            ### filter to prevent coding with another split code
            keepingTrackFilter_updated[utterancesWithMatches[i]] <- FALSE;
          }
        }
      }

      ### Replace processed rows in the input source
      input[filter & keepingTrackFilter & splitToCodes[[currentCodeSplit]]] <-
        filteredUtterances;

      ### Now that we have replaced the rows, we can update the
      ### keepingTrackFilter
      keepingTrackFilter <- keepingTrackFilter_updated;

    }

    ### Count number of recoded utterances
    diffCount <- sum(input != oldInput);

    if (!silent) {
      cat0("\nSplit ", diffCount, " instances of code '",
           cleanOldCode, "' into ",
           vecTxtQ(names(splitToCodes)), ".\n\n");
    }

  }

  return(invisible(input));

}
