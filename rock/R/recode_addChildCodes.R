#' Add child codes under a parent code
#'
#' This function conditionally adds new child codes under a code. Where
#' [rock::recode_split()] removes the original code (splitting
#' it into the new codes), this function retains the original, adding the new
#' codes as sub-codes.
#'
#' @param input One of 1) a character string specifying the path to a file
#' with a source; 2) an object with a loaded source as produced by a call
#' to [load_source()]; 3) a character string specifying the path to a directory
#' containing one or more sources; 4) or an object with a list of loaded
#' sources as produced by a call to [load_sources()].
#' @param codes A single character value with the code to add the child codes
#' to.
#' @param childCodes A named list with specifying when to add which
#' child code. Each element of this list is a filtering criterion that will be
#' passed on to [rock::get_source_filter()] to create the actual filter that
#' will be applied. The name of each element is the code that will be applied
#' to utterances matching that filter. When calling `recode_addChildCodes()`
#' for a single source, instead of passing the filtering criterion, it is also
#' possible to pass a filter (i.e. the result of the call to
#' [rock::get_source_filter()]), which allows more finegrained control. Note
#' that these 'child code filters' and the corresponding codes are processed
#' sequentially in the order specified in `childCodes`. Any utterances coded
#' with the code specified in `codes` that do not match with any of the 'child
#' code filters' specified as the `childCodes` elements will remain unchanged.
#' To create a catch-all ('else') category, pass `".*"` or `TRUE` as
#' a filter (see the example).
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param decisionLabel A description of the (recoding) decision that was taken.
#' @param output If specified, the recoded source(s) will be written here.
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
#' loadedExampleSource <- rock::load_source(exampleFile);
#'
#' ### Split a code into two codes, showing progress (the backticks are
#' ### used to be able to specify a name that starts with an underscore)
#' recoded_source <-
#'   rock::recode_addChildCodes(
#'     loadedExampleSource,
#'     codes="childCode1",
#'     childCodes = list(
#'       `_and_` = " and ",
#'       `_book_` = "book",
#'       `_else_` = TRUE
#'     ),
#'     silent=FALSE
#'   );
#' @export
recode_addChildCodes <- function(input,
                                 codes,
                                 childCodes,
                                 filter = TRUE,
                                 output = NULL,
                                 filenameRegex = ".*",
                                 outputPrefix = "",
                                 outputSuffix = "_rcAdded",
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
        childCodes = childCodes,
        filter = filter,
        func = changeSource_addChildCodes,
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

changeSource_addChildCodes <- function(input,
                                       codes,
                                       childCodes,
                                       filter,
                                       silent = rock::opts$get('silent')) {

  if (is.null(names(childCodes))) {
    stop("The `childCodes` argument has to be a named list. ",
         "Each element should be a filtering criterion that can be passed ",
         "to rock::get_source_filter(), and every element's name the child ",
         "code to be added when that filtering criterion matches, for ",
         "example:\n\n",
         "childCodes = list(newChild1 = 'positive', newChild2 = 'negative')");
  } else {

    ### Check whether any are filters already
    alreadyFilters <-
      unlist(
        lapply(
          childCodes,
          is_source_filter
        )
      );
    if (any(alreadyFilters)) {
      ### If so, get indices
      alreadyFilters <- which(alreadyFilters);
      ### And then check whether they have the correct length
      for (i in alreadyFilters) {
        if (length(childCodes[[alreadyFilters]]) != length(input)) {
          stop("As argument `childCodes`, you passed ",
               length(alreadyFilters), " filters as produced by ",
               "rock::get_source_filter(). However, of those, at least ",
               "one, specifically the one named '", names(childCodes)[i],
               "', is not the same length as the source (",
               length(childCodes[[alreadyFilters]]), " versus ",
               length(input), ", respectively). This can happen, for ",
               "example, if you tried to add child codes in multiple ",
               "sources with one command. If you do that, you can't pass ",
               "a source filter (as produced by a call to ",
               "rock::get_source_filter()), because source filters ",
               "correspond to the source for which they were constructed (and ",
               "so are always equally long). Instead, if you want to add ",
               "child codes in multiple sources with one command, you ",
               "have to pass a text string to search for, or a regular ",
               "expression to match against the utterances in the source, ",
               "in `childCodes`.");
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
      childCodes[[i]] <-
        get_source_filter(source = input,
                          filter = childCodes[[i]]);
    }

  }


  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  if (length(codes) > 1) {

    stop("You can only add child codes to one code at a time!");

  } else {
    ### `codes` has length 1

    ### For counting number of changes later on
    oldInput <- input;

    ### Filter for utterances that haven't been changed yet
    unprocessedUtterances <- rep(TRUE, length(input));

    ### Get clean code, removing any delimiters if they were added
    cleanOldCode <- cleanCode(codes);
    cleanNewCodes <-
      paste0(
        cleanOldCode,
        inductiveCodingHierarchyMarker,
        cleanCode(names(childCodes))
      );
    names(cleanNewCodes) <-
      names(childCodes);
    fullNewCodes <-
      paste0(
        codeDelimiters[1],
        cleanNewCodes,
        codeDelimiters[2]
      );
    names(fullNewCodes) <-
      names(childCodes);

    msg("To those occurrences of code '",
        cleanOldCode,
        "' that match the respective filters, adding child codes ",
        vecTxtQ(cleanNewCodes), ".\n",
        silent=silent);

    ### Create the regular expressions to change the code
    regexToFindCode <-
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

    msg("Using regular expression '", regexToFindCode, "'.",
        silent=silent);

    for (currentChildCode in names(childCodes)) {

      ### Select utterances to check (filter 'supercedes' the split filters
      ### so is always required)
      filteredUtterances <-
        #input[filter & unprocessedUtterances & childCodes[[currentChildCode]]];
        filter & unprocessedUtterances & childCodes[[currentChildCode]];

      ### Check which utterances are coded with the code to split
      utterancesMatchingRegex <-
        grepl(regexToFindCode, input);

      utterancesWithMatches <-
        filteredUtterances & utterancesMatchingRegex;

      msg("\n\nOut of the ", length(input), " utterances in the provided source, ",
          sum(filteredUtterances),
          " match both the general filter and the filter specified for child ",
          "code '",
          currentChildCode, "' (and have not been processed in a previous step). ",
          "Of those, ",
          sum(utterancesWithMatches), " have been coded with ",
            "code '", cleanOldCode,
          "', so to those utterances, child code '",
          fullNewCodes[currentChildCode], "' will now be appended.\n",
          silent=silent);

      input[utterancesWithMatches] <-
      #filteredUtterances[utterancesWithMatches] <-
        paste0(
          input[utterancesWithMatches],
          " ",
          fullNewCodes[currentChildCode]
        );

      ### Replace processed rows in the input source
      # input[
      #   filter &
      #     unprocessedUtterances &
      #     childCodes[[currentChildCode]]
      # ] <- filteredUtterances;

      ### Indicate that these were processed
      unprocessedUtterances[utterancesWithMatches] <- FALSE;

      msg(
        paste0(
          paste0("--PROCESSED: ", input[utterancesWithMatches]),
          collapse = "\n"
        ),
        silent=silent
      );

    }

    ### Count nr of changes
    diffCount <- sum(input != oldInput);

    msg("\n\nAdded child codes ", vecTxtQ(fullNewCodes), " to ", diffCount,
        " utterances that were coded with code '",
        cleanOldCode, "'.\n\n",
        silent=silent);

  }

  return(input);

}
