#' Parsing sources
#'
#' These function parse one (`parse_source`) or more (`parse_sources`) sources and the
#' contained identifiers, sections, and codes.
#'
#' @param text,file As `text` or `file`, you can specify a `file` to read with
#' encoding `encoding`, which will then be read using [base::readLines()]. If the
#' argument is named `text`, whether it is the path to an existing file is checked
#' first, and if it is, that file is read. If the argument is named `file`, and it
#' does not point to an existing file, an error is produced (useful if calling
#' from other functions). A `text` should be a character vector where every
#' element is a line of the original source (like provided by [base::readLines()]);
#' although if a character vector of one element *and* including at least one
#' newline character (`\\n`) is provided as `text`, it is split at the newline
#' characters using [base::strsplit()]. Basically, this behavior means that the
#' first argument can be either a character vector or the path to a file; and if
#' you're specifying a file and you want to be certain that an error is thrown if
#' it doesn't exist, make sure to name it `file`.
#' @param utteranceLabelRegexes Optionally, a list with two-element vectors
#' to preprocess utterances before they are stored as labels (these 'utterance
#' perl regular expression!
#' @param path The path containing the files to read.
#' @param extension The extension of the files to read; files with other extensions will
#' be ignored. Multiple extensions can be separated by a pipe (`|`).
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param regex Instead of specifing an extension, it's also possible to specify a regular
#' expression; only files matching this regular expression are read. If specified, `regex`
#' takes precedece over `extension`,
#' @param recursive Whether to also process subdirectories (`TRUE`)
#' or not (`FALSE`).
#' @param ignoreOddDelimiters If an odd number of YAML delimiters is encountered, whether this
#' should result in an error (`FALSE`) or just be silently ignored (`TRUE`).
#' @param checkClassInstanceIds Whether to check for the occurrence of class
#' instance identifiers specified in the attributes.
#' @param encoding The encoding of the file to read (in `file`).
#' @param postponeDeductiveTreeBuilding Whether to imediately try to build the deductive
#' tree(s) based on the information in this file (`FALSE`) or whether to skip that. Skipping
#' this is useful if the full tree information is distributed over multiple files (in which case
#' you should probably call `parse_sources` instead of `parse_source`).
#' @param mergeInductiveTrees Merge multiple inductive code trees into one; this
#' functionality is currently not yet implemented.
#' @param removeSectionBreakRows,removeIdentifierRows,removeEmptyRows Whether to
#' remove from the QDT, respectively: rows containing section breaks; rows
#' containing only (class instance) identifiers; and empty rows.
#' @param filesWithYAML Any additional files to process to look for YAML fragments.
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#' @param x The object to print.
#' @param prefix The prefix to use before the 'headings' of the printed result.
#' @param ... Any additional arguments are passed on to the default print method.
#'
#' @rdname parsing_sources
#' @aliases parsing_sources parse_source parse_sources print.rock_parsedSource
#'
#' @return For `rock::parse_source()`, an object of class `rock_parsedSource`;
#' for `rock::parse_sources()`, an object of class `rock_parsedSources`. These
#' objects contain the original source(s) as well as the final data frame with
#' utterances and codes, as well as the code structures.
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Show inductive code tree for the codes
#' ### extracted with the regular expression specified with
#' ### the name 'codes':
#' parsedExample$inductiveCodeTrees$codes;
#'
#' ### If you want `rock` to be chatty, use:
#' parsedExample <- rock::parse_source(exampleFile,
#'                                     silent=FALSE);
#'
#' ### Parse as selection of example sources in that directory
#' parsedExamples <-
#'   rock::parse_sources(
#'     examplePath,
#'     regex = "(test|example)(.txt|.rock)"
#'   );
#'
#' ### Show combined inductive code tree for the codes
#' ### extracted with the regular expression specified with
#' ### the name 'codes':
#' parsedExamples$inductiveCodeTrees$codes;
#'
#' ### Show a souce coded with the Qualitative Network Approach
#' qnaExample <-
#'   rock::parse_source(
#'     file.path(
#'       examplePath,
#'       "network-example-1.rock"
#'     )
#'   );
#'
#' @export
parse_source <- function(text,
                         file,
                         utteranceLabelRegexes = NULL,
                         ignoreOddDelimiters=FALSE,
                         checkClassInstanceIds = rock::opts$get(checkClassInstanceIds),
                         postponeDeductiveTreeBuilding = FALSE,
                         filesWithYAML = NULL,
                         removeSectionBreakRows = rock::opts$get('removeSectionBreakRows'),
                         removeIdentifierRows = rock::opts$get('removeIdentifierRows'),
                         removeEmptyRows = rock::opts$get('removeEmptyRows'),
                         rlWarn = rock::opts$get('rlWarn'),
                         encoding=rock::opts$get('encoding'),
                         silent=rock::opts$get('silent')) {

  codeRegexes <- rock::opts$get('codeRegexes');
  idRegexes <- rock::opts$get('idRegexes');
  ciid_columnsToCopy <- rock::opts$get('ciid_columnsToCopy');
  anchorRegex <- rock::opts$get('anchorRegex');
  anchorsCol <- rock::opts$get('anchorsCol');
  classInstanceRegex <- rock::opts$get('classInstanceRegex');
  codeValueRegexes <- rock::opts$get('codeValueRegexes');
  sectionRegexes <- rock::opts$get('sectionRegexes');
  uidRegex <- rock::opts$get('uidRegex');
  autoGenerateIds <- rock::opts$get('autoGenerateIds');
  ### Obsolete now all class instance identifiers are persistent
  # persistentIds <- rock::opts$get(persistentIds);
  noCodes <- rock::opts$get('noCodes');
  inductiveCodingHierarchyMarker <- rock::opts$get('inductiveCodingHierarchyMarker');
  attributeContainers <- rock::opts$get('attributeContainers');
  networkContainers <- rock::opts$get('networkContainers');
  aestheticContainers <- rock::opts$get('aestheticContainers');
  codesContainers <- rock::opts$get('codesContainers');
  sectionBreakContainers <- rock::opts$get('sectionBreakContainers');
  delimiterRegEx <- rock::opts$get('delimiterRegEx');
  ignoreRegex <- rock::opts$get('ignoreRegex');
  nestingMarker <- rock::opts$get('nestingMarker');
  diagrammerSanitizing <- rock::opts$get('diagrammerSanitizing');
  networkCodeRegexes <- rock::opts$get('networkCodeRegexes');
  networkCodeRegexOrder <- rock::opts$get('networkCodeRegexOrder');
  networkEdgeWeights <- rock::opts$get('networkEdgeWeights');
  networkDefaultEdgeWeight <- rock::opts$get('networkDefaultEdgeWeight');
  networkCodeCleaningRegexes <- rock::opts$get('networkCodeCleaningRegexes');
  networkCollapseEdges <- rock::opts$get('networkCollapseEdges');
  theme_networkDiagram <- rock::opts$get('theme_networkDiagram');
  theme_utteranceDiagram <- rock::opts$get('theme_utteranceDiagram');

  ###---------------------------------------------------------------------------

  if (missing(file)) {
    if (missing(text)) {
      stop("Provide either a `file` or a `text` to scan!");
    } else {
      if ((length(text) == 1) && file.exists(text)) {
        x <- readLines(text,
                       encoding=encoding,
                       warn=rlWarn);
        if (!silent) {
          cat0("Read the contents of file '", text, "' (", length(x), " lines read).\n");
        }
      } else {
        x <- text;
        if ((length(x) == 1) && grepl('\n', x)) {
          x <-
            strsplit(x,
                     "\n")[[1]];
        }
        if (!silent) {
          cat0("Read input string (", length(x), " lines read).\n\n");
        }
      }
    }
  } else {
    if (file.exists(file)) {
      x <- readLines(file,
                     encoding=encoding,
                     warn=rlWarn);
      if (!silent) {
        cat0("Read the contents of file '", file, "' (", length(x), " lines read).\n");
      }
    } else {
      stop("The file you specified in argument `file` ('",
           paste0(file, collapse=" "),
           "') does not exist. If you meant to provide a text ",
           "to process, please use argument `text`");
    }
  }

  ### Store results in the object to return
  res <-
    structure(list(arguments = as.list(environment())),
              class="rock_parsedSource");

  ###---------------------------------------------------------------------------

  ### First process YAML fragments and remove them
  res$yamlFragments <-
    yum::extract_yaml_fragments(text=x,
                                delimiterRegEx=delimiterRegEx,
                                ignoreOddDelimiters=ignoreOddDelimiters,
                                encoding=encoding,
                                silent=silent);
  x <-
    yum::delete_yaml_fragments(text=x,
                               delimiterRegEx=delimiterRegEx,
                               ignoreOddDelimiters=ignoreOddDelimiters,
                               silent=silent);

  if (!is.null(filesWithYAML)) {
    for (currentYAMLfile in filesWithYAML) {
      if (file.exists(currentYAMLfile)) {
        res$yamlFragments <-
          structure(
            c(
              res$yamlFragments,
              yum::extract_yaml_fragments(
                file=currentYAMLfile,
                delimiterRegEx=delimiterRegEx,
                ignoreOddDelimiters=ignoreOddDelimiters,
                encoding=encoding,
                silent=silent
              )
            ),
            class = "yamlFragments"
          );
      }
    }
  }

  ### Process attributes and deductive code trees
  if (!is.null(res$yamlFragments)) {
    msg(
      "Encountered YAML fragments. Parsing them for attributes.\n",
      silent = silent
    );
    res$attributes <-
      yum::load_and_simplify(yamlFragments=res$yamlFragments,
                             select=paste0(attributeContainers, collapse="|"));
    msg(
      "Read ", length(unlist(res$attributes)),
      " attributes specifications. Continuing with deductive code trees.\n",
      silent = silent
    );

    res$rawDeductiveCodes <-
      yum::load_and_simplify(yamlFragments=res$yamlFragments,
                             select=paste0(codesContainers, collapse="|"));

    # ### Store data frame with all info about deductive codes
    # res$deductiveCodeDfs <-
    #   lapply(
    #     parsedSource$rawDeductiveCodes,
    #     get_dataframe_from_nested_list
    #   );
    #
    # names(res$deductiveCodeDfs) <-
    #   names(res$rawDeductiveCodes);

    ### Get all deductive code ids
    res$deductiveCodes <-
      get_deductive_code_values(res$rawDeductiveCodes,
                                name="id");
    ### Remove empty codes
    res$deductiveCodes <-
      res$deductiveCodes[nchar(res$deductiveCodes)>0];
    msg(
      "Read ", length(res$deductiveCodes),
      " deductive codes (",
      vecTxtQ(res$deductiveCodes), ").\n"
    );

    ### Store tree, unless we should postpone that
    if (!postponeDeductiveTreeBuilding) {
      ### Build tree
      deductiveCodeTrees <-
        yum::build_tree(res$rawDeductiveCodes);
      deductiveCodeTrees$name <- 'codes';
      res$deductiveCodeTrees <-
        deductiveCodeTrees;
    } else {
      deductiveCodeTrees <- NA;
    }
    res$sectionBreakRegexes <-
      yum::load_and_simplify(yamlFragments=res$yamlFragments,
                             select=paste0(sectionBreakContainers, collapse="|"));
    msg(
      "Read ", length(unlist(res$sectionBreakRegexes)),
      " section break codes.\n"
    );

    msg(
      "Looking for network configuration.\n",
      silent = silent
    );
    res$aestheticConfig <-
      yum::load_and_simplify(yamlFragments=res$yamlFragments,
                             select=paste0(aestheticContainers, collapse="|"));
    msg(
      "Read ", length(unlist(res$aestheticConfig)),
      " aesthetic specifications.\n",
      silent = silent
    );

    res$aestheticsTheme <-
      aesthetics_to_graph_theme(res$aestheticConfig);

    res$aestheticRegexes <-
      aesthetics_to_regexIndexed_list(res$aestheticConfig);

    msg(
      "Done parsing YAML fragments.\n",
      silent = silent
    );

  } else {

    msg(
      "No YAML fragments encountered.\n",
      silent = silent
    );

    res$attributes <- NA;
    res$deductiveCodes <- NA;
    res$deductiveCodeTrees <- NA;
    res$sectionBreakRegexes <- NA;
    res$aestheticConfig <- NA;
  }

  ###---------------------------------------------------------------------------

  if ((length(res$attributes) > 0) && (!all(is.na(unlist(res$attributes))))) {

    ### Simplify YAML attributes and convert into a data frame
    res$attributesDf <-
      do.call(rbind,
              lapply(res$attributes,
                     as.data.frame,
                     stringsAsFactors=FALSE));

    ### Store attributes variables for convenient use later on
    res$convenience <-
      list(attributesVars =
             setdiff(names(res$attributesDf),
                     c(names(idRegexes),
                       names(attributeContainers))));

  } else {

    res$attributesDf <- data.frame();

    res$convenience <-
      list(attributesVars = NULL);

  }

  ###---------------------------------------------------------------------------

  ### Then remove lines to ignore
  linesToIgnore <- grepl(ignoreRegex,
                         x);
  ignoredLines <- x[linesToIgnore];
  x <- x[!linesToIgnore];

  ### Create dataframe for parsing
  sourceDf <- data.frame(utterances_raw = x,
                         stringsAsFactors=FALSE);

  ###---------------------------------------------------------------------------

  ### Identify sections
  if (!is.null(sectionRegexes) && length(sectionRegexes) > 0) {
    for (sectionRegex in names(sectionRegexes)) {

      ### Store whether each utterance matches
      sourceDf[, glue::glue("{sectionRegex}_match")] <-
        grepl(sectionRegexes[sectionRegex], x);

      ### Store value of match
      sourceDf[, glue::glue("{sectionRegex}_content")] <-
        ifelse(
          sourceDf[, glue::glue("{sectionRegex}_match")],
          gsub(
            paste0(".*(", sectionRegexes[sectionRegex], ").*"),
            "\\1",
            x
          ),
          ""
        );

      matched_sectionRegex_substrings <-
        unique(
          gsub(
            sectionRegexes[sectionRegex],
            "\\1",
            sourceDf[, glue::glue("{sectionRegex}_content")])
        );
      matched_sectionRegex_substrings <-
        matched_sectionRegex_substrings[
          nchar(matched_sectionRegex_substrings) > 0
        ];

      ### Set incremental counter for each match

      if (length(matched_sectionRegex_substrings) > 0) {

        ### If we could extract a substring, we distinguish section breaks
        ### based on this substring.

        for (current_sectionRegex_substring in matched_sectionRegex_substrings) {

          colNamePrefix <-
            glue::glue(
              "{sectionRegex}_{current_sectionRegex_substring}"
            );

          sourceDf[, glue::glue("{colNamePrefix}_match")] <-
            grepl(
              current_sectionRegex_substring,
              sourceDf[, glue::glue("{sectionRegex}_content")]
            );

          if (glue::glue("{colNamePrefix}_match") %in% names(sourceDf) &&
              (length(sourceDf[, glue::glue("{colNamePrefix}_match")]) > 0)) {
            sourceDf[, glue::glue("{colNamePrefix}_counter")] <-
              purrr::accumulate(sourceDf[, glue::glue("{colNamePrefix}_match")],
                                `+`);
          }

        }

      } else {

        ### Otherwise, just use this regex only
        if (glue::glue("{sectionRegex}_match") %in% names(sourceDf) &&
            (length(sourceDf[, glue::glue("{sectionRegex}_match")]) > 0)) {
          sourceDf[, glue::glue("{sectionRegex}_counter")] <-
            purrr::accumulate(sourceDf[, glue::glue("{sectionRegex}_match")],
                              `+`);
        }

      }

    }
  }

  ###---------------------------------------------------------------------------
  ### Process specified class instance identifiers
  ###---------------------------------------------------------------------------

  # if (!is.null(idRegexes) && length(idRegexes) > 0) {
  #   for (idRegex in names(idRegexes)) {
  #
  #     ### Get a list of matches
  #     ids <-
  #       regmatches(x,
  #                  gregexpr(idRegexes[idRegex], x));
  #
  #     ### Check whether there are multiple matches
  #     multipleIds <-
  #       which(unlist(lapply(ids, length))>1);
  #     if (length(multipleIds) > 0) {
  #       warning(glue::glue("Multiple class instance identifiers matching '{idRegex}' found in the following utterances:\n",
  #                      paste0(x[multipleIds],
  #                             collapse="\n"),
  #                      "\n\nOnly using the first  class instanceidentifier for each utterance, removing and ignoring the rest!"));
  #       ids <-
  #         lapply(ids, utils::head, 1);
  #     }
  #
  #     ### Clean identifiers (i.e. only retain identifier content itself)
  #     ids <-
  #       lapply(ids, gsub, pattern=idRegexes[idRegex], replacement="\\1");
  #
  #     ### Set "no_id" for utterances without id
  #     ids <-
  #       ifelse(unlist(lapply(ids,
  #                            length)),
  #              ids,
  #              "no_id");
  #
  #     ### Convert from a list to a vector
  #     ids <- unlist(ids);
  #
  #     if (length(ids) > 1) {
  #
  #       ### Implement 'identifier persistence' by copying the
  #       ### identifier of the previous utterance if the identifier
  #       ### is not set - can't be done using vectorization as identifiers
  #       ### have to carry over sequentially.
  #       # if (idRegex %in% persistentIds) {
  #       #   rawIds <- ids;
  #       #   for (i in 2:length(ids)) {
  #       #     if ((ids[i] == "no_id")) {
  #       #       ids[i] <- ids[i-1];
  #       #     }
  #       #   }
  #       # }
  #
  #       ### 2022-10-07 --- Decision Szilvia & GJ: change ROCK standard such
  #       ###                that all class instance identifiers are always
  #       ###                persistent.
  #       rawIds <- ids;
  #       for (i in 2:length(ids)) {
  #         if ((ids[i] == "no_id")) {
  #           ids[i] <- ids[i-1];
  #         }
  #       }
  #
  #     } else {
  #       ids = "no_id";
  #     }
  #
  #     ### Check whether any matches were found
  #     if (!(all(ids=="no_id"))) {
  #       ### Generate identifiers for ids without identifier
  #       if (idRegex %in% autoGenerateIds) {
  #         ids[ids=="no_id"] <-
  #           paste0("autogenerated_id_",
  #                  1:(sum(ids=="no_id")));
  #       }
  #       ### Store identifiers in sourceDf
  #       sourceDf[, idRegex] <-
  #         ids;
  #       # if (idRegex %in% persistentIds) {
  #       #   sourceDf[, paste0(idRegex, "_raw")] <-
  #       #     rawIds;
  #       # }
  #       ### 2022-10-07 --- Decision Szilvia & GJ: change ROCK standard such
  #       ###                that all class instance identifiers are always
  #       ###                persistent.
  #       sourceDf[, paste0(idRegex, "_raw")] <-
  #         rawIds;
  #
  #     }
  #   }
  # }

  ###---------------------------------------------------------------------------
  ### Process unspecified (generic) class instance identifiers
  ###---------------------------------------------------------------------------

  classIdMatches <-
    regmatches(x,
               gregexpr(classInstanceRegex,
                        x,
                        perl = TRUE));

  if (all(unlist(lapply(classIdMatches, length)) == 0)) {

    msg(
      "No UIDs or class instance identifiers found.\n",
      silent = silent
    );

    unspecifiedClasses <- NULL;
    specifiedClasses <- NULL;
    allClasses <- NULL;

  } else {

    msg(
      "Found UIDS or class instance identifiers: commencing to process.\n",
      silent = silent
    );

    ### Get matches with the class instance identifier pattern

    classIdMatches <-
      lapply(
        classIdMatches,
        function(x) {
          return(list(
            gsub(classInstanceRegex, "\\1", x, perl = TRUE),
            gsub(classInstanceRegex, "\\2", x, perl = TRUE)
          ));
        }
      );

    ### Convert the object for each line into a vector

    namedClassIdMatches <-
      lapply(
        classIdMatches,
        function(x) {
          return(
            stats::setNames(
              x[[2]],
              x[[1]]
            )
          );
        }
      );

    ### Get a list of all classes we have

    unspecifiedClasses <-
      unique(
        unlist(
          lapply(
            classIdMatches,
            function(x) {
              if (length(x) == 0) {
                return(NULL)
              } else {
                return(x[[1]]);
              }
            }
          )
        )
      );

    unspecifiedClasses <- setdiff(unspecifiedClasses, c("uid"));

    if (length(unspecifiedClasses) == 0) {

      msg(
        "No generic class instance identifiers found.\n",
        silent = silent
      );

    } else {

      msg(
        "Found class instance identifiers (for classes with identifiers ",
        vecTxtQ(unspecifiedClasses), "): commencing to process.\n",
        silent = silent
      );

      ### Convert all the vectors into single-row data frames

      unspecifiedClassInstanceIdentifierList_raw <-
        lapply(
          namedClassIdMatches,
          function(x) {
            if ((length(x) == 0) || (all(tolower(names(x)) == "uid"))) {
              return(
                stats::setNames(
                  data.frame(t(rep(NA, length(unspecifiedClasses)))),
                  unspecifiedClasses
                )
              );
            } else {
              cols <- names(x)[names(x) %in% unspecifiedClasses];
              return(as.data.frame(as.list(x))[, cols, drop=FALSE]);
            }
          }
        );

      ### rbind() all these single rows into one data frame

      unspecifiedClassInstanceIdentifierDf_raw <-
        tryCatch(
          rbind_df_list(
            unspecifiedClassInstanceIdentifierList_raw
          )
        , error = function(e) {

          ### Sometimes there's a C stack error that apparently has to
          ### do with recursion. If there are many lines, rbind_df_list()
          ### calls itself lots of times, so then in this case we can
          ### simplify matters since we know the columns we'll need
          ### (those are stored in unspecifiedClasses).

          res <-
            lapply(
              unspecifiedClassInstanceIdentifierList_raw,
              function(x) {
                colsToAdd <-
                  unspecifiedClasses[!(unspecifiedClasses %in% names(x))];
                x[, colsToAdd] <- NA;
                return(x[, unspecifiedClasses]);
              }
            );

          return(do.call(rbind, res));

        }
      );

      unspecifiedClassInstanceIdentifierDf <-
        lapply(
          unspecifiedClassInstanceIdentifierDf_raw,
          function(x) {
            x <-
              ifelse(is.na(x) | (nchar(x) == 0),
                     "no_id",
                     x);
            for (i in 2:length(x)) {
              if ((x[i] == "no_id")) {
                x[i] <- x[i-1];
              }
            }
            return(x);
          }
        );

      if ((!(all(unspecifiedClasses %in%
             names(unspecifiedClassInstanceIdentifierDf))) ||
          (!(all(names(unspecifiedClassInstanceIdentifierDf) %in%
             unspecifiedClasses))))) {
        stop("Inconsistency in column names");
      }

      sourceDf[, unspecifiedClasses] <-
        unspecifiedClassInstanceIdentifierDf;
      sourceDf[, paste0(unspecifiedClasses, "_raw")] <-
        unspecifiedClassInstanceIdentifierDf_raw;

      colsToCopy <- names(ciid_columnsToCopy) %in% unspecifiedClasses;

      if (length(colsToCopy) > 0) {
        colsToCopy <- ciid_columnsToCopy[colsToCopy];
        sourceDf[, colsToCopy] <-
          sourceDf[, names(colsToCopy)];
        specifiedClasses <-
          res$convenience$specifiedClasses <- colsToCopy;
      } else {
        specifiedClasses <-
          res$convenience$specifiedClasses <- NULL;
      }

      res$convenience$unspecifiedClasses <-
        unspecifiedClasses;

      allClasses <-
        res$convenience$allClasses <-
        c(specifiedClasses, unspecifiedClasses);

      msg(
        "Processed ", length(unspecifiedClasses),
        " class instance identifiers (", vecTxtQ(unspecifiedClasses), ").\n",
        silent = silent
      );

    }

  }

  ###---------------------------------------------------------------------------
  ### Delete identifiers and store clean version in sourceDf
  ###---------------------------------------------------------------------------

  x <-
    gsub(paste0(idRegexes, collapse="|"),
         "",
         x,
         perl = TRUE);
  x <-
    gsub(classInstanceRegex,
         "",
         x,
         perl = TRUE);

  sourceDf$utterances_without_identifiers <- x;

  ###---------------------------------------------------------------------------
  ### Anchors
  ###---------------------------------------------------------------------------

  ### Get a list of matches
  anchors <-
    unlist(
      lapply(
        regmatches(x,
                   gregexpr(anchorRegex, x)),
        function(x) {
          if (length(x) == 0) {
            return("");
          } else {
            return(x);
          }
        }
      )
    );

  sourceDf[, paste0(anchorsCol, "_raw")] <-
    anchors;

  sourceDf[, anchorsCol] <-
    trimws(
      gsub(anchorRegex,
           "\\1",
           anchors)
    );

  ###---------------------------------------------------------------------------
  ### Process codes
  ###---------------------------------------------------------------------------

  codings <- list();
  codeProcessing <- list();
  occurrences <- list();
  occurrenceCounts <- list();
  namedOccurrences <- list();

  ### Process codes
  if (!is.null(codeRegexes) && length(codeRegexes) > 0) {

    for (codeRegex in names(codeRegexes)) {

      ### Find matches (the full substrings that match this code in each line)
      matches <-
        regmatches(x,
                   gregexpr(codeRegexes[codeRegex], x));

      ### Retain only the 'parenthesized' expression (i.e. the part of
      ### this code's regex between the parentheses, i.e., the actual code itself)
      cleanedMatches <-
        lapply(matches, gsub, pattern=codeRegexes[codeRegex], replacement="\\1");

      ### Remove all codes matching the 'noCodes' argument
      cleanedMatches <-
        lapply(cleanedMatches, grep, pattern=noCodes, value=TRUE, invert=TRUE);

      ### Get a complete list of all used codes. Note that this list can still contain
      ### duplicate leaves, because the ancestors are included here as well.
      codings[[codeRegex]] <-
        sort(unique(unlist(cleanedMatches)));

      ### Split these unique codes (but potentially containing duplicates given the
      ### inclusion of ancestors) into levels in case inductive coding was applied
      if ((nchar(inductiveCodingHierarchyMarker) > 0) &&
          (!is.null(codings[[codeRegex]])) &&
          (length(codings[[codeRegex]]) > 0)) {

        ### Create an object with the intermediate objects
        codeProcessing[[codeRegex]] <- list(matches = matches);

        ### Split the codes using the specified marker
        codeProcessing[[codeRegex]]$splitCodings <-
          strsplit(codings[[codeRegex]],
                   inductiveCodingHierarchyMarker);

        ### Make a specific vector with the leaves
        codeProcessing[[codeRegex]]$leafCodes <-
          unlist(lapply(codeProcessing[[codeRegex]]$splitCodings,
                        utils::tail,
                        1));

        ### Remove duplicate elements from the list with leaves
        codeProcessing[[codeRegex]]$leafCodes <-
          sort(unique(codeProcessing[[codeRegex]]$leafCodes));

        ### Make a separate list containing only the inductive codes
        codeProcessing[[codeRegex]]$inductiveCodes <-
          lapply(codeProcessing[[codeRegex]]$splitCodings,
                 function(codeVector) {
                   ### Check which of the codes in this vector are
                   ### deductive codes
                   deductives <-
                     codeVector %in% res$deductiveCodes;

                   if (all(deductives)) {
                     ### If no inductive codes included in this vector, return NULL
                     return(NULL);
                   } else if (all(!(deductives))) {
                     ### If they're all inductive codes, return them all
                     return(codeVector);
                   } else {
                     ### If a part is inductive, strip the deductive codes
                     if (identical(rev(sort(deductives)), deductives)) {
                       ### So we have 1+ deductive codes. We need to retain the
                       ### last one, but can remove its ancestors.
                       inductives <- !deductives;
                       inductives[max(which(deductives))] <- TRUE;
                       return(codeVector[inductives]);
                     } else {
                       problemCode <-
                         paste0("[[",
                                paste0(codeVector,
                                       sep=inductiveCodingHierarchyMarker),
                                "]]");
                       stop("I encountered a code specification that includes both deductive ",
                            "and inductive codes (", problemCode,
                            ", occurring on lines ",
                            grep(problemCode, res$arguments$x), " of the input source), but the inductive codes ",
                            "are not all descendents of the deductive codes! Inductive coding ",
                            "can be combined with deductive coding, but only if the inductive ",
                            "codes further specify (i.e. are subcodes, or descendents, of) the ",
                            "deductive codes.");
                     }
                   }
                 });

        ### Remove elements that are NULL
        codeProcessing[[codeRegex]]$inductiveCodes <-
          codeProcessing[[codeRegex]]$inductiveCodes[!unlist(lapply(codeProcessing[[codeRegex]]$inductiveCodes,
                                                                    is.null))];

      } else {
        codeProcessing[[codeRegex]] <-
          list(splitCodings = lapply(codings[[codeRegex]],
                                     function(x) return(x)),
               leafCodes = codings[[codeRegex]],
               inductiveCodes = codings[[codeRegex]][!(codings[[codeRegex]] %in% res$deductiveCodes)]);
      }

      ### Get inductive leaf codes
      codeProcessing[[codeRegex]]$inductiveLeafCodes <-
        unlist(lapply(codeProcessing[[codeRegex]]$splitCodings,
                      utils::tail,
                      1));
      codeProcessing[[codeRegex]]$inductiveLeafCodes <-
        sort(unique(codeProcessing[[codeRegex]]$inductiveLeafCodes));

      ### If inductive coding was applied using a hierarchical structure,
      ### build the inductive code tree
      if ((nchar(inductiveCodingHierarchyMarker) > 0) &&
          (!is.null(codeProcessing[[codeRegex]]$inductiveCodes)) &&
          (length(codeProcessing[[codeRegex]]$inductiveCodes) > 0)) {

        ### Process inductive code trees
        codeProcessing[[codeRegex]]$inductiveCodeTrees <-
          inductiveCodes_to_tree(inductiveCodes=codeProcessing[[codeRegex]]$inductiveCodes,
                                 silent=silent);

        ### Set graph style
        data.tree::SetGraphStyle(codeProcessing[[codeRegex]]$inductiveCodeTrees,
                                 directed="false");
        data.tree::SetGraphStyle(codeProcessing[[codeRegex]]$inductiveCodeTrees,
                                 rankdir = "LR");

        ### Try to convert to a DiagrammeR graph
        tryCatch({
          codeProcessing[[codeRegex]]$inductiveDiagrammeR <-
            data.tree::ToDiagrammeRGraph(codeProcessing[[codeRegex]]$inductiveCodeTrees);
          codeProcessing[[codeRegex]]$inductiveDiagrammeR <-
            do.call(
              rock::apply_graph_theme,
              c(list(graph = codeProcessing[[codeRegex]]$inductiveDiagrammeR),
                rock::opts$get("theme_codeTreeDiagram"))
            );
        }, error = function(e) {
          warning("Error issued by 'data.tree::ToDiagrammeRGraph' when converting '",
                  codeRegex, "' code tree: ", e$message, "\n\nClass and content:\n\n",
                  paste0(utils::capture.output(print(class(codeProcessing[[codeRegex]]$inductiveCodeTrees))),
                         collapse="\n"),
                  "\n",
                  paste0(utils::capture.output(print(codeProcessing[[codeRegex]]$inductiveCodeTrees)),
                         collapse="\n"));
        });

      } ### Changed this on 2022-11-27

      ### Set matches for lines that did
      ### not have a match to NA
      cleanedMatches[unlist(lapply(matches, length))==0] <- NA;

      ### Get presence of codes in utterances
      occurrences[[codeRegex]] <-
        lapply(get_leaf_codes(cleanedMatches,
                              inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker),
               `%in%`,
               x=codeProcessing[[codeRegex]]$leafCodes);

      ### Convert from logical to numeric
      occurrenceCounts[[codeRegex]] <-
        lapply(occurrences[[codeRegex]], as.numeric);

      ### Add the codes as names
      namedOccurrences[[codeRegex]] <-
        lapply(occurrenceCounts[[codeRegex]],
               `names<-`,
               value <- codeProcessing[[codeRegex]]$leafCodes);

      ### Convert the lists to dataframes
      sourceDf <-
        cbind(sourceDf,
              as.data.frame(do.call(rbind,
                                    namedOccurrences[[codeRegex]])));

      ### Delete codes from utterances
      x <-
        gsub(codeRegexes[codeRegex],
             "",
             x);

      ### }
    }
  }

  ###---------------------------------------------------------------------------
  ### Process codeValues

  if (!is.null(codeValueRegexes) && length(codeValueRegexes) > 0) {

    for (codeValueRegex in names(codeValueRegexes)) {

      ### Find matches (the full substrings that match this code in each line)
      matches <-
        regmatches(x,
                   gregexpr(codeValueRegexes[codeValueRegex], x));

      ### Retain only the 'parenthesized' expression (i.e. the part of
      ### this code's regex between the parentheses, i.e., the actual code itself)
      cleanedCodeValueNames <-
        lapply(matches, gsub, pattern=codeValueRegexes[codeValueRegex],
               replacement="\\1");

      cleanedValues <-
        lapply(matches, gsub, pattern=codeValueRegexes[codeValueRegex],
               replacement="\\2");

      namedCodeValues <-
        mapply(
          stats::setNames,
          cleanedValues,
          cleanedCodeValueNames
        );

      allCodeValueNames <-
        sort(unlist(unique(cleanedCodeValueNames)));

      if (any(allCodeValueNames %in% names(sourceDf))) {
        stop("At least one of the codeValue names also occurs as regular code identifier!");
      }

      for (currentCodeValueName in allCodeValueNames) {
        sourceDf[[currentCodeValueName]] <-
          unlist(
            lapply(
              namedCodeValues,
              function(vector,
                       elementToReturn) {
                if (elementToReturn %in% names(vector)) {
                  return(vector[elementToReturn]);
                } else {
                  return(NA);
                }
              },
              elementToReturn = currentCodeValueName
            )
          );

      }

      ### Delete codes from utterances
      x <-
        gsub(codeValueRegexes[codeValueRegex],
             "",
             x);

    }
  }

  ###---------------------------------------------------------------------------
  ### Process network code identifiers

  if (!is.null(networkCodeRegexes) && length(networkCodeRegexes) > 0) {

    res$networkCodes <- list();

    for (networkCodeRegex in names(networkCodeRegexes)) {

      res$networkCodes[[networkCodeRegex]] <-
        list(coded = data.frame());

      ### Find matches (the full substrings that match this code in each line)
      matches <-
        regmatches(x,
                   gregexpr(networkCodeRegexes[networkCodeRegex], x));

      res$networkCodes[[networkCodeRegex]]$matches <- matches;

      if (length(matches) > 0) {

        ### Cycle through the four elements; then through the matches;
        ### then through the elments of each match (in case there are
        ### more than one).
        res$networkCodes[[networkCodeRegex]]$coded_list <-
          lapply(
            c(from = "from", to = "to", type = "type", weight = "weight"),
            function(col) {
              return(
                do.call(
                  rbind,
                  lapply(
                    seq_along(matches),
                    function(originalSequenceNr) {
                      selection <-
                        unlist(
                          lapply(
                            matches[[originalSequenceNr]],
                            function(match) {
                              return(
                                gsub(
                                  networkCodeRegexes[networkCodeRegex],
                                  paste0("\\", which(networkCodeRegexOrder==col)),
                                  match
                                )
                              );
                            }
                          )
                        );
                      if (is.null(selection)) {
                        selection <-
                          data.frame(
                            NA,
                            originalSequenceNr
                          );
                      } else {
                        selection <-
                          data.frame(
                            gsub(
                              networkCodeCleaningRegexes[1],
                              networkCodeCleaningRegexes[2],
                              selection
                            ),
                            rep(originalSequenceNr, length(selection))
                          );
                      }
                      names(selection) <- c(col, "originalSequenceNr");
                      return(selection);
                    }
                  )
                )
              );
            }
          );

        res$networkCodes[[networkCodeRegex]]$coded_df_full <-
          data.frame(
            from = res$networkCodes[[networkCodeRegex]]$coded_list$from$from,
            to = res$networkCodes[[networkCodeRegex]]$coded_list$to$to,
            type = res$networkCodes[[networkCodeRegex]]$coded_list$type$type,
            weight = res$networkCodes[[networkCodeRegex]]$coded_list$weight$weight
          );

        res$networkCodes[[networkCodeRegex]]$coded_df_full$frequency <-
          apply(
            res$networkCodes[[networkCodeRegex]]$coded_df_full,
            1,
            function(row) {
              if (all(is.na(row))) {
                return(0);
              } else {
                return(
                  nrow(
                    res$networkCodes[[networkCodeRegex]]$coded_df_full[
                      (res$networkCodes[[networkCodeRegex]]$coded_df_full$from %in% row['from']) &
                        (res$networkCodes[[networkCodeRegex]]$coded_df_full$to %in% row['to']) &
                        (res$networkCodes[[networkCodeRegex]]$coded_df_full$type %in% row['type']),
                    ]
                  )
                );
              }
            }
          );

        res$networkCodes[[networkCodeRegex]]$coded_df <-
          res$networkCodes[[networkCodeRegex]]$coded_df_full[
            (!is.na(res$networkCodes[[networkCodeRegex]]$coded_df_full$from)) &
              (!is.na(res$networkCodes[[networkCodeRegex]]$coded_df_full$to)),
          ];

        ### Set edge weights
        if (networkEdgeWeights == "manual") {
          res$networkCodes[[networkCodeRegex]]$coded_df$edge_weight <-
            ifelse(
              is.na(res$networkCodes[[networkCodeRegex]]$coded_df$weight) |
                (nchar(res$networkCodes[[networkCodeRegex]]$coded_df$weight) == 0),
              networkDefaultEdgeWeight,
              res$networkCodes[[networkCodeRegex]]$coded_df$weight
            );
        } else if (networkEdgeWeights == "frequency") {
          res$networkCodes[[networkCodeRegex]]$coded_df$edge_weight <-
            res$networkCodes[[networkCodeRegex]]$coded_df$frequency
        }

        if (networkCollapseEdges) {
          res$networkCodes[[networkCodeRegex]]$coded_df <-
            unique(res$networkCodes[[networkCodeRegex]]$coded_df);
        }

        ### Delete codes from utterances
        x <-
          gsub(networkCodeRegexes[networkCodeRegex],
               "",
               x);

        res$networkCodes[[networkCodeRegex]]$nodeList <-
          unique(
            c(res$networkCodes[[networkCodeRegex]]$coded_df$to,
              res$networkCodes[[networkCodeRegex]]$coded_df$from)
          );

        if (length(res$networkCodes[[networkCodeRegex]]$nodeList) > 0) {

          ### Build DiagrammeR's node_df and edge_df
          res$networkCodes[[networkCodeRegex]]$node_df <-
            DiagrammeR::create_node_df(
              n = length(res$networkCodes[[networkCodeRegex]]$nodeList),
              label = res$networkCodes[[networkCodeRegex]]$nodeList,
            );

          res$networkCodes[[networkCodeRegex]]$label_to_id <-
            stats::setNames(
              seq_along(res$networkCodes[[networkCodeRegex]]$nodeList),
              nm = res$networkCodes[[networkCodeRegex]]$nodeList
            );

          res$networkCodes[[networkCodeRegex]]$node_df <-
            DiagrammeR::create_node_df(
              n = length(res$networkCodes[[networkCodeRegex]]$nodeList),
              label = res$networkCodes[[networkCodeRegex]]$nodeList,
              type = networkCodeRegex
            );

          ### Prepare a list to provide to do.call when creating the edge_df
          list_for_edf <-
            list(
              from =
                res$networkCodes[[networkCodeRegex]]$label_to_id[
                  res$networkCodes[[networkCodeRegex]]$coded_df$from
                ],
              to =
                res$networkCodes[[networkCodeRegex]]$label_to_id[
                  res$networkCodes[[networkCodeRegex]]$coded_df$to
                ],
              rel = res$networkCodes[[networkCodeRegex]]$coded_df$type,
              penwidth = res$networkCodes[[networkCodeRegex]]$coded_df$edge_weight
            );

          if (!is.na(res$aestheticConfig)) {

            configName <- paste0("ROCK_", networkCodeRegex);

            uniqueTypes <-
              unique(
                res$networkCodes[[networkCodeRegex]]$coded_df$type
              );

            configuredEdgeTypes <-
              unlist(
                lapply(
                  res$aestheticConfig[[configName]]$edges,
                  function(x) {
                    if (is.null(x$type) || is.na(x$type) || (nchar(x$type) == 0)) {
                      return("no_type_specified");
                    } else {
                      return(x$type);
                    }
                  }
                )
              );

            res$networkCodes[[networkCodeRegex]]$edgeConfig <-
              stats::setNames(
                res$aestheticConfig[[configName]]$edges,
                configuredEdgeTypes
              );

            for (currentType in uniqueTypes) {

              configuredColor <-
                unlist(
                  res$networkCodes[[networkCodeRegex]]$edgeConfig[[currentType]][
                    setdiff(names(res$networkCodes[[networkCodeRegex]]$edgeConfig[[currentType]]), "type")
                    ]
                );

              if (!is.null(configuredColor)) {

                res$networkCodes[[networkCodeRegex]]$coded_df[
                  which(res$networkCodes[[networkCodeRegex]]$coded_df$type ==
                          currentType),
                  setdiff(names(res$networkCodes[[networkCodeRegex]]$edgeConfig[[currentType]]), "type")
                ] <-
                  configuredColor;
              }

            }

            configuredEdgeAttributes <-
              setdiff(
                names(res$networkCodes[[networkCodeRegex]]$coded_df),
                c("from", "to", "type", "weight", "edge_weight")
              );

            for (edgeInfoToAdd in configuredEdgeAttributes) {
              list_for_edf <-
                c(list_for_edf,
                  structure(
                    list(
                      unlist(
                        res$networkCodes[[networkCodeRegex]]$coded_df[, edgeInfoToAdd]
                      )
                    ),
                    names = edgeInfoToAdd
                  )
                );
            }

          }

          res$networkCodes[[networkCodeRegex]]$edge_df <-
            do.call(
              DiagrammeR::create_edge_df,
              list_for_edf
            );

          res$networkCodes[[networkCodeRegex]]$graph <-
            DiagrammeR::create_graph(
              nodes_df = res$networkCodes[[networkCodeRegex]]$node_df,
              edges_df = res$networkCodes[[networkCodeRegex]]$edge_df
            );

          res$networkCodes[[networkCodeRegex]]$graph <-
            do.call(
              apply_graph_theme,
              c(
                list(graph = res$networkCodes[[networkCodeRegex]]$graph),
                theme_networkDiagram
              )
            );

          res$networkCodes[[networkCodeRegex]]$dot <-
            DiagrammeR::generate_dot(
              res$networkCodes[[networkCodeRegex]]$graph
            );

        }

      } else {

        res$networkCodes[[networkCodeRegex]]$coded_df_full <- NA;
        res$networkCodes[[networkCodeRegex]]$coded_df <- NA;
        res$networkCodes[[networkCodeRegex]]$label_to_id <- NA;
        res$networkCodes[[networkCodeRegex]]$edges <- NA;
        res$networkCodes[[networkCodeRegex]]$edgeConfig <- NA;
        res$networkCodes[[networkCodeRegex]]$node_df <- NA;
        res$networkCodes[[networkCodeRegex]]$edge_df <- NA;
        res$networkCodes[[networkCodeRegex]]$graph <- NA;

      }

    }

  } else {
    res$networkCodes <- NULL;
  }

  ###---------------------------------------------------------------------------

  ### Trim spaces from front and back and store almost clean utterances
  sourceDf$utterances_clean_with_uids <-
    trimws(x);

  ###---------------------------------------------------------------------------
  ### Utterance identifiers

  ### Extract and store UIDs
  sourceDf$uids <-
    ifelse(grepl(uidRegex,
                 sourceDf$utterances_clean_with_uids,
                 perl=TRUE),
           gsub(paste0(".*", uidRegex, ".*"),
                       "\\1",
                       sourceDf$utterances_clean_with_uids),
           "");

  ### Store even cleaner utterances
  sourceDf$utterances_clean <-
    trimws(gsub(uidRegex,
                "",
                sourceDf$utterances_clean_with_uids));

  ###---------------------------------------------------------------------------
  ### Check for occurrences of the nestingMarker, which have to appear
  ### at the beginning of the utterances (except for spaces maybe)

  ### Only retain 'leading' nestingMarkers
  nestingCharacters <-
    sub(
      paste0(
        "^\\s*([",
        nestingMarker,
        " \\s]+).*$"
      ),
      "\\1",
      sourceDf$utterances_clean
    );

  ### From this 'leading' bit, strip all characters
  ### that are not the nestingMarker
  nestingCharacters <-
    gsub(
      paste0(
        "[^",
        nestingMarker,
        "]"
      ),
      "",
      nestingCharacters
    );

  ### Store nesting levels that will be used later
  ### to store utterance identifiers
  sourceDf$nestingLevel <-
    nchar(nestingCharacters);

  ### Store parent utterance identifiers
  sourceDf$parent_uid <-
    get_parent_uid(
      sourceDf$uids,
      sourceDf$nestingLevel
    );

  ### Strip nesting markers from the beginning of the utterances to clean
  ### them even more
  sourceDf$utterances_clean <-
    sub(
      paste0(
        "^\\s*[",
        nestingMarker,
        " \\s]+(.*)$"
      ),
      "\\1",
      sourceDf$utterances_clean
    );

  ### Create 'label' version of utterances

  sourceDf$utteranceLabel <- sourceDf$utterances_clean;

  if (!is.null(utteranceLabelRegexes)) {
    for (i in seq_along(utteranceLabelRegexes)) {
      sourceDf$utteranceLabel <-
        gsub(
          utteranceLabelRegexes[[i]][1],
          utteranceLabelRegexes[[i]][2],
          sourceDf$utteranceLabel,
          perl=TRUE
        );
    }
  }

  ###---------------------------------------------------------------------------
  ### Create an utterance tree

  ### Restructure
  networkDf <-
    sourceDf[,
             c("uids", "parent_uid",
               setdiff(names(sourceDf), c("uids", "parent_uid")))
    ];

  ### Remove lines without utterance identifier
  networkDf <-
    networkDf[
      nchar(networkDf$uids) > 0,
    ];

  ### Convert to tree
  if (any(nchar(networkDf$parent_uid) > 0)) {
    utteranceTree <-
      data.tree::FromDataFrameNetwork(
        networkDf
      );
  } else {
    utteranceTree <- NA;
  }

  ###---------------------------------------------------------------------------
  ### Utterance diagram

  if (any(nchar(networkDf$parent_uid) > 0)) {
    utteranceTree$Do(
      function(node) {
        node$diagramLabel <-
          node$utteranceLabel;
        for (i in seq_along(diagrammerSanitizing)) {
          node$diagramLabel <- gsub(
            diagrammerSanitizing[[i]][1],
            diagrammerSanitizing[[i]][2],
            node$diagramLabel
          );
        }
        node$diagramLabel <-
          paste0(strwrap(node$diagramLabel, 40), collapse="\n");

        return(node$diagramLabel);
      }
    );

    data.tree::SetNodeStyle(
      utteranceTree,
      label = function(node) return(node$diagramLabel)
    );

    utteranceDiagram <-
      data.tree::ToDiagrammeRGraph(
        utteranceTree
      );

    utteranceDiagram <-
      do.call(
        apply_graph_theme,
        list(
          utteranceDiagram,
          theme_utteranceDiagram
        )
      );

  } else {
    utteranceDiagram <- NA;
  }

  ###---------------------------------------------------------------------------

  ### Clean the source dataframe by removing section break rows.

  if (nrow(sourceDf) > 0) {

    sourceDf$originalSequenceNr <- 1:nrow(sourceDf);

    cleanSourceDf <-
      sourceDf[!grepl(paste0(sectionRegexes, collapse="|"),
                      x), ];

    ###-------------------------------------------------------------------------
    ### Changed on 2022-09-13 because rows holding only a code were
    ### also deleted (but shouldn't be)

    ### Original, before 2022-09-13
    # cleanSourceDf <-
    #   cleanSourceDf[nchar(cleanSourceDf$utterances_clean)>0, ];
    ###-------------------------------------------------------------------------

    ### Commented out on 2023-04-12 as it assumes there is only one section
    ### break and it's called "sectionBreak". Didn't need replacement because
    ### the previous command actually already deletes all lines matching one
    ### or more section breaks.
    # cleanSourceDf <-
    #   cleanSourceDf[!cleanSourceDf$sectionBreak_match, ];

    if (removeSectionBreakRows) {
      cleanSourceDf <-
        cleanSourceDf[!cleanSourceDf$sectionBreak_match, ];
    }
    if (removeIdentifierRows) {
      cleanSourceDf <-
        cleanSourceDf[nchar(cleanSourceDf$utterances_without_identifiers)>0, ];
    }
    if (removeEmptyRows) {
      cleanSourceDf <-
        cleanSourceDf[nchar(cleanSourceDf$utterances_raw)>0, ];
    }

  } else {
    cleanSourceDf <- data.frame();
  }

  if (nrow(cleanSourceDf) > 0) {
    cleanSourceDf$sequenceNr <- 1:nrow(cleanSourceDf);
  }

  ### Store results in the object to return
  res$qdt <- cleanSourceDf;
  res$sourceDf <- cleanSourceDf;
  res$utteranceTree <- utteranceTree;
  res$rawSourceDf <- sourceDf;
  res$codings <- codeProcessing[[codeRegex]]$leafCodes;
  res$rawCodings <- codings;
  res$codeProcessing <- codeProcessing;
  res$inductiveCodeTrees <- purrr::map(res$codeProcessing, "inductiveCodeTrees");
  res$inductiveDiagrammeRs <- purrr::map(res$codeProcessing, "inductiveDiagrammeR");
  res$utteranceDiagram <- utteranceDiagram;
  res$mergedSourceDf <- res$qdt;

  ### Merge attributes with source dataframe
  if (length(res$attributes) > 0) {

    # ### Merge attributes with source data
    # res$mergedSourceDf <-
    #   merge(res$sourceDf,
    #         res$attributesDf);

    qdtNew <-
      merge_utterances_and_attributes(
        qdt = res$qdt,
        classes = allClasses,
        attributesDf = res$attributesDf,
        checkClassInstanceIds = checkClassInstanceIds,
        silent = silent
      );

    res$convenience$attributesVars <-
      unique(c(res$convenience$attributesVars,
               setdiff(names(qdtNew), names(res$qdt))));

    res$qdt <- qdtNew;

    ###---------------------------------------------------------------------------
    ###
    ### START --- move this to a separate function for parse_source and parse_sources
    ###
    ###---------------------------------------------------------------------------

    # ### Add attributes to the utterances
    # for (i in seq_along(idRegexes)) {
    #   ### Check whether attributes was provided for this identifier
    #   if (names(idRegexes)[i] %in% names(res$attributesDf)) {
    #     if (!silent) {
    #       print(glue::glue("\n\nFor identifier class {names(idRegexes)[i]}, attributes were provided: proceeding to join to sources dataframe.\n"));
    #     }
    #     ### Convert to character to avoid errors and delete
    #     ### empty columns from merged source dataframe
    #     usedIdRegexes <-
    #       names(idRegexes)[names(idRegexes) %in% names(res$attributesDf)];
    #     for (j in usedIdRegexes) {
    #       res$attributesDf[, j] <-
    #         as.character(res$attributesDf[, j]);
    #     }
    #     for (j in intersect(names(res$mergedSourceDf),
    #                         names(res$attributesDf))) {
    #       if (all(is.na(res$mergedSourceDf[, j]))) {
    #         res$mergedSourceDf[, j] <- NULL;
    #       }
    #     }
    #
    #     if (!(names(idRegexes)[i] %in% names(res$mergedSourceDf))) {
    #       msg <-
    #         paste0("When processing identifier regex '", idRegexes[i],
    #                "', I failed to find its name ('", names(idRegexes[i]),
    #                "') in the column names of the merged ",
    #                "sources data frame (",
    #                vecTxtQ(names(res$mergedSourceDf)), "), so not merging ",
    #                "the attributes data frame with the source data frame for ",
    #                "this class instance identifier.")
    #       if (checkClassInstanceIds) {
    #         warning(msg);
    #       }
    #       if (!silent) {
    #         cat(msg);
    #       }
    #     } else if (!(names(idRegexes)[i] %in% setdiff(names(res$attributesDf), 'type'))) {
    #       msg <-
    #         paste0("When processing identifier regex '", idRegexes[i],
    #                "', I failed to find its name (", names(idRegexes)[i],
    #                ") in the column names of the merged ",
    #                "attributes data frame, so not merging ",
    #                "the attributes data frame with the source data frame for ",
    #                "this class instance identifier..");
    #       if (checkClassInstanceIds) {
    #         warning(msg);
    #       }
    #       if (!silent) {
    #         cat(msg);
    #       }
    #     } else {
    #       # attributesDf[, names(idRegexes)[i]] <-
    #       #   as.character(attributesDf[, names(idRegexes)[i]]);
    #       ### Join attributes based on identifier
    #       res$mergedSourceDf <-
    #         dplyr::left_join(res$mergedSourceDf,
    #                          res$attributesDf[, setdiff(names(res$attributesDf), 'type')],
    #                          by=names(idRegexes)[i]);
    #     }
    #
    #   } else {
    #     if (!silent) {
    #       print(glue::glue("\nFor identifier class {names(idRegexes)[i]}, no attributes was provided.\n"));
    #     }
    #   }
    # }

    if (!silent) {
      cat0("Finished merging attributes with source dataframe. Starting to collect deductive code trees.\n");
    }

    ###---------------------------------------------------------------------------
    ###
    ### END --- move this to a separate function for parse_source and parse_sources
    ###
    ###---------------------------------------------------------------------------


  }

  # ### Check for identifier column existence and convert to character
  # for (i in names(idRegexes)) {
  #   if (i %in% names(res$mergedSourceDf)) {
  #     res$mergedSourceDf[, i] <-
  #       as.character(res$mergedSourceDf[, i]);
  #   } else {
  #     res$mergedSourceDf[, i] <-
  #       rep("", nrow(res$mergedSourceDf));
  #   }
  # }

  ### Add codings and leaves only to the convenience list
  res$convenience$codings <- sort(unique(unlist(res$rawCodings)));

  # if (exists('unspecifiedClasses') && !is.null(unspecifiedClasses)) {
  #   res$convenience$unspecifiedClasses <- unspecifiedClasses;
  # } else {
  #   res$convenience$unspecifiedClasses <- NULL;
  # }

  res$codings <- res$convenience$codings;
  res$convenience$codingLeaves <-
    sort(unique(unlist(get_leaf_codes(res$convenience$codings,
                                      inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker))));

  res$convenience$codingPaths <-
    codePaths_to_namedVector(res$convenience$codings);

  if (length(res$convenience$codings) > 0) {
    ### Count how often each code was used
    res$countedCodings <-
      colSums(res$sourceDf[, res$convenience$codingLeaves, drop=FALSE]);
  } else {
    res$countedCodings <-
      NULL;
  }

  ### Store all available deductive codes in this source
  res$convenience$deductiveCodes <-
    res$deductiveCodes;

  ### Store all available inductive codes in this source
  res$convenience$inductiveSplitCodes <-
    c(lapply(res$codeProcessing,
             function(x) {
               return(x$inductiveCodes);
             }));
  res$convenience$inductiveCodes <-
    lapply(res$convenience$inductiveSplitCodes,
           function(x) {
             return(sort(unique(unname(unlist(x)))));
           });

  if (postponeDeductiveTreeBuilding) {
    res$extendedDeductiveCodeTrees <- NA;
    res$fullyMergedCodeTrees <- NA;
  } else if (!postponeDeductiveTreeBuilding && ("Node" %in% class(res$deductiveCodeTrees))) {
    ### Merge inductive code tree into deductive code tree (currently only support
    ### for one deductive code tree)
    res$extendedDeductiveCodeTrees <-
      data.tree::Clone(res$deductiveCodeTrees);
    res$fullyMergedCodeTrees <-
      data.tree::Clone(res$deductiveCodeTrees);

    for (i in names(res$inductiveCodeTrees)) {
      if ("Node" %in% class(res$inductiveCodeTrees[[i]])) {
        for (j in names(res$inductiveCodeTrees[[i]]$children)) {
          if (j %in% res$deductiveCodes) {
            currentNode1 <-
              data.tree::FindNode(res$extendedDeductiveCodeTrees,
                                  j);
            currentNode2 <-
              data.tree::FindNode(res$fullyMergedCodeTrees,
                                  j);
            for (k in names(res$inductiveCodeTrees[[i]]$children[[j]]$children)) {
              currentNode1$AddChildNode(res$inductiveCodeTrees[[i]]$children[[j]]$children[[k]]);
              currentNode2$AddChildNode(res$inductiveCodeTrees[[i]]$children[[j]]$children[[k]]);
            }
          } else {
            res$fullyMergedCodeTrees$AddChildNode(res$inductiveCodeTrees[[i]]$children[[j]]);
          }
        }
      }
    }
  } else if (!postponeDeductiveTreeBuilding) {
    res$extendedDeductiveCodeTrees <- NA;
    res$fullyMergedCodeTrees <- res$inductiveCodeTrees;
  }


  res$convenience$valid_inductiveCodeTrees <-
    which(unlist(lapply(res$inductiveCodeTrees, is.environment)));

  res$convenience$original_inductiveCodeTreeNames <-
    names(res$inductiveCodeTrees);

  res$inductiveCodeTrees <-
    res$inductiveCodeTrees[res$convenience$valid_inductiveCodeTrees];

  if (!silent) {
    cat("\n\n");
  }

  ### Return result
  return(res);

}

#' @rdname parsing_sources
#' @method print rock_parsedSource
#' @export
print.rock_parsedSource <- function(x, prefix="### ",  ...) {
  totalSectionMatches <-
    sum(unlist(lapply(x$rawSourceDf[, grep('_match',
                                           names(x$rawSourceDf))],
                      as.numeric)));

  appliedCodes <-
    sort(unique(unlist(x$convenience$codingLeaves)));

  totalCodingMatches <-
    sum(unlist(x$sourceDf[, appliedCodes]));

  if (totalCodingMatches > 0) {
    codingInfo <-
      glue::glue("These {nrow(x$sourceDf)} utterances were coded ",
                 "{totalCodingMatches} times in total using these codes: ",
                 "{vecTxtQ(appliedCodes)}.");
  } else {
    codingInfo <-
      glue::glue("These {nrow(x$sourceDf)} utterances were not coded at all.");
  }

  if (length(x$inductiveCodeTrees) > 0) {
    inductiveTreesInfo <-
      glue::glue("This source contained inductive coding trees. ",
                 "These are shown in R Studio's viewer.\n\n")
  } else {
    inductiveTreesInfo <-
      glue::glue("This source contained no inductive coding trees.\n\n")
  }

  if (length(x$deductiveCodeTrees) > 0) {
    deductiveTreesInfo <-
      glue::glue("This source contained deductive coding trees. ",
                 "These are also shown in R Studio's viewer.\n\n")
  } else {
    deductiveTreesInfo <-
      glue::glue("This source contained no deductive coding trees.\n\n")
  }

  identifiers <-
    names(x$arguments$idRegexes);
  occurringIdentifiers <-
    identifiers[identifiers %in% names(x$sourceDf)];

  if (length(occurringIdentifiers) > 0) {
    actualIdentifiers <-
      lapply(x$sourceDf[, occurringIdentifiers, drop=FALSE],
             unique);
    actualIdentifiers <-
      lapply(actualIdentifiers,
             sort);
    actualIdentifiers <-
      lapply(actualIdentifiers,
             function(x) return(x[!(x=="no_id")]));
    identifierInfo <-
      glue::glue("This source contained matches with identifier regular expressions. Specifically, ",
                 glue::glue_collapse(lapply(names(actualIdentifiers),
                                            function(x) return(glue::glue("identifier regular expression '{x}' matched ",
                                                                          "with identifiers {vecTxtQ(actualIdentifiers[[x]])}"))),
                                     ", "),
                 ".");
  } else {
    identifierInfo <-
      glue::glue("This source contained no matches with identifier regular expressions.")
  }

  print(glue::glue("\n\n",
                   "{prefix}Preprocessing\n\n",
                   "The parsed source contained {length(x$arguments$x)} lines. ",
                   "After removing lines that matched '{x$arguments$ignoreRegex}', ",
                   "the regular expression specifying which lines to ignore, and did not ",
                   "make up the {length(x$yamlFragments)} YAML fragments with attributes or ",
                   "deductive coding tree specifications, {nrow(x$rawSourceDf)} lines remained.",
                   " {totalSectionMatches} of these matched one of the section regular ",
                   "expressions ({vecTxtQ(x$arguments$sectionRegexes)}), and after ",
                   "removing these lines and all lines that were empty after removing ",
                   "characters that matched one or more class instance identifier(s) ",
                   "({vecTxtQ(x$arguments$idRegexes)}) and coding regular expressions",
                   "regular expressions, ({vecTxtQ(x$arguments$codeRegexes)}), ",
                   "{nrow(x$sourceDf)} utterances remained.",
                   "\n\n",
                   "{prefix}Class instance identifiers\n\n",
                   identifierInfo,
                   "\n\n",
                   "{prefix}Utterances and coding\n\n",
                   codingInfo,
                   "\n\n",
                   "{prefix}Inductive coding trees\n\n",
                   inductiveTreesInfo,
                   "\n",
                   "{prefix}Deductive coding trees\n\n",
                   deductiveTreesInfo));
  #if (length(x$inductiveCodeTrees) > 0) {
  if (length(x$inductiveDiagrammeRs) > 0) {
    #for (i in names(x$inductiveCodeTrees)) {
    for (i in names(x$inductiveDiagrammeRs)) {
      #print(graphics::plot(x$inductiveCodeTrees[[i]]));
      if (!is.null(x$inductiveDiagrammeRs[[i]])) {
        print(DiagrammeR::render_graph(x$inductiveDiagrammeRs[[i]]));
      }
    }
  }
  if (length(x$deductiveCodeTrees) > 0) {
    print(graphics::plot(x$deductiveCodeTrees));
  }
  invisible(x);
}
