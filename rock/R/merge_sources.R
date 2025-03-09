#' Merge source files by different coders
#'
#' This function takes sets of sources and merges them using the utterance
#' identifiers (UIDs) to match them.
#'
#' @inheritParams parse_sources
#' @param input The directory containing the input sources.
#' @param output The path to the directory where to store the merged sources.
#' This path will be created with a warning if it does not exist. An exception
#' is if "`same`" is specified - in that case, every file will be written to the
#' same directory it was read from.
#' @param outputPrefix,outputSuffix A pre- and/or suffix to add to the filename
#' when writing the merged sources (especially useful when writing them to the
#' same directory).
#' @param primarySourcesRegex A regular expression that specifies how to
#' recognize the primary sources (i.e. the files used as the basis, to which
#' the codes from other sources are added).
#' @param primarySourcesIgnoreRegex A regular expression that specifies which
#' files to ignore as primary files.
#' @param primarySourcesPath The path containing the primary sources.
#' @param primarySourcesFileList,sourcesFileList Alternatively to using regular
#' expressions, lists of full paths and filenames to the primary sources and all
#' sources to process can be specified using these arguments. If this is used,
#' neither can be `NULL`.
#' @param recursive,primarySourcesRecursive Whether to read files from
#' sub-directories (`TRUE`) or not.
#' @param filenameRegex Only files matching this regular expression are read.
#' @param preventOverwriting Whether to prevent overwriting existing files or not.
#' @param inheritSilence If not silent, whether to let functions called
#' by `merge_sources` inherit that setting.
#'
#' @return Invisibly, a list of the parsed, primary, and merged sources.
#' @export
merge_sources <- function(input,
                          output,
                          outputPrefix = "",
                          outputSuffix = "_merged",
                          primarySourcesRegex=".*",
                          primarySourcesIgnoreRegex=outputSuffix,
                          primarySourcesPath = input,
                          recursive = TRUE,
                          primarySourcesRecursive = recursive,
                          filenameRegex = ".*",
                          primarySourcesFileList = NULL,
                          sourcesFileList = NULL,
                          postponeDeductiveTreeBuilding = TRUE,
                          ignoreOddDelimiters=FALSE,
                          preventOverwriting = rock::opts$get(preventOverwriting),
                          encoding=rock::opts$get(encoding),
                          silent=rock::opts$get(silent),
                          inheritSilence = FALSE) {

  if (!dir.exists(primarySourcesPath)) {
    stop("Directory specified to read primary sources from (",
         primarySourcesPath, ") does not exist!");
  }

  ### Store all arguments and delete the ones specific to this function
  # args <- as.list(environment());
  # args <-
  #   args[setdiff(names(args),
  #                c('primarySourcesRegex',
  #                  'primarySourcesIgnoreRegex',
  #                  'primarySourcesPath',
  #                  'primarySourcesRecursive',
  #                  'output',
  #                  'outputPrefix',
  #                  'outputSuffix',
  #                  'preventOverwriting',
  #                  'inheritSilence'))];
  # ### Set 'silent' as function of both imperative for this function and the called functions
  # args$silent <- ifelse(inheritSilence, silent, TRUE);

  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);
  autoGenerateIds <- rock::opts$get(autoGenerateIds);
  ### Obsolete now all class instance identifiers are persistent
  # persistentIds <- rock::opts$get(persistentIds);
  utteranceMarker <- rock::opts$get(utteranceMarker);
  noCodes <- rock::opts$get(noCodes);
  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);
  attributeContainers <- rock::opts$get(attributeContainers);
  codesContainers <- rock::opts$get(codesContainers);
  delimiterRegEx <- rock::opts$get(delimiterRegEx);
  ignoreRegex <- rock::opts$get(ignoreRegex);
  coderId <- rock::opts$get(coderId);
  idForOmittedCoderIds <- rock::opts$get(idForOmittedCoderIds);

  if (!is.null(primarySourcesFileList) || !is.null(sourcesFileList)) {
    if (is.null(primarySourcesFileList) || is.null(sourcesFileList)) {
      stop("If passing paths and filenames directly using ",
           "`primarySourcesFileList` and `sourcesFileList`, both must ",
           "be provided!");
    }

    stop("This functionality has not bee implemented yet!");

  } else {

    if (!silent) {
      cat0("\n\nStarting to extract all codings from all sources in directory '",
           input, "' that match regular expression '", filenameRegex, "'.");
    }

    ### Then pass arguments along to extract_codings_by_coderId and store result
    parsedSources <-
      do.call(extract_codings_by_coderId,
              list(input=input,
                   recursive = recursive,
                   filenameRegex = filenameRegex,
                   ignoreOddDelimiters=ignoreOddDelimiters,
                   postponeDeductiveTreeBuilding = postponeDeductiveTreeBuilding,
                   encoding=encoding,
                   silent=ifelse(inheritSilence, silent, TRUE)));

  }

  if (!silent) {
    cat0("\n\nRead ", length(parsedSources$parsedSources), " sources.");
  }

  allCodedUtterances <-
    names(parsedSources$utterances);

  if (!silent) {
    cat0("\n\nProcessed codings for a total of ",
         length(allCodedUtterances), " utterances, where the first six utterance identifiers are ",
         vecTxtQ(utils::head(allCodedUtterances)), " and the last six utterance identifiers are ",
         vecTxtQ(utils::tail(allCodedUtterances)), ".\n\n");
  }

  if (!silent) {
    cat0("\n\nStarting to load primary sources in directory '",
         primarySourcesPath, "'.\n\n");
  }

  ### Processing section breaks
  sectionBreaksByUID <-
    sectionBreaksByCoder_to_sectionBreaksByUID(parsedSources$sectionBreaksByCoder);

  ### Read primary sources
  primarySources <-
    load_sources(input = primarySourcesPath,
                 encoding=encoding,
                 filenameRegex=primarySourcesRegex,
                 recursive=primarySourcesRecursive,
                 full.names=TRUE,
                 ignoreRegex=primarySourcesIgnoreRegex,
                 silent=ifelse(inheritSilence, silent, TRUE));

  if (!silent) {
    cat0("\n\nLoaded ", length(primarySources), " primary sources.");
  }

  if (!(tolower(output) == "same")) {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  if (!silent) {
    cat0("\n\nStarting to merge codes into primary sources.\n");
  }

  mergedSources <- list();
  primarySourceUids <- list();
  for (i in names(primarySources)) {

    if (!silent) {
      cat0("\n - Starting to process primary source '", basename(i), "'.");
    }

    ### Check for matches with UID regex
    uidRegexMatches <-
      grepl(uidRegex,
            primarySources[[i]],
            perl=TRUE);

    if (!silent) {
      cat0("\n   - Out of the ", length(uidRegexMatches), " lines in this source, ",
           sum(uidRegexMatches), " match the uidRegex (i.e. contain an utterance identifier).");
    }

    ### Extract the UIDs and store in a vector, where every element
    ### corresponds to a line in the source.
    primarySourceUids[[i]] <-
      ifelse(uidRegexMatches,
             gsub(paste0(".*", uidRegex, ".*"),
                  "\\1",
                  primarySources[[i]]),
             "");

    ### Store primary source as basis for the merged source text
    mergedSources[[i]] <- primarySources[[i]];

    ### This way, 'j' is both the index for the UID vector and
    ### for the corresponding line in the sources
    nrOfMergedUtteranceCodings <- 0;
    for (j in seq_along(primarySourceUids[[i]])) {

      currentUID <- primarySourceUids[[i]][j];

      if (rock::opts$get(debug)) {
        cat0("\n     - Processing UID '", currentUID, "'.");
      }

      ### Process coded utterances
      if (currentUID %in% allCodedUtterances) {
        nrOfMergedUtteranceCodings <-
          nrOfMergedUtteranceCodings + 1;
        ### Check whether one of them is already applied
        codings <-
          unname(unlist(parsedSources$utterances[[currentUID]]));

        if (rock::opts$get(debug)) {
          cat0("\n       - Found codings: ", vecTxtQ(codings), " in utterance:\n",
               "         '", mergedSources[[i]][j], "'");
        }

        alreadyAppliedCodings <-
          unlist(lapply(codings,
                        grepl,
                        x = mergedSources[[i]][j],
                        fixed=TRUE));

        if (rock::opts$get(debug)) {
          cat0("\n       - Already applied codings: ", vecTxtQ(codings[alreadyAppliedCodings]), ".");
        }

        ### Add new codes
        mergedSources[[i]][j] <-
          paste0(mergedSources[[i]][j], " ",
                 paste0(codings[!alreadyAppliedCodings],
                        collapse = " "));

        if (rock::opts$get(debug)) {
          cat0("\n       - Added codings: ", vecTxtQ(codings[!alreadyAppliedCodings]), ".");
        }

      }

    }

    if (!silent) {
      cat0("\n   - ", nrOfMergedUtteranceCodings, " utterances merged.");
    }

    if (!silent) {
      cat0("\n   - Starting to merge section breaks - starting with checking ",
           length(sectionBreaksByUID$matches_pre), " section breaks to insert after a UID.");
    }

    if (any(primarySourceUids[[i]] %in% names(sectionBreaksByUID$matches_pre))) {

      if (rock::opts$get(debug)) {
        cat0("\n   - At least one utterance matches the list of utterances after which to insert a section break.");
      }

      ### Get indices (and so, line numbers) of the lines after which to
      ### insert section breaks. Reverse the order because we have to start
      ### at the end with inserting the section breaks - otherwise the line
      ### numbers shift.
      preSectionBreakUIDindices <-
        rev(which(primarySourceUids[[i]] %in% names(sectionBreaksByUID$matches_pre)));

      for (currentPreSectionBreakUIDindex in preSectionBreakUIDindices) {

        currentUID <- primarySourceUids[[i]][currentPreSectionBreakUIDindex];
        currentSectionBreak <-
          sectionBreaksByUID$matches_pre[names(sectionBreaksByUID$matches_pre) == currentUID];

        ###---------------------------------------------------------------------
        ### At some point - try to find the appropriate content and add it in here.
        ###---------------------------------------------------------------------

        # currentContent <-


        if (length(currentSectionBreak) > 1) {
          if (rock::opts$get(debug)) {
            cat0("\n     - Processing UID '", currentUID, "', which matches multiple section breaks: ",
                 vecTxtQ(currentSectionBreak), ". ");
          }
        } else {
          if (rock::opts$get(debug)) {
            cat0("\n     - Processing UID '", currentUID, "' and section break '",
                 currentSectionBreak, "'. ");
          }
        }

        if (grepl(paste0(rock::opts$get(sectionRegexes), collapse="|"),
                  mergedSources[[i]][currentPreSectionBreakUIDindex+1])) {

          ### There's already a section break on that line
          if (rock::opts$get(debug)) {
            cat0("The next line ('",
                 trimws(mergedSources[[i]][currentPreSectionBreakUIDindex+1]),
                 "') already contains a section break; ");
          }

          ### Check whether the section break we're checking for is already on that line.
          sectionBreaksAlreadyThere <-
            multigrepl(currentSectionBreak,
                       mergedSources[[i]][currentPreSectionBreakUIDindex+1],
                       fixed=TRUE);
          if (all(sectionBreaksAlreadyThere)) {
            if (rock::opts$get(debug)) {
              cat0("and it's the one we were about to add ('", currentSectionBreak,
                   "'), so not doing anything.");
            }
          } else {
            ### It isn't - we can just append this one
            mergedSources[[i]][currentPreSectionBreakUIDindex+1] <-
              paste(mergedSources[[i]][currentPreSectionBreakUIDindex+1],
                    currentSectionBreak[!sectionBreaksAlreadyThere]);
            # stop("Just added '", currentSectionBreak, "' to line '",
            #      mergedSources[[i]][currentPreSectionBreakUIDindex+1], "'.")
            if (rock::opts$get(debug)) {
              cat0("but it's a different one from the one we were about to add ('",
                   currentSectionBreak[!sectionBreaksAlreadyThere],
                   "'), so we append it to that line.");
            }
          }

        } else {

          if (rock::opts$get(debug)) {
            cat0("The next line does not already contain a section break; shifting ",
                 "all subsequent lines one position and inserting a line ",
                 "containing only section break '", currentSectionBreak, "'.");
          }

          ### We have to shift all subsequent lines and then insert this one
          lastLineIndex <- length(primarySourceUids[[i]]);

          linesOfLastPartOfSource_preShift <- (currentPreSectionBreakUIDindex+1):lastLineIndex;
          linesOfLastPartOfSource_postShift <- linesOfLastPartOfSource_preShift + 1;

          #### Shift
          mergedSources[[i]][linesOfLastPartOfSource_postShift] <-
            mergedSources[[i]][linesOfLastPartOfSource_preShift];

          ### Insert line containing only the section break
          mergedSources[[i]][currentPreSectionBreakUIDindex+1] <-
            sectionBreaksByUID$matches_pre[currentUID];

        }
      }
    } else {
      if (rock::opts$get(debug)) {
        cat0("\n   - None of the utterances matches the list of utterances before which to insert a section break.");
      }
    }

    newFilename <-
      paste0(outputPrefix,
             sub("^(.*)\\.[a-zA-Z0-9]+$",
                 "\\1",
                 basename(i)),
             outputSuffix,
             ".rock");
    if (tolower(output) == "same") {
      newFileDir <-
        dirname(i);
    } else {
      newFileDir <-
        output;
    }
    newFullname <- file.path(newFileDir,
                             newFilename);

    if (!utils::tail(mergedSources[[i]], 1) == "") {
      mergedSources[[i]] <-
        c(mergedSources[[i]], "");
    }

    if (!silent) {
      cat0("\nStarting to write merged source '", newFilename,
           "' to directory '", newFileDir, "'.\n");
    }

    if (file.exists(newFullname) && (preventOverwriting)) {
      if (!silent) {
        message("Output file '", newFilename, "' already exists and ",
                "`preventOverwriting` is set to TRUE - not writing output file!");
      }
    } else {
      con <- file(description=newFullname,
                  open="w",
                  encoding=encoding);
      writeLines(text=mergedSources[[i]],
                 con=con);
      close(con);
    }

  }

  res <- list(parsedSources = parsedSources,
              primarySources = primarySources,
              sectionBreaksByUID = sectionBreaksByUID,
              mergedSources = mergedSources);

  return(invisible(res));

}
