#' Extract the codings by each coder using the coderId
#'
#' @param input The directory with the sources.
#' @param recursive Whether to also process subdirectories.
#' @param filenameRegex Only files matching this regular expression will be processed.
#' @param postponeDeductiveTreeBuilding Whether to build deductive code trees, or only
#' store YAML fragments.
#' @param ignoreOddDelimiters Whether to throw an error when encountering an odd number of
#' YAML delimiters.
#' @param encoding The encoding of the files to read.
#' @param silent Whether to be chatty or silent.
#'
#' @return An object with the read sources.
#'
#' @export
extract_codings_by_coderId <- function(input,
                                       recursive = TRUE,
                                       filenameRegex = ".*",
                                       postponeDeductiveTreeBuilding = TRUE,
                                       ignoreOddDelimiters=FALSE,
                                       encoding=rock::opts$get(encoding),
                                       silent=rock::opts$get(silent)) {

  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);
  autoGenerateIds <- rock::opts$get(autoGenerateIds);
  ### Obsolete now all class instance identifiers are persistent
  # persistentIds <- rock::opts$get(persistentIds);
  noCodes <- rock::opts$get(noCodes);
  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);
  attributeContainers <- rock::opts$get(attributeContainers);
  codesContainers <- rock::opts$get(codesContainers);
  delimiterRegEx <- rock::opts$get(delimiterRegEx);
  ignoreRegex <- rock::opts$get(ignoreRegex);
  coderId <- rock::opts$get(coderId);
  idForOmittedCoderIds <- rock::opts$get(idForOmittedCoderIds);

  if (!silent) {
    cat0("\n\nStarting to parse sources in directory '",
         input, "' by coder identifier.\n\n");
  }

  ### First parse all sources by coderId
  parsedSources <-
    do.call(parse_sources_by_coderId,
            list(input=input,
                 recursive = recursive,
                 filenameRegex = filenameRegex,
                 ignoreOddDelimiters=ignoreOddDelimiters,
                 postponeDeductiveTreeBuilding = postponeDeductiveTreeBuilding,
                 encoding=encoding,
                 silent=silent));

  idNames <- names(idRegexes);

  res <- list(parsedSources = parsedSources,
              codingsByCoder = list(),
              sectionBreaksByCoder = list());

  if (!silent) {
    cat0("\n\nStarting to extract codings for each utterance identifier.\n\n");
  }

  ### Construct objects with codes for each utterance
  for (filename in names(parsedSources)) {

    if (!silent) {
      cat0("\nStarting to extract codings from '", filename, "'.\n");
    }

    for (coderId in names(parsedSources[[filename]]$parsedSubsources)) {
      if ('uids' %in% names(parsedSources[[filename]]$parsedSubsources[[coderId]]$rawSourceDf)) {
        ### For convenience, store some stuff
        ### Note that we use 'rawSourceDf' since that still has lines with section breaks
        sourceDf <-
          parsedSources[[filename]]$parsedSubsources[[coderId]]$rawSourceDf;
        codings <-
          parsedSources[[filename]]$parsedSubsources[[coderId]]$convenience$codingLeaves;
        if (length(codings) == 0) {
          codingsList <- NA;
        } else {
          codingsList <-
            apply(sourceDf[, c('uids', codings)],
                  1,
                  function(x) {
                    codingNames <-
                      utils::tail(names(x), -1);
                    logicalCodings <-
                      as.logical(as.numeric(utils::tail(x, -1)));
                    return(codingNames[logicalCodings]);
                  });

          names(codingsList) <-
            sourceDf$uids;
        }
        if (!(filename %in% names(res$codingsByCoder))) {
          res$codingsByCoder[[filename]] <- list();
        }
        res$codingsByCoder[[filename]][[coderId]] <-
          codingsList;

        sectionMatches <- list();

        for (j in names(sectionRegexes)) {

          sectionMatches[[j]] <-
            list(index = which(sourceDf[, paste0(j, "_match")]));

          sectionMatches[[j]]$content <-
            sourceDf[sectionMatches[[j]]$index, paste0(j, "_content")];

          sectionMatches[[j]]$uid_pre <-
            unlist(lapply(sectionMatches[[j]]$index-1,
                          function(i) {
                            if (i < 1) {
                              return(NA);
                            } else {
                              return(sourceDf[
                                max(which(nchar(sourceDf[1:i, 'uids']) > 0)),
                                'uids']);
                            }
                          }));

          sectionMatches[[j]]$uid_at <-
            sourceDf[sectionMatches[[j]]$index, "uids"];

          sectionMatches[[j]]$uid_post <-
            unlist(lapply(sectionMatches[[j]]$index+1,
                          function(i) {
                            if (i > nrow(sourceDf)) {
                              return(NA);
                            } else {
                              if (any(nchar(sourceDf[i:nrow(sourceDf), 'uids']) > 0)) {
                                return(sourceDf[
                                  ### Correct for index starting at i (i starts at next row)
                                  i-1 +
                                    min(which(nchar(sourceDf[i:nrow(sourceDf), 'uids']) > 0)),
                                  'uids']
                                );
                              } else {
                                return(NA);
                              }
                            }
                          }));
        }

        if (!(filename %in% names(res$sectionBreaksByCoder))) {
          res$sectionBreaksByCoder[[filename]] <- list();
        }

        res$sectionBreaksByCoder[[filename]][[coderId]] <-
          sectionMatches;

      } ### End if section that's only run if 'uids' exists in the sourceDf
    }
  }

  if (!silent) {
    cat0("\n\nStarting to organise codings per utterance identifier.\n\n");
  }

  res$utterances <- list();

  for (currentSource in names(res$codingsByCoder)) {

    if (!silent) {
      cat0("\nProcessing source '", currentSource, "'.\n");
    }

    for (currentCoderId in names(res$codingsByCoder[[currentSource]])) {

      if (!silent) {
        cat0("\nProcessing codings by coder with identifier '", currentCoderId, "'.\n");
      }

      utterancesInSource <-
        names(res$codingsByCoder[[currentSource]][[currentCoderId]]);
      codedUtterances <-
        which(unlist(lapply(res$codingsByCoder[[currentSource]][[currentCoderId]],
                            length)) > 0);
      for (currentUID in utterancesInSource[codedUtterances]) {
        ### Loop through all coded utterances by this coder in this source

        ### Get original codings from original source
        originalSourceLine <-
          parsedSources[[currentSource]]$parsedSubsources[[currentCoderId]]$rawSourceDf[
            parsedSources[[currentSource]]$parsedSubsources[[currentCoderId]]$rawSourceDf$uids == currentUID,
            'utterances_raw'];
        rawCodings <-
          regmatches(originalSourceLine,
                     gregexpr(paste0(codeRegexes, collapse="|"),
                              originalSourceLine));
        codingInfo <-
          stats::setNames(rawCodings, #list(res$codingsByCoder[[currentSource]][[currentCoderId]][[currentUID]]),
                          currentSource);

        if (currentUID %in% names(res$utterances)) {
          ### If this uid already contains information, append the new info
          if (currentCoderId %in% names(res$utterances[[currentUID]])) {
            res$utterances[[currentUID]][[currentCoderId]] <-
              c(res$utterances[[currentUID]][[currentCoderId]],
                codingInfo);
          } else {
            res$utterances[[currentUID]][[currentCoderId]] <-
              codingInfo;
          }
        } else {
          ### No coding information about this utterance has been added yet
          res$utterances[[currentUID]] <-
            stats::setNames(list(codingInfo),
                            currentCoderId);
        }
      }
    }
  }

  return(res);

}
