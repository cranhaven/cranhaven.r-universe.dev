#' @rdname parsing_sources
#' @export
parse_sources <- function(path,
                          extension = "rock|dct",
                          regex=NULL,
                          recursive=TRUE,
                          removeSectionBreakRows = rock::opts$get('removeSectionBreakRows'),
                          removeIdentifierRows = rock::opts$get('removeIdentifierRows'),
                          removeEmptyRows = rock::opts$get('removeEmptyRows'),
                          suppressDuplicateInstanceWarnings = rock::opts$get('suppressDuplicateInstanceWarnings'),
                          filesWithYAML = NULL,
                          ignoreOddDelimiters = FALSE,
                          checkClassInstanceIds = rock::opts$get("checkClassInstanceIds"),
                          mergeInductiveTrees = FALSE,
                          encoding=rock::opts$get("encoding"),
                          silent=rock::opts$get("silent")) {

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
  codeTreeMarker <- rock::opts$get(codeTreeMarker);

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  if (is.null(regex)) {
    if (grepl("|", extension, fixed=TRUE)) {
      regex <- paste0("^(.*)\\.",
                      strsplit(extension,
                               "|",
                               fixed=TRUE)[[1]],
                      "$",
                      collapse="|");
    } else {
      regex <- paste0("^(.*)\\.", extension, "$");
    }
  }

  fileList <-
    list.files(path=path,
               pattern=regex,
               recursive=recursive,
               full.names=TRUE);

  if (length(fileList) == 0) {
    cat0("\nThere are no files matching regular expression '",
         regex, "' in directory '", path, "', so I have nothing to do.\n");
    return(invisible(NULL));
  }

  res <- list(input=as.list(environment()));

  if (!silent) {
    cat0("\nStarting to process all files matching regular expression '",
              regex, "' in directory '", path, "'.\n\n");
  }

  res$parsedSources <-
    lapply(fileList,
           parse_source,
           ignoreOddDelimiters = ignoreOddDelimiters,
           encoding=encoding,
           postponeDeductiveTreeBuilding = TRUE,
           removeSectionBreakRows = removeSectionBreakRows,
           removeIdentifierRows = removeIdentifierRows,
           filesWithYAML = filesWithYAML,
           removeEmptyRows = removeEmptyRows,
           mergeAttributes = FALSE,
           silent=silent);

  if (!silent) {
    cat0("Done parsing all sources in directory '", path, "'.\n");
  }

  names(res$parsedSources) <-
    basename(fileList);

  ### Get a full list of all rawCodings
  res$convenience <-
    list(rawCodings = purrr::map(res$parsedSources,
                                 'rawCodings'),
         rawCodingLeaves = purrr::map(res$parsedSources,
                                      'codings'));

  res$convenience$codings <-
    sort(unique(unlist(res$convenience$rawCodings)));
  res$convenience$codingLeaves <-
    sort(
      unique(
        unlist(
          get_leaf_codes(
            res$convenience$rawCodingLeaves,
            inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker
          )
        )
      )
    );

  # res$convenience$attributes <-
  #   dplyr::bind_rows(
  #     lapply(res$parsedSource,
  #            function(x) {
  #              if (is.data.frame(x$attributesDf)) {
  #                return(x$attributesDf);
  #              } else {
  #                return(NULL);
  #              }
  #            })
  #   );

  ### 2024-05-29: Get all the class identifiers from all the sources
  res$convenience$allClassIds <-
    unique(
      unlist(
        lapply(
          res$parsedSources,
          function(x) {
            return(x$convenience$allClasses);
          }
        )
      )
    );

  ### 2024-05-29: Stored just before refactoring to allow attributes to exist for multiple
  ### classes
  ### Also 2024-05-29: enabled again as we discovered that merge_utterances_and_attributes()
  ### seems to be written for multiple classes with attributes already --- provided we can
  ### deal with different column names for each attribute specification
  res$convenience$attributes <-
    rbind_df_list(
      lapply(
        res$parsedSources,
        function(x) {
          return(x$attributesDf);
        }
      )
    );

  ### 2024-05-29: Parse the attributes in all separate sources, organizing them per
  ### class identifier, and then collapsing them over sources into one attribute dataframe
  ### per class identifier.

  res$convenience$attributesPerClass <-
    lapply(
      res$convenience$allClassIds,
      function(currentClassId) {

        listOfAttDfsForThisClassForAllSources <-
          lapply(
            res$parsedSources,
            function(currentSource) {

              if ((!all(is.na(currentSource$attributes))) &&
                  (length(currentSource$attributes) > 0)) {

                listOfDataframes <-
                  lapply(
                    currentSource$attributes,
                    function(currentAttributeSpec) {
                      if (currentClassId %in% names(currentAttributeSpec)) {
                        return(as.data.frame(currentAttributeSpec, stringsAsFactors = FALSE));
                      } else {
                        return(NULL);
                      }
                    }
                  );

                if (length(unlist(listOfDataframes)) > 0) {

                  attributeDfForThisClassInThisSource <-
                    rbind_df_list(listOfDataframes);

                  # attributeDfForThisClassInThisSource <-
                  #   tryCatch(
                  #     do.call(rbind,
                  #             listOfDataframes),
                  #     error = function(e) {
                  #
                  #       colCounts <-
                  #         table(
                  #           unlist(
                  #             lapply(
                  #               listOfDataframes,
                  #               colnames
                  #             )
                  #           )
                  #         );
                  #
                  #       stop("I could not parse the attributes into a data frame. At present, ",
                  #            "I require that all attributes are specified for all class ",
                  #            "instances - you may have omitted one (or more). Sorry! ",
                  #            "The following columns appear the following number of ",
                  #            "times: ", vecTxt(paste0(names(colCounts), " (", colCounts, " times)")),
                  #            ".");
                  #     });

                } else {

                  ### No attributes for this source
                  return(NULL);

                }

              } else {

                ### No attributes for this source
                return(NULL);

              }

              return(attributeDfForThisClassInThisSource);

            }
          );

        if (length(unlist(listOfAttDfsForThisClassForAllSources)) == 0) {
          attributeDfsRbindedOverSources <- NULL;
        } else {

          attributeDfsRbindedOverSources <-
            tryCatch(
              rbind_df_list(listOfAttDfsForThisClassForAllSources),
              # do.call(rbind,
              #         listOfAttDfsForThisClassForAllSources),
              error = function(e) {

                colCounts <-
                  table(
                    unlist(
                      lapply(
                        listOfAttDfsForThisClassForAllSources,
                        colnames
                      )
                    )
                  );

                stop("I could not parse the attributes into a data frame. At present, ",
                     "I require that all attributes are specified for all class ",
                     "instances - you may have omitted one (or more). Sorry! ",
                     "The following columns appear the following number of ",
                     "times: ", vecTxt(paste0(names(colCounts), " (", colCounts, " times)")),
                     ".");
              });
        }

        if (length(unlist(listOfAttDfsForThisClassForAllSources)) > 0) {

          if (length(duplicated(attributeDfsRbindedOverSources[[currentClassId]])) > 0) {

            res <-
              do.call(
                rbind,
                lapply(
                  sort(unique(attributeDfsRbindedOverSources[[currentClassId]])),
                  function(instanceId) {

                    instanceDf <-
                      attributeDfsRbindedOverSources[
                        attributeDfsRbindedOverSources[[currentClassId]] == instanceId,
                      ];

                    instanceList <-
                      lapply(
                        names(instanceDf),
                        function(colName) {

                          col <- instanceDf[, colName];

                          nonMissingValues <-
                            unique(col[!is.na(col)]);

                          if (all(is.na(col))) {
                            return(NA);
                          } else if (all(!is.na(col)) && (length(nonMissingValues) == 1)) {
                            return(nonMissingValues);
                          } else if (length(nonMissingValues) == 1) {
                            return(nonMissingValues);
                          } else {
                            ### Take the first non-NA element
                            firstElement <- nonMissingValues[1];
                            if (!suppressDuplicateInstanceWarnings) {
                              warning("For instance '", instanceId, "' of class '",
                                      currentClassId, "', attribute '",
                                      colName, "' has different values: ",
                                      vecTxtQ(nonMissingValues),
                                      ". Taking the first element: '",
                                      firstElement, "'.");
                            }
                            return(firstElement);
                          }
                        }
                    );

                    names(instanceList) <- names(instanceDf);

                    return(
                      as.data.frame(
                        instanceList
                      )
                    );

                  }
                )
              );

          } else {
            res <- attributeDfsRbindedOverSources;
          }

        } else {
          res <- NA;
        }

        return(res);

      }
    );

  ### Class instance identifiers are sometimes used without attributes; in that
  ### case, this will be a data frame of 0 cols and 0 rows.
  if (ncol(res$convenience$attributes) == length(res$convenience$allClassIds)) {
    names(res$convenience$attributes) <- res$convenience$allClassIds;
  }

  ### 2024-05-29: What we just produced is actually the 'new attributeDf' except that it's
  ### a list of Dfs organized per class identifier

  # dplyr::bind_rows(purrr::map(res$parsedSources,
  #                             'attributesDf'));

  ### 2024-05-29: This no longer makes sense; keeping the code for now for future reference
  res$convenience$attributesVars <- NA;

  ### 2024-05-29: Given what we just discovered about merge_utterances_and_attributes()
  ### this is 'reactivated' again
  res$convenience$attributesVars <-
    sort(unique(c(unlist(lapply(
      res$parsedSource,
      function(x) {
        return(x$convenience$attributeVars);
      }
    )))));

    # sort(unique(c(unlist(lapply(purrr::map(res$parsedSource,
    #                                        'convenience'),
    #                             function(x) {
    #                               return(x$attributesVars);
    #                             })))));

         # codings = purrr::map(res$parsedSources,
         #                      'codings'),
         # attributes = purrr::map(res$parsedSources,
         #                       'attributes'));

  ### Get a list of all names of codes (usually just 'codes', but
  ### in theory, people could use multiple types of code)
  codeNames <- unique(unlist(lapply(res$convenience$rawCodings, function(x) {
    return(names(x));
  })));

  if (!silent) {
    cat0("Found codes with names ", vecTxtQ(codeNames), ".\n");
  }

  res$inductiveSplitCodes <-
    lapply(codeNames,
           function(codeName) {
             return(list(unlist(lapply(res$parsedSources,
                                       function(parsedSource) {
                                         return(parsedSource$convenience$inductiveSplitCodes[[codeName]]);
                                      }),
                                recursive=FALSE)));

             # ### Get used codes for this 'code type'
             # usedCodes <-
             #   unique(unlist(lapply(res$parsedSources,
             #                 function(x) {
             #                   return(x$rawCodings[[codeRegex]]);
             #                 })));
             #
             # ### Process inductive code trees
             # tmpRes <-
             #   inductiveCodes_to_tree(inductiveCodes=usedCodes,
             #                          codeRegex=codeRegex,
             #                          inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker,
             #                          silent=silent);
             #
             # res <-
             #   list(inductiveCodeProcessing = tmpRes$inductiveCodeProcessing[[codeRegex]],
             #        inductiveCodeTrees = tmpRes$inductiveCodeTrees[[codeRegex]],
             #        inductiveDiagrammeR = tmpRes$inductiveDiagrammeR[[codeRegex]],
             #        codingLeaves = tmpRes$codingLeaves[[codeRegex]],
             #        codings = tmpRes$codings[[codeRegex]]);
             #
             # return(res);
           });

  res$inductiveSplitCodes <-
    lapply(res$inductiveSplitCodes,
           function(x) {
             return(unname(unlist(x,
                                  recursive=FALSE)));
           });

  names(res$inductiveSplitCodes) <-
    codeNames;

  if (!silent) {
    cat0("Successfully extracted and combined the inductive codes from each source. Starting building the inductive code tree.\n");
  }

  res$inductiveCodeTrees <-
    lapply(res$inductiveSplitCodes,
           function(x) {
             if (length(x) > 0) {
               return(inductiveCodes_to_tree(x,
                                             silent=silent));
             } else {
               return(NA);
             }
           });

  res$convenience$valid_inductiveCodeTrees <-
    which(unlist(lapply(res$inductiveCodeTrees, is.environment)));

  res$convenience$original_inductiveCodeTreeNames <-
    names(res$inductiveCodeTrees);

  res$inductiveCodeTrees <-
    res$inductiveCodeTrees[res$convenience$valid_inductiveCodeTrees];

  if (length(res$convenience$valid_inductiveCodeTrees) > 0) {

    res$inductiveCodeTreeGraphs <-
      lapply(
        res$inductiveCodeTrees,
        function(tree) {
          tree$root$Set(name = 'codes',
                        filterFun=function(x) x$isRoot);
          res <- data.tree::ToDiagrammeRGraph(tree);

          res <-
            do.call(
              rock::apply_graph_theme,
              c(list(graph = res),
                rock::opts$get("theme_codeTreeDiagram"))
            );

          # res <-
          #   apply_graph_theme(res,
          #                     c("layout", "dot", "graph"),
          #                     c("rankdir", "LR", "graph"),
          #                     c("outputorder", "edgesfirst", "graph"),
          #                     c("fixedsize", "false", "node"),
          #                     c("shape", "box", "node"),
          #                     c("style", "rounded,filled", "node"),
          #                     c("fontname", "Arial", "node"),
          #                     c("color", "#000000", "node"),
          #                     c("color", "#888888", "edge"),
          #                     c("dir", "none", "edge"),
          #                     c("headclip", "false", "edge"),
          #                     c("tailclip", "false", "edge"),
          #                     c("fillcolor", "#FFFFFF", "node"));
          return(res);
        }
      );

    class(res$inductiveCodeTreeGraphs) <- c(
      "rock_graphList",
      class(res$inductiveCodeTreeGraphs)
    );

    if (!silent) {
      cat0("Successfully built the inductive code trees. Merging source dataframes.\n");
    }

  } else {

    if (!silent) {
      cat0("No inductive code trees found/combined. Merging source dataframes.\n");
    }

  }

  ###---------------------------------------------------------------------------

  ### Merge source dataframes
  res$sourceDf <-
    rbind_df_list(
      lapply(
        res$parsedSources,
        function(x) {
          return(x$sourceDf);
        }
      )
    );

    # dplyr::bind_rows(purrr::map(res$parsedSources,
    #                             'sourceDf'));

  ### Merge merged source dataframes

  ### 2024-05-29: Replacing this with a new merging activity where we merge
  ### the attributes in from the new res$convenience$attributes object using
  ### the res$convenience$allClassIds
#
#   res$qdt <-
#     res$sourceDf;
#
#   for (currentClassId in res$convenience$allClassIds) {
#     if (currentClassId %in% names(res$qdt)) {
#
#     }
#   }

  res$qdt <-
    rbind_df_list(
      lapply(
        names(res$parsedSources),
        function(i) {
          if (is.data.frame(res$parsedSources[[i]]$qdt) &&
              nrow(res$parsedSources[[i]]$qdt) > 0) {
            tmpRes <- res$parsedSources[[i]]$qdt;
            tmpRes$originalSource <- i;
            return(tmpRes);
          } else {
            return(NULL);
          }
        }
      )
    );

    # dplyr::bind_rows(purrr::map(lapply(res$parsedSources,
    #                                    function(x) {
    #                                      if (is.data.frame(x$mergedSourceDf)) {
    #                                        return(x);
    #                                      } else {
    #                                        x$mergedSourceDf <-
    #                                          NULL;
    #                                        return(x);
    #                                      }
    #                                    }),
    #                             'mergedSourceDf'),
    #                  .id="originalSource");

  res$qdt[, res$convenience$codingLeaves] <-
    lapply(res$qdt[, res$convenience$codingLeaves, drop=FALSE],
           function(x) {
             return(ifelse(is.na(x),
                           0,
                           x));
           });

  if (!silent) {
    cat0("Merged all source dataframes together and set NA occurrences to 0.\n");
  }

  ###--------------------------------------------------------------------------
  ### Now look in the returned objects for generic information and structure
  ### the result better
  ###--------------------------------------------------------------------------

  ### Pre-yum bit; keeping it for now just in case

  # yamlLineSets <-
  #   purrr::map(res$parsedSources,
  #              'yamlFragments');
  #
  # yamlLineSets <-
  #   unlist(yamlLineSets,
  #          recursive = FALSE);
  #
  # yamlLineSets <-
  #   lapply(yamlLineSets,
  #          paste,
  #          collapse="\n");
  #
  # if (!silent) {
  #   cat0("Extracted the following YAML fragments:\n\n",
  #             paste0(unlist(yamlLineSets),
  #                    collapse="\n\n"));
  # }
  #
  # rawSpecs <-
  #   res$rawSpecs <-
  #   yum::load_yaml_list(yamlLineSets);
  #
  # if (!silent) {
  #   print(glue::glue("\n\nLoaded {length(rawSpecs)} raw attributes specifications.\n"));
  # }
  #
  # ### Get the attributes
  # attributesList <- list();
  # for (currentattributesContainer in attributesContainers) {
  #   attributesList <-
  #     c(attributesList,
  #       unlist(purrr::map(rawSpecs,
  #                         currentattributesContainer),
  #              recursive=FALSE));
  # }
  #
  # ### Add type and convert to data frame
  # attributesDfs <-
  #   lapply(attributesList,
  #          function(x) {
  #            x$type <-
  #              names(idRegexes)[names(idRegexes) %in% names(x)];
  #            return(as.data.frame(x,
  #                                 stringsAsFactors=FALSE));
  #          });
  #
  # ### Bind together into one dataframe
  # res$attributes <-
  #   attributesDf <-
  #   dplyr::bind_rows(attributesDfs);

  if (!silent) {
    cat0("Creating attributes dataframe and merging with source dataframe.\n");
  }

  attributesDf <-
    res$attributesDf <-
    # dplyr::bind_rows(purrr::map(res$parsedSources,
    #                             'attributesDf'));
    rbind_df_list(
      lapply(
        res$parsedSources,
        function(parsedSource) {
          return(parsedSource$attributesDf);
        }
      )
    );

  unspecifiedClasses <-
    unique(
      unlist(
        lapply(
          res$parsedSources,
          function(currentParsedSource) {
            return(currentParsedSource$convenience$unspecifiedClasses);
            }
        )
      )
    );

  specifiedClasses <-
    unique(
      unlist(
        lapply(
          res$parsedSources,
          function(currentParsedSource) {
            return(currentParsedSource$convenience$specifiedClasses);
          }
        )
      )
    );

  allClasses <-
    c(specifiedClasses, unspecifiedClasses);

  ### Merge attributes with source dataframe
  if (nrow(attributesDf) > 0) {

    qdtNew <-
      merge_utterances_and_attributes(
        qdt = res$qdt,
        classes = allClasses,
        attributesDf = res$attributesDf,
        checkClassInstanceIds = checkClassInstanceIds,
        suppressDuplicateInstanceWarnings = suppressDuplicateInstanceWarnings,
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
    #   if (names(idRegexes)[i] %in% names(attributesDf)) {
    #     if (!silent) {
    #       print(glue::glue("\nFor identifier class {names(idRegexes)[i]}, attributes were provided: proceeding to join to sources dataframe.\n"));
    #     }
    #     ### Convert to character to avoid errors and delete
    #     ### empty columns from merged source dataframe
    #     usedIdRegexes <-
    #       names(idRegexes)[names(idRegexes) %in% names(attributesDf)];
    #     for (j in usedIdRegexes) {
    #       attributesDf[, j] <-
    #         as.character(attributesDf[, j]);
    #     }
    #     for (j in intersect(names(res$qdt),
    #                         names(attributesDf))) {
    #       if (all(is.na(res$qdt[, j]))) {
    #         res$qdt[, j] <- NULL;
    #       }
    #     }
    #
    #     if (!(names(idRegexes)[i] %in% names(res$qdt))) {
    #       msg <-
    #         paste0("When processing identifier regex '", idRegexes[i],
    #                "', I failed to find its name ('", names(idRegexes[i]),
    #                "') in the column names of the merged ",
    #                "sources data frame (",
    #                vecTxtQ(names(res$qdt)), "), so not merging ",
    #                "the attributes data frame with the source data frame for ",
    #                "this class instance identifier..");
    #       if (checkClassInstanceIds) {
    #         warning(msg);
    #       }
    #       if (!silent) {
    #         cat(msg);
    #       }
    #     } else if (!(names(idRegexes)[i] %in% setdiff(names(attributesDf), 'type'))) {
    #       msg <-
    #         paste0("When processing identifier regex '", idRegexes[i],
    #                "', I failed to find its name (", names(idRegexes[i]),
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
    #
    #       attributesToLookFor <-
    #         setdiff(
    #           names(attributesDf),
    #           names(idRegexes)[i]
    #         );
    #
    #       alreadyPresentAttributeIndices <-
    #         attributesToLookFor %in% names(res$qdt);
    #
    #       alreadyPresentAttributes <-
    #         attributesToLookFor[alreadyPresentAttributeIndices];
    #
    #       if (any(alreadyPresentAttributeIndices)) {
    #
    #         if (!silent) {
    #           cat0("\n\nOne or more attribute columns already exist in the merged ",
    #                "source data frame. To be safe, proceeding to check whether ",
    #                "after merging again, the results are the same.");
    #         }
    #
    #         testDf <-
    #           dplyr::left_join(res$qdt,
    #                            attributesDf[, setdiff(names(attributesDf), 'type')],
    #                            by=names(idRegexes)[i]);
    #
    #         for (i in alreadyPresentAttributes) {
    #
    #           if (i %in% names(testDf)) {
    #
    #             if (res$qdt[, i] != testDf[, i]) {
    #
    #               cat("\nFound a difference in column ", i, ".");
    #               stop("Found a difference in column ", i, ".");
    #
    #             }
    #
    #           } else {
    #
    #             if (!silent) {
    #               cat0("\nColumn ", i, " does not exist in the newly merged ",
    #                    "data frame.");
    #             }
    #
    #           }
    #
    #         }
    #
    #         if (!silent) {
    #           cat0("\nAll columns that existed in both data frames are ",
    #                "the same. Not performing the merge of the attribute ",
    #                "data frame and the source data frame.");
    #         }
    #
    #         #res$mergedSourceDf <- testDf;
    #
    #       } else {
    #
    #         # attributesDf[, names(idRegexes)[i]] <-
    #         #   as.character(attributesDf[, names(idRegexes)[i]]);
    #         ### Join attributes based on identifier
    #         res$qdt <-
    #           dplyr::left_join(
    #             res$qdt,
    #             attributesDf[, setdiff(names(attributesDf), 'type')],
    #             by=names(idRegexes)[i]
    #           );
    #
    #         res$mergedSourceDf <-
    #           res$qdt;
    #       }
    #
    #     }
    #
    #   } else {
    #     if (!silent) {
    #       print(glue::glue("\nFor identifier class {names(idRegexes)[i]}, no attributes were provided.\n"));
    #     }
    #   }
    # }

    if (!silent) {
      cat0("\nFinished merging attributes with source dataframe. Starting to collect deductive code trees.\n");
    }

    ###---------------------------------------------------------------------------
    ###
    ### END --- move this to a separate function for parse_source and parse_sources
    ###
    ###---------------------------------------------------------------------------

  }

  res$mergedSourceDf <- res$qdt

  ###---------------------------------------------------------------------------

  deductiveCodeLists <-
    do.call(c,
            purrr::map(res$parsedSources,
                       'rawDeductiveCodes'));
    # yum::load_yaml_list(yamlLineSets,
    #                     select=paste0(codesContainers, sep="|"));

  if (is.null(deductiveCodeLists)) {
    res$deductiveCodeTrees <- NA;
    if (!silent) {
      cat0("No deductive code trees found.\n");
    }
  } else {

    if (!silent) {
      cat0("Specifications of deductive code trees found: combining them into actual tree.\n");
    }

    class(deductiveCodeLists) <-
      "simplifiedYum";

    res$deductiveCodeTrees <-
      yum::build_tree(deductiveCodeLists);

    res$deductiveCodeTrees$root$Set(
      name = 'codes',
      filterFun=function(x) x$isRoot
    );

    res$deductiveCodeTreeGraph <-
      data.tree::ToDiagrammeRGraph(res$deductiveCodeTrees);

    res$deductiveCodeTreeGraph <-
      do.call(
        rock::apply_graph_theme,
        c(list(graph = res$deductiveCodeTreeGraph),
          rock::opts$get("theme_codeTreeDiagram"))
      );

      # apply_graph_theme(res$deductiveCodeTreeGraph,
      #                   c("layout", "dot", "graph"),
      #                   c("rankdir", "LR", "graph"),
      #                   c("outputorder", "edgesfirst", "graph"),
      #                   c("fixedsize", "false", "node"),
      #                   c("shape", "box", "node"),
      #                   c("style", "rounded,filled", "node"),
      #                   c("color", "#000000", "node"),
      #                   c("color", "#888888", "edge"),
      #                   c("dir", "none", "edge"),
      #                   c("headclip", "false", "edge"),
      #                   c("tailclip", "false", "edge"),
      #                   c("fillcolor", "#FFFFFF", "node"));

    if (!silent) {
      cat0("Successfully combined deductive code tree specifications into actual tree. Starting merging with inductive code trees.\n");
    }

  }

  if ("Node" %in% class(res$deductiveCodeTrees)) {

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

    if (!silent) {
      cat0("Successfully merged deductive code tree with inductive code trees.\n");
    }

  } else {
    res$extendedDeductiveCodeTrees <- NA;

    if (length(res$inductiveCodeTrees) == 1) {
      res$fullyMergedCodeTrees <- res$inductiveCodeTrees[[1]];
    } else {
      if (mergeInductiveTrees) {
        warning("Multiple inductive code trees found; functionality to merge ",
                "these currently not yet implemented. Setting ",
                "`fullyMergedCodeTrees` to NA (missing).");
      }
      res$fullyMergedCodeTrees <- NA;
    }
  }
  if (!silent) {
    cat("\n\n");
  }

  if ("Node" %in% class(res$fullyMergedCodeTrees)) {
    res$convenience$codingPaths <- c();
    res$convenience$codingPaths <-
      gsub("/",
           codeTreeMarker,
           res$fullyMergedCodeTrees$Get("pathString"));
  } else {
    ###------------------------------------------------------------------------
    ### This needs to be fixed to properly work with multiple parallel coding
    ### systems
    ###------------------------------------------------------------------------
    if ("Node" %in% class (res$deductiveCodeTrees)) {
      res$convenience$codingPaths <-
        gsub("/",
             codeTreeMarker,
             res$deductiveCodeTrees$Get("pathString"));
      ### Not needed, because the names are already the node names
      # res$convenience$codingPaths <-
      #   codePaths_to_namedVector(
      #     res$convenience$codingPaths
      #   );
    } else {
      if (any(!is.na(res$inductiveCodeTrees))) {
        res$convenience$codingPaths <- c();
        for (i in names(res$inductiveCodeTrees)) {
          res$convenience$codingPaths <-
            c(res$convenience$codingPaths,
              gsub("/",
                   codeTreeMarker,
                   res$inductiveCodeTrees[[i]]$root$Get("pathString")));
        }
        ### Not needed, because the names are already the node names
        # res$convenience$codingPaths <-
        #   codePaths_to_namedVector(
        #     res$convenience$codingPaths
        #   );
      }
    }
  }

  # ### Get the codes
  # deductiveCodeLists <- list();
  # for (currentCodesContainer in codesContainers) {
  #   deductiveCodeLists[[currentCodesContainer]] <-
  #     purrr::map(rawSpecs,
  #                1,
  #                currentCodesContainer);
  #   if (length(deductiveCodeLists[[currentCodesContainer]]) > 0) {
  #     deductiveCodeLists[[currentCodesContainer]] <-
  #       deductiveCodeLists[[currentCodesContainer]][
  #         !unlist(lapply(deductiveCodeLists[[currentCodesContainer]],
  #                        is.null))
  #       ];
  #   }
  #   print(length(deductiveCodeLists[[currentCodesContainer]]))
  # }
  # res$deductiveCodeList <-
  #   deductiveCodeList <-
  #   do.call(c,
  #           deductiveCodeLists);
  #
  # if (length(res$deductiveCodeList) > 0) {
  #   res$deductiveCodeTree <-
  #     codes_to_nodes(res$deductiveCodeList,
  #                    silent=silent);
  #   res$deductiveCodeTree$root$Set(name = 'codes',
  #                                  filterFun=function(x) x$isRoot);
  #   res$deductiveCodeTreeGraph <-
  #     data.tree::ToDiagrammeRGraph(res$deductiveCodeTree);
  #   res$deductiveCodeTreeGraph <-
  #     apply_graph_theme(res$deductiveCodeTreeGraph,
  #                       c("layout", "dot", "graph"),
  #                       c("rankdir", "LR", "graph"),
  #                       c("outputorder", "nodesfirst", "graph"),
  #                       c("fixedsize", "false", "node"),
  #                       c("shape", "box", "node"),
  #                       c("style", "rounded,filled", "node"),
  #                       c("color", "#000000", "node"),
  #                       c("color", "#888888", "edge"),
  #                       c("dir", "none", "edge"),
  #                       c("fillcolor", "#FFFFFF", "node"));
  #
  # } else {
  #   res$deductiveCodeTree <- NULL;
  #   res$deductiveCodeTreeGraph <- NULL;
  # }

  return(structure(res,
                   class="rock_parsedSources"));

}

#' @rdname parsing_sources
#' @method print rock_parsedSources
#' @export
print.rock_parsedSources <- function(x, prefix="### ",  ...) {
  sourceFileNames <- names(x$parsedSources);
  print(glue::glue("Parsed {length(sourceFileNames)} sources, with filenames ",
                   "{vecTxtQ(sourceFileNames)}."));
  print(graphics::plot(x));
  invisible(x);
}

#' @rdname parsing_sources
#' @method plot rock_parsedSources
#' @export
plot.rock_parsedSources <- function(x, ...) {
  if (!is.null(x$deductiveCodeTreeGraph)) {
    return(DiagrammeR::render_graph(x$deductiveCodeTreeGraph));
  } else {
    return(glue::glue("\nThese parsed sources do not contain a deductive code tree.\n"));
  }
}
