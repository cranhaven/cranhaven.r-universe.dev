merge_utterances_and_attributes <- function(qdt,
                                            classes,
                                            attributesDf,
                                            checkClassInstanceIds = FALSE,
                                            suppressDuplicateInstanceWarnings = rock::opts$get('suppressDuplicateInstanceWarnings'),
                                            silent = rock::opts$get(silent)) {


  ### Reconsider this -> prohibits errors from being sent.

  if (is.null(classes) || all(is.na(classes)) || (length(classes) == 0)) {
    return(qdt);
  }

  resQdt <- qdt;

  ### Add attributes to the utterances
  for (className in classes) {
    ### Check whether attributes was provided for this identifier
    if (className %in% names(attributesDf)) {
      if (!silent) {
        print(glue::glue("\n\nFor class ", className, ", attributes were provided: proceeding to merge.\n"));
      }


      ### 2024-05-29: Refactoring to make this work for multiple classes with
      ### attributes when called from parse_sources()

      currentClassId <- classId <- className;

      subDf_forClass <- attributesDf[!is.na(attributesDf[[classId]]), ]

      subDf_forClass <-
        subDf_forClass[
          ,
          unlist(
            apply(
              subDf_forClass,
              2,
              function(x) {
                if(all(is.na(x)))
                  return(FALSE)
                else
                  return(TRUE)
              }
            )
          )
        ];

      unduplicated_subDf_forClass <-
        do.call(
          rbind,
          lapply(
            sort(unique(subDf_forClass[[currentClassId]])),
            function(instanceId) {

              instanceDf <-
                subDf_forClass[
                  subDf_forClass[[currentClassId]] == instanceId,
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

      resQdt <-
        merge(
          resQdt,
          unduplicated_subDf_forClass,
          all.x = TRUE,
          all.y = FALSE
        );

      ### Sort again by original sequence identifier; merge mixes the order
      resQdt <-
        resQdt[
          order(resQdt$originalSource,
                resQdt$originalSequenceNr,
                decreasing = FALSE),
        ];

      ### 2024-05-29: Commented out -- BEGINNING

      # ### Convert to character to avoid errors and delete
      # ### empty columns from merged source dataframe
      # usedClasses <-
      #   className[className %in% names(attributesDf)];
      # for (j in usedClasses) {
      #   attributesDf[, j] <-
      #     as.character(attributesDf[, j]);
      # }
      # for (j in intersect(names(qdt),
      #                     names(attributesDf))) {
      #   if (all(is.na(qdt[, j]))) {
      #     qdt[, j] <- NULL;
      #   }
      # }
      #
      # if (!(className %in% names(qdt))) {
      #   msg <-
      #     paste0("When processing identifier regex '", className,
      #            "', I failed to find it in the column names of the merged ",
      #            "sources data frame (",
      #            vecTxtQ(names(qdt)), "), so not merging ",
      #            "the attributes data frame with the source data frame for ",
      #            "this class instance identifier.")
      #   if (checkClassInstanceIds) {
      #     warning(msg);
      #   }
      #   if (!silent) {
      #     cat(msg);
      #   }
      # } else if (!(className %in% setdiff(names(attributesDf), 'type'))) {
      #   msg <-
      #     paste0("When processing identifier regex '", className,
      #            "', I failed to find it in the column names of the merged ",
      #            "attributes data frame, so not merging ",
      #            "the attributes data frame with the source data frame for ",
      #            "this class instance identifier..");
      #   if (checkClassInstanceIds) {
      #     warning(msg);
      #   }
      #   if (!silent) {
      #     cat(msg);
      #   }
      # } else {
      #   # attributesDf[, names(idRegexes)[i]] <-
      #   #   as.character(attributesDf[, names(idRegexes)[i]]);
      #   ### Join attributes based on identifier
      #
      #   qdt <-
      #     dplyr::left_join(qdt,
      #                      attributesDf[, setdiff(names(attributesDf), 'type')],
      #                      by=className,
      #                      relationship = "many-to-many");
      #
      # }

      ### 2024-05-29: Commented out -- ENDING

    } else {
      if (!silent) {
        print(glue::glue("\nFor identifier class {className}, no attributes were provided.\n"));
      }
    }
  }

  if (nrow(qdt) != nrow(resQdt)) {
    browser();
    stop("When merging the attributes and data (the utterances), something strange happened: ",
         "the dataframe with utterances had ", qdt, " rows, whereas the dataframe ",
         "merged with the attributes has ", resQdt, " rows. That can't be right. However, ",
         "no solution has been implemented as yet; nor has more detailed diagnostic information. ",
         "My apologies! Of course, if this actually ever occurs, that's a good reason (and provides ",
         "a use case to test with) to look into this. Yay.");
  }

  return(resQdt);

}
