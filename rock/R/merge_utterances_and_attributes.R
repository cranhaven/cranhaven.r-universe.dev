merge_utterances_and_attributes <- function(qdt,
                                            classes,
                                            attributesDf,
                                            checkClassInstanceIds = FALSE,
                                            silent = rock::opts$get(silent)) {


  ### Reconsider this -> prohibits errors from being sent.

  if (is.null(classes) || all(is.na(classes)) || (length(classes) == 0)) {
    return(qdt);
  }

  ### Add attributes to the utterances
  for (className in classes) {
    ### Check whether attributes was provided for this identifier
    if (className %in% names(attributesDf)) {
      if (!silent) {
        print(glue::glue("\n\nFor class ", className, ", attributes were provided: proceeding to merge.\n"));
      }
      ### Convert to character to avoid errors and delete
      ### empty columns from merged source dataframe
      usedClasses <-
        className[className %in% names(attributesDf)];
      for (j in usedClasses) {
        attributesDf[, j] <-
          as.character(attributesDf[, j]);
      }
      for (j in intersect(names(qdt),
                          names(attributesDf))) {
        if (all(is.na(qdt[, j]))) {
          qdt[, j] <- NULL;
        }
      }

      if (!(className %in% names(qdt))) {
        msg <-
          paste0("When processing identifier regex '", className,
                 "', I failed to find it in the column names of the merged ",
                 "sources data frame (",
                 vecTxtQ(names(qdt)), "), so not merging ",
                 "the attributes data frame with the source data frame for ",
                 "this class instance identifier.")
        if (checkClassInstanceIds) {
          warning(msg);
        }
        if (!silent) {
          cat(msg);
        }
      } else if (!(className %in% setdiff(names(attributesDf), 'type'))) {
        msg <-
          paste0("When processing identifier regex '", className,
                 "', I failed to find it in the column names of the merged ",
                 "attributes data frame, so not merging ",
                 "the attributes data frame with the source data frame for ",
                 "this class instance identifier..");
        if (checkClassInstanceIds) {
          warning(msg);
        }
        if (!silent) {
          cat(msg);
        }
      } else {
        # attributesDf[, names(idRegexes)[i]] <-
        #   as.character(attributesDf[, names(idRegexes)[i]]);
        ### Join attributes based on identifier

        qdt <-
          dplyr::left_join(qdt,
                           attributesDf[, setdiff(names(attributesDf), 'type')],
                           by=className,
                           relationship = "many-to-many");
      }

    } else {
      if (!silent) {
        print(glue::glue("\nFor identifier class {className}, no attributes were provided.\n"));
      }
    }
  }

  return(qdt);

}
