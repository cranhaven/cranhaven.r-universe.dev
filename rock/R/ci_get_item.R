#' Get an item in a specific language
#'
#' This function takes a Narrative Response Model specification as used in
#' NRM-based cognitive interviews, and composes an item based on the
#' specified template for that item, the specified stimuli, and the
#' requested language.
#'
#' @param nrm_spec The Narrative Response Model specification.
#' @param item_id The identifier of the requested item.
#' @param language The language of the stimuli.
#'
#' @return A character value with the item.
#' @export
ci_get_item <- function(nrm_spec,
                        item_id,
                        language) {

  if (!inherits(nrm_spec, "rock_nrm_spec")) {
    stop("As `nrm_spec`, pass a Narrative Response Model specification ",
         "as produced by a call to rock::ci_import_nrm_spec().");
  }

  nrm_wsNames <- rock::opts$get("nrm_wsNames");
  nrm_colNames <- rock::opts$get("nrm_colNames");
  ci_template_replacementDelimiters <-
    rock::opts$get("ci_template_replacementDelimiters");

  ### Get some convenient dataframes

  instrumentDf <-
    nrm_spec[[nrm_wsNames$instrument]];

  stimuliDf <-
    nrm_spec[[nrm_wsNames$stimuli]];

  ### Select general stimuli in this language

  generalDf <-
    stimuliDf[
      stimuliDf[[nrm_colNames$stimuli['stimulus_language']]] == language &
        stimuliDf[[nrm_colNames$stimuli['item_id']]] == "general"
      ,
    ];

  ### Select item-specific stimuli in this language

  stimuliDf <-
    stimuliDf[
      stimuliDf[[nrm_colNames$stimuli['stimulus_language']]] == language &
        stimuliDf[[nrm_colNames$stimuli['item_id']]] == item_id
      ,
    ];

  ### Get template for this item

  template <-
    instrumentDf[
      instrumentDf[[nrm_colNames$instrument['item_id']]] == item_id,
      nrm_colNames$instrument['item_template_nrm']
    ];

  ### Perform any replacements that are required based on
  ### stimulus aliases that are specified in the template

  regex <- paste0(ci_template_replacementDelimiters[1],
                  "([a-zA-Z0-9_]*)",
                  ci_template_replacementDelimiters[2]);

  matchObject <- gregexpr(regex, template);

  replacementColumns <- regmatches(template, matchObject)[[1]];

  replacementColumns <- gsub(regex, "\\1", replacementColumns);

  replacementValues <- c();
  for (i in seq_along(replacementColumns)) {
    replacementValues[i] <-
      ifelse(
        replacementColumns[i] %in%
          stimuliDf[[nrm_colNames$stimuli['stimulus_alias']]],
        stimuliDf[stimuliDf[[nrm_colNames$stimuli['stimulus_alias']]] == replacementColumns[i],
                  nrm_colNames$stimuli['stimulus_content']],
        ifelse(
          replacementColumns[i] %in%
            generalDf[[nrm_colNames$stimuli['stimulus_alias']]],
          generalDf[
            generalDf[[nrm_colNames$stimuli['stimulus_alias']]] == replacementColumns[i],
            nrm_colNames$stimuli['stimulus_content']],
          ""
        )
      );
  }

  if (!is.null(replacementValues)) {
    regmatches(template, matchObject) <- list(replacementValues);
  }

  ### Return the result

  return(template);


}
