ci_create_interviewScheme <- function(nrm_spec,
                                      language = "en") {

  if (!inherits(nrm_spec, "rock_nrm_spec")) {
    stop("As `nrm_spec`, pass a Narrative Response Model specification ",
         "as produced by a call to rock::ci_import_nrm_spec().");
  }

  rpe_mq_idName <- rock::opts$get('rpe_mq_idName');
  nrm_wsNames <- rock::opts$get("nrm_wsNames");
  nrm_colNames <- rock::opts$get("nrm_colNames");

  ### Create dataframes for convenience

  instrumentDf <-
    nrm_spec[[nrm_wsNames$instrument]];

  stimuliDf <-
    nrm_spec[[nrm_wsNames$stimuli]];

  probeDf <-
    nrm_spec[[nrm_wsNames$probes]];

  ### Get item identifiers in the right order

  itemIds <-
    instrumentDf[, nrm_colNames$instrument['item_id']][
      order(
        as.numeric(
          instrumentDf[, nrm_colNames$instrument['item_sequence']]
        )
      )
    ];

  ### Start composing result

  res <- character();

  for (currentItem in itemIds) {

    res <-
      c(
        res,
        "\n\n",
        "--<<item_break>>--\n",
        "\n### Item text:\n\n",
        paste0(
          stimuliDf[nrm_colNames$stimuli['stimulus_content']][
            stimuliDf[nrm_colNames$stimuli['item_id']] == currentItem &
              stimuliDf[nrm_colNames$stimuli['stimulus_language']] == language
          ],
          collapse = " "
        ),
        paste0("   [[uiid:", currentItem, "]]\n"),
        "\n### Think-aloud notes:\n\n\n",
        "\n### Probes:\n\n",
        paste0(
          probeDf[[nrm_colNames$probes['probe_label']]][
            probeDf[[nrm_colNames$probes['item_id']]] == currentItem
          ],
          "   [[", rpe_mq_idName, ":",
          probeDf[[nrm_colNames$probes['probe_id']]][
            probeDf[[nrm_colNames$probes['item_id']]] == currentItem
          ],
          "]]\n",
          collapse = "\n\n\n"
        ),
        "\n\n\n\n"
      );

  }

  res <- paste0(res, collapse="");

  return(res);

}
