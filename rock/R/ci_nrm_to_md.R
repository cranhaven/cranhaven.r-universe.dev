ci_nrm_to_md <- function(nrm_spec,
                         language,
                         includeProbes = FALSE,
                         headingLevel = 2) {

  nrm_wsNames <- rock::opts$get("nrm_wsNames");
  nrm_colNames <- rock::opts$get("nrm_colNames");

  if (!inherits(nrm_spec, "rock_nrm_spec")) {
    stop("As `nrm_spec`, pass a Narrative Response Model specification ",
         "as produced by a call to rock::ci_import_nrm_spec().");
  }

  if (!(nrm_wsNames$responsemodels %in% names(nrm_spec))){
    stop("To produce a MarkDown overview of the response models, you must ",
         "have a worksheet containing the responsemodels (which by default ",
         "must be named 'responsemodels' and must contain columns 'item_id', ",
         "'responsemodel_id', and 'responsemodel_label', and 'comments').");
  }

  responsemodelDf <-
    nrm_spec[[nrm_wsNames$responsemodels]];

  instrumentDf <-
    nrm_spec[[nrm_wsNames$instrument]];

  stimuliDf <-
    nrm_spec[[nrm_wsNames$stimuli]];
  stimuliDf <-
    stimuliDf[
      stimuliDf[[nrm_colNames$stimuli['stimulus_language']]] == language
      ,
    ];

  probeDf <-
    nrm_spec[[nrm_wsNames$probes]];

  ### Heading

  if (is.null(nrm_spec$metadata) ||
      (!("instrument_name" %in% names(nrm_spec$metadata)))) {
    res <- heading("Narrative Response Model (",
                   language,
                   ") {.tabset .tabset-pills}",
                   headingLevel = headingLevel,
                   cat = FALSE);
  } else {
    res <- heading("Narrative Response Model for ",
                   nrm_spec$metadata$instrument_name,
                   " (", language,
                   ") {.tabset .tabset-pills}",
                   headingLevel = headingLevel,
                   cat = FALSE);
  }

  ### Add prototype stuff
  prototype_present <-
    nrm_wsNames$responsemodel_prototype %in% names(nrm_spec);

  if (prototype_present){

    prototype <-
      nrm_spec[[nrm_wsNames$responsemodel_prototype]];

    prototype_id <-
      prototype[
        ,
        nrm_colNames$responsemodel_prototype['responsemodel_id']
      ];

    prototype_sequence<-
      prototype[
        ,
        nrm_colNames$responsemodel_prototype['responsemodel_sequence']
      ];

    prototype_label <-
      prototype[
        ,
        nrm_colNames$responsemodel_prototype['responsemodel_label']
      ];

    prototype_comments <-
      prototype[,
                nrm_colNames$responsemodel_prototype['responsemodel_comments']
      ];

    names(prototype_label) <- prototype_id;
    names(prototype_comments) <- prototype_id;

    res <- c(res,
             heading(
               "Prototype",
               headingLevel = headingLevel + 1,
               cat = FALSE
             ));

    res <- c(
      res,
      paste0(
        "- ",
        prototype_sequence,
        ": ",
        prototype_label,
        " (`",
        prototype_id,
        ifelse(
          is.na(prototype_comments),
          "`",
          paste0("`; *", prototype_comments, "*")
        ),
        ")\n"
      ),
      collapse="\n"
    );
  }

  ### Response models per item
  itemIds <-
    instrumentDf[, nrm_colNames$instrument['item_id']];

  responsemodels <-
    lapply(itemIds,
           function(currentItemId) {
             res <- responsemodelDf[
               responsemodelDf[
                 ,
                 nrm_colNames$responsemodels['item_id']
               ] == currentItemId,
             ];
             if (prototype_present) {
               res <-
                 rbind_dfs(
                   res,
                   prototype
                 );
             }
             res <-
               res[
                 order(
                   res[[nrm_colNames$responsemodels['responsemodel_sequence']]]
                 ),
               ];
             return(res);
           });
  names(responsemodels) <-
    itemIds;

  ### Process all items

  for (currentItemId in itemIds) {

    res <- c(res,
             heading(
               currentItemId,
               headingLevel = headingLevel + 1,
               cat = FALSE
             ));

    tmpItem <-
      ci_get_item(
        nrm_spec = nrm_spec,
        language = language,
        item_id = currentItemId
      );

    tmpItem <-
      paste(
        "> ",
        tmpItem,
        collapse = "\n"
      );

    tmpItem <- gsub(
      "\n",
      "\n> ",
      tmpItem
    );

    res <- c(res,
             paste0("\n",
                    tmpItem,
                    "\n"));

    if (includeProbes) {

      # responseModelIds <-
      #   responsemodels[[currentItemId]][[
      #     nrm_colNames$responsemodels['responsemodel_id']
      #   ]];

      for (currentRow in 1:nrow(responsemodels[[currentItemId]])) {

        currentResponseModelId <-
          responsemodels[[currentItemId]][
            currentRow,
            nrm_colNames$responsemodels['responsemodel_id']
          ];

        ### Start with responsemodel step

        res <- c(
          res,
          paste0(
            "- ",
            responsemodels[[currentItemId]][
              currentRow,
              nrm_colNames$responsemodels['responsemodel_sequence']
            ],
            ": ",
            responsemodels[[currentItemId]][
              currentRow,
              nrm_colNames$responsemodels['responsemodel_label']
            ],
            " (`",
            responsemodels[[currentItemId]][
              currentRow,
              nrm_colNames$responsemodels['responsemodel_id']
            ],
            ifelse(
              is.na(
                responsemodels[[currentItemId]][
                  currentRow,
                  nrm_colNames$responsemodels['responsemodel_comments']
                ]
              ),
              "`",
              paste0("`; *", responsemodels[[currentItemId]][
                  currentRow,
                  nrm_colNames$responsemodels['responsemodel_comments']
                ],
                "*"
              )
            ),
            ")\n",
            collapse="\n"
          )
        );

        ### Probes

        probes <-
          probeDf[
            probeDf[[nrm_colNames$probes['item_id']]] == currentItemId &
              probeDf[[nrm_colNames$probes['responsemodel_id']]] == currentResponseModelId,
          ];

        if (nrow(probes) > 0) {

          res <- c(
            res,
            paste0(
              "    - ",
              probes[[
                nrm_colNames$probes['probe_label']
              ]], " (`",
              probes[[
                nrm_colNames$probes['probe_id']
              ]],
              "`; *",
              probes[[
                nrm_colNames$probes['probe_ambiguity']
              ]],
              "*)",
              collapse="\n"
            )
          );

        }

      }

    } else {

      res <- c(
        res,
        paste0(
          "- ",
          responsemodels[[currentItemId]][[
            nrm_colNames$responsemodels['responsemodel_sequence']
          ]],
          ": ",
          responsemodels[[currentItemId]][[
            nrm_colNames$responsemodels['responsemodel_label']
          ]],
          " (`",
          responsemodels[[currentItemId]][[
            nrm_colNames$responsemodels['responsemodel_id']
          ]],
          ifelse(
            is.na(responsemodels[[currentItemId]][[
              nrm_colNames$responsemodels['responsemodel_comments']
            ]]),
            "`",
            paste0("`; *", responsemodels[[currentItemId]][[
              nrm_colNames$responsemodels['responsemodel_comments'],
              "*"
            ]])
          ),
          ")\n",
          collapse="\n"
        )
      );

    }

  }

  class(res) <- c("ci_nrm_md", "character");

  return(res);

}

#' @export
#' @method print ci_nrm_md
print.ci_nrm_md <- function(x, ...) {
  cat(x,
      sep="\n");
}
