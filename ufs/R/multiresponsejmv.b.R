
# This file is a generated template, your changes will not be overwritten

multiResponsejmvClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "multiResponsejmvClass",
    inherit = multiResponsejmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if (length(self$options$items) > 0) {

                res <-
                    ufs::multiResponse(data = self$data,
                                       items = self$options$items,
                                       endorsedOption = self$options$endorsedOption);

                nrOfCases <-
                  gsub(".*\\((\\d+)\\).*", "\\1", utils::tail(names(res), 1));

                names(res) <- c("var", "freq", "percentageResp", "percentageCases");

                for (i in seq_len(nrow(res)-1)) {
                  self$results$table$setRow(rowNo=i,
                                            values=as.list(res[i, 2:4]));
                }

                self$results$table$addRow(rowKey="Total",
                                          values=as.list(res[nrow(res), 1:4]));

                self$results$table$setCell(rowNo=nrow(res),
                                           col=1,
                                           value="Total:")

                self$results$table$setNote(key = "Total",
                                           note = paste0("These responses were provided by ",
                                                         nrOfCases, " cases."));

            }

        })
)
