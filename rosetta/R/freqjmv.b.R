
# This file is a generated template, your changes will not be overwritten

freqjmvClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "freqjmvClass",
    inherit = freqjmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ((length(self$options$vector) > 0) &&
                (self$options$vector %in% names(self$data)) &&
                (length(na.omit(self$data[, self$options$vector])) > 0)) {

                res <- rosetta::freq(self$data[, self$options$vector])$dat;
                names(res) <- c("freq", "percTotal", "percValid", "cumulative");

                # self$results$text$setContent(res);

                res$value <- row.names(res);
                res <- res[, c("value",
                               "freq",
                               "percTotal",
                               "percValid",
                               "cumulative")];

                for (i in seq_len(nrow(res))) {
                  if (i > 4) {
                      # self$results$table$addRow(rowKey=paste0("row", i),
                      #                           values=as.list(res[nrow(res), 1:4]));
                      self$results$table$addRow(rowKey=paste0("row", i),
                                                values=as.list(res[i, ]));
                  } else {
                      self$results$table$setRow(rowNo=i,
                                                values=as.list(res[i, ]));
                  }
                }

#
#                 self$results$table$setNote(key = "Total",
#                                            note = paste0("These responses were provided by ",
#                                                          nrOfCases, " cases."));

            }

        })
)
