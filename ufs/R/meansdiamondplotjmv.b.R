
# This file is a generated template, your changes will not be overwritten

meansDiamondPlotjmvClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "meansDiamondPlotjmvClass",
    inherit = meansDiamondPlotjmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            items <- self$options$items;
            data <- jmvcore::select(self$data, items);
            conf.level <- self$options$conf.level;

            if ((conf.level <= 0) | (conf.level >= 100)) {
                conf.level <- 95;
            }

            totalRows <- nrow(data);
            validRows <- sum(complete.cases(data));

            prepped_plot <- FALSE;

            if ((length(items) > 0) && (validRows > 0)) {

                for (i in items) {
                    data[[i]] <- jmvcore::toNumeric(data[[i]]);
                }

                totalRows <- nrow(data);
                validRows <- sum(complete.cases(data));

                if (all(unlist(lapply(data, is.numeric)))) {

                    self$results$diamondPlot$setState(list(data=data,
                                                           options=self$options));

                    self$results$text$setContent(paste0("Diamond plot for the ",
                                                        conf.level,
                                                        "% confidence intervals for the means of ",
                                                        vecTxtQ(items),
                                                        " based on a dataset with ",
                                                        totalRows, " rows, ", validRows, " of which have complete data."));

                    prepped_plot <- TRUE;
                }
            }

            if (!prepped_plot) {
                self$results$text$setContent(paste0("Select one or more variables to ",
                                                    "generate the diamond plot."));
            }
        },
        .plot = function(diamondPlot, ...) {

            if (!is.null(diamondPlot$state)) {

                data <- diamondPlot$state$data;
                items <- diamondPlot$state$options$items;
                conf.level <- diamondPlot$state$options$conf.level;
                showData <- diamondPlot$state$options$showData;

                if ((conf.level <= 0) | (conf.level >= 100)) {
                    conf.level <- 95;
                }

                if (length(items) > 0) {

                    conf.level <- conf.level / 100;

                    diamondPlot <-
                        ufs::meansDiamondPlot(data = data,
                                              items = items,
                                              conf.level=conf.level,
                                              showData=showData,
                                              theme=ggplot2::theme_bw(base_size=9) +
                                                  ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent",
                                                                                                          color="transparent"),
                                                                 plot.background = ggplot2::element_rect(fill="transparent",
                                                                                                         color="transparent"),
                                                                 legend.background = ggplot2::element_rect(fill="transparent",
                                                                                                           color="transparent"),
                                                                 legend.box.background = ggplot2::element_rect(fill="transparent",
                                                                                                                color="transparent")));

                    print(grid::grid.draw(diamondPlot));

                }
            }

            TRUE;

        })
)
