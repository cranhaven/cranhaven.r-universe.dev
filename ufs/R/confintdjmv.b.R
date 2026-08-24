
# This file is a generated template, your changes will not be overwritten

confintdjmvClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "confintdjmvClass",
    inherit = confintdjmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ((is.numeric(self$options$d)) &&
                (is.numeric(self$options$n) &&
                 (self$options$n > 0)) &&
                (is.numeric(self$options$conf.level) &&
                 (self$options$conf.level > 0) &&
                 (self$options$conf.level < 100))) {

                res <- ufs::confIntD(d = self$options$d,
                                     n = self$options$n,
                                     conf.level = self$options$conf.level/100,
                                     plot=FALSE);

                self$results$text$setContent(
                    paste0("A Cohen's d of ",
                           self$options$d, " computed from a sample of ",
                           self$options$n,
                           " datapoints has a ",
                           self$options$conf.level, "% confidence ",
                           "interval of ",
                           ufs::formatCI(res), "."));

                self$results$ciPlot$setState(list(d=self$options$d,
                                                  n=self$options$n,
                                                  conf.level=self$options$conf.level));

            }

        },
        .plot = function(ciPlot, ggtheme, theme, ...) {

            if (!is.null(ciPlot$state)) {
                d <- ciPlot$state$d;
                n <- ciPlot$state$n;
                conf.level <- ciPlot$state$conf.level / 100;
                res <- ufs::confIntD(d = d,
                                     n = n,
                                     conf.level = conf.level,
                                     plot=TRUE);
                plot <- attr(res, 'plot') +
                    ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent",
                                                                            color="transparent"),
                                   plot.background = ggplot2::element_rect(fill="transparent",
                                                                           color="transparent"),
                                   legend.background = ggplot2::element_rect(fill="transparent",
                                                                             color="transparent"),
                                   legend.box.background = ggplot2::element_rect(fill="transparent",
                                                                                 color="transparent")) +
                    ggtheme +
                    ggplot2::theme(plot.title = ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
                                   plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)) +
                    NULL;
                print(res);
            }

            TRUE;

        })
)
