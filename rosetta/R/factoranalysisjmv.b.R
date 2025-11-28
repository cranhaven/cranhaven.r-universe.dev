
# This file is a generated template, your changes will not be overwritten

factorAnalysisjmvClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "factorAnalysisjmvClass",
    inherit = factorAnalysisjmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            ready <- TRUE;

            if (is.null(self$options$items) || length(self$options$items) < 2) {
                ready <- FALSE;
            }

            if (ready) {

                data <- self$data;
                nfactors <- ifelse(self$options$nFactorMethod == "eigen",
                                   "eigen",
                                   self$options$nFactors);

                res <-
                    rosetta::factorAnalysis(
                        data = data,
                        nfactors = nfactors,
                        items = self$options$items,
                        rotate = self$options$rotation,
                        covar = FALSE,
                        kaiser = self$options$minEigen,
                        loadings = TRUE,
                        summary = self$options$factorSummary,
                        correlations = self$options$factorCor,
                        modelFit = self$options$modelFit,
                        eigenValues = self$options$eigen,
                        screePlot = self$options$screePlot,
                        colorLoadings = self$options$colorLoadings,
                        fm = self$options$extraction
                    );

                ###-------------------------------------------------------------
                ### Table with loadings
                ###-------------------------------------------------------------

                if (self$options$colorLoadings) {
                    res$output$loadings[, 1:(ncol(res$output$loadings) - 1)] <-
                        data.frame(
                            lapply(
                                res$output$loadings[, 1:(ncol(res$output$loadings) - 1),
                                                    drop=FALSE],
                                function(column) {
                                    kableExtra::cell_spec(
                                        round(column, res$input$digits),
                                        bold = T,
                                        color = kableExtra::spec_color(
                                            abs(column),
                                            end = 0.9,
                                            direction = -1,
                                            scale_from = c(0, 1)
                                        ),
                                        font_size = kableExtra::spec_font_size(
                                            abs(column),
                                            scale_from = c(-1, 1)
                                        )
                                    )
                                }
                            )
                        );
                }
                self$results$loadings$setContent(
                    paste0(
                        kableExtra::kable_styling(
                            kableExtra::kable(
                                res$output$loadings,
                                digits=res$input$digits,
                                escape=FALSE
                            )
                        ),
                        paste0(
                            "<div>",
                            res$output$extractionMethod,
                            " extraction and ",
                            res$output$rotation,
                            ".</div>"
                        )
                    )
                );

                ###-------------------------------------------------------------
                ### Table with
                ###-------------------------------------------------------------


                ###-------------------------------------------------------------
                ### End of 'if ready' section
                ###-------------------------------------------------------------

            }

            ###-----------------------------------------------------------------
            ### End of .run()
            ###-----------------------------------------------------------------

        })
)
