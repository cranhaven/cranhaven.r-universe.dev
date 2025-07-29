uitpSummaryStatistics <-
  tabPanel(
    "Summary Statistics and Plots",

    # Side Panel
    # div(
    #  style = paste(
    #    "float: left; width: 400px; height: 76vh; padding: 10px;",
    #    "border: 1px solid lightgray; background-color: #EDEDED;",
    #    "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
    #    "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
    #  ),
    fluidRow(column(
      10L,
      offset = 1L,
      style = paste(
        "border: 1px solid lightgray; background-color: #EDEDED;",
        "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
      ),
      withMathJax(includeHTML(file.path("..", "extdata", "ui_guidance",
                                        "summary_stats.html")))
    )),
    br(),
    fluidRow(
      column(1L, offset = 1L, shinyBS::popify(
        downloadButton("downloadKinship", "Export Kinship Matrix"),
        NULL,
        #"Exports Kinship Matrix as CSV",
        paste0(
          "This exports the kinship matrix to a CSV file ",
          "in the users home directory"
        )
      )),
      column(1L, offset = 1L, shinyBS::popify(
        downloadButton("downloadMaleFounders", "Export Male Founders"),
        NULL,
        #"Exports Male Founder records as CSV",
        paste0(
          "This exports the male founder pedigree records to ",
          "a CSV file to the user selected directory."
        )
      )),
      column(1L, offset = 1L, shinyBS::popify(
        downloadButton("downloadFemaleFounders", "Export Female Founders"),
        NULL,
        #"Exports Female Founder records as CSV",
        paste0(
          "This exports the female founder pedigree records ",
          "to a CSV file to the user selected directory."
        )
      )),
      column(2L, offset = 1L, shinyBS::popify(
        downloadButton("downloadFirstOrder",
                       "Export First-Order Relationships"),
        NULL,
        #"Exports All First-Order Relationships as CSV",
        paste0(
          "This exports all first-order relations to a CSV file ",
          "to the user selected directory."
        )
      ))
    ),
    br(),
    # nolint start: commented_code_linter
    # fluidRow(
    #   column(2L, offset = 1L,
    #        style = paste0("padding-top:1px;display:inline-block;",
    #                       "padding-bottom:1px"),
    #        checkboxInput("displayRelations",
    #                      label = paste0("Optional: Display ",
    #                                     "Relations Table"),
    #                      width = "150%",
    #                      value = FALSE)
    # )),
    # nolint end: commented_code_linter
    fluidRow(
      column(10L, offset = 1L, htmlOutput("summaryStats")),
      # nolint start: commented_code_linter
      #   column(10L, offset = 1L,
      #          DT::dataTableOutput("relations")
      #          # DT::dataTableOutput("relations"),
      #          # DT::dataTableOutput("maleFounders"),
      #          # DT::dataTableOutput("femaleFounders")
      #   ),
      # nolint end: commented_code_linter),
      br(),
      br(),
      fluidRow(
        # Main Panel
        column(
          5L,
          offset = 1L,
          # style = "margin-left:425px;padding:10px;", # nolint: commented_code_linter
          plotOutput("mkHist", width = "400px", height = "400px"),
          br(),
          shinyBS::popify(
            downloadButton(
              "downloadMeanKinshipCoefficientHistogram",
              "Export Mean Kinship Coefficient histogram"
            ),
            NULL,
            #"Export Mean Kinship Coefficient histogram as PNG file",
            paste0(
              "This exports the Mean Kinship Coefficient histogram as ",
              "a PNG file to the user selected directory."
            )
          ),
          br(),
          br(),
          plotOutput("zscoreHist", width = "400px", height = "400px"),
          br(),
          shinyBS::popify(
            downloadButton("downloadZScoreHistogram",
                           "Export Z-Score histogram"),
            NULL,
            paste0(
              "This exports the Mean Kinship Z-score histogram as ",
              "a PNG file to the user selected directory."
            )
          ),
          br(),
          br(),
          plotOutput("guHist", width = "400px", height = "400px"),
          br(),
          shinyBS::popify(
            downloadButton(
              "downloadGenomeUniquenessHistogram",
              "Export Genome Uniqueness histogram"
            ),
            NULL,
            #"Exports Genome Uniqueness histogram as PNG file",
            paste0(
              "This exports the Genome Uniqueness histogram as PNG ",
              "file to the user selected directory."
            )
          ),
          br()
        ),
        column(
          5L,
          plotOutput("mkBox", width = "400px", height = "400px"),
          br(),
          shinyBS::popify(
            downloadButton(
              "downloadMeanKinshipCoefficientBoxPlot",
              "Export Mean Kinship Coefficient box plot"
            ),
            NULL,
            #"Export Mean Kinship Coefficient box plot as PNG file",
            paste0(
              "This Mean Kinship Coefficient box plot as PNG file ",
              "to the user selected directory."
            )
          ),
          br(),
          br(),
          plotOutput("zscoreBox", width = "400px", height = "400px"),
          br(),
          shinyBS::popify(
            downloadButton("downloadZScoreBoxPlot", "Export Z-Score box plot"),
            NULL,
            paste0(
              "This exports the Mean Kinship Z-score box plot as ",
              "a PNG file to the user selected directory."
            )
          ),
          br(),
          br(),
          plotOutput("guBox", width = "400px", height = "400px"),
          br(),
          shinyBS::popify(
            downloadButton(
              "downloadGenomeUniquenessBoxPlot",
              "Export Genome Uniqueness box plot"
            ),
            NULL,
            #"Export Genome Uniqueness box plot as PNG file",
            paste0(
              "This exports Genome Uniqueness box plot as a PNG ",
              "file to the user selected directory."
            )
          ),
          br(),
          br()
        )
      ),
      fluidRow(column(
        10L,
        offset = 1L,
        style = paste(
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        withMathJax(includeHTML(
          file.path("..", "extdata", "ui_guidance",
                    "population_genetics_terms.html")
        ))
      ))

    )
  )
