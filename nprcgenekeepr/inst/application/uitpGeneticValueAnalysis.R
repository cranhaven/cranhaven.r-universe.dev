uitpGeneticValueAnalysis <-
  tabPanel(
    "Genetic Value Analysis",

    div(
      style = "min-width:1000px",
      # Side Panel
      div(
        style = paste(
          "float: left; width: 400px; height: 195px; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left:3px; margin-top: 3px; margin-bottom: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        includeHTML(file.path("..", "extdata", "ui_guidance",
                              "genetic_value.html"))
      ),

      # Main Panel
      div(
        style = "margin-left:425px;padding:10px;",

        div(
          style = paste0("display:inline-block;width:350px;vertical-align: ",
                         "top;padding:10px"),
          numericInput(
            "iterations",
            label = paste0("Enter the number of simulations for the ",
                           "gene-drop analysis:"),
            value = 1000L,
            min = 2L,
            max = 100000L
          )
        ),
        div(
          style = paste0("display:inline-block;width:350px;vertical-align: ",
                         "top;padding:10px"),
          selectInput(
            "threshold",
            label = "Enter the genome uniqueness threshold:",
            choices = list(
              "0" = 1L,
              "1" = 2L,
              "2" = 3L,
              "3" = 4L,
              "4" = 5L
            ),
            selected = 4L
          )
        ),
        helpText("Analysis may take a significant amount of time (>20 min)"),
        actionButton("analysis", label = "Begin Analysis"),
        br(),
        hr(),
        helpText(h4("Results:")),
        helpText(paste0("Enter IDs of animals to be viewed (separate with ",
                        "comma, semicolon, tab or return):")),
        helpText("(Leave blank to view all)"),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          tags$textarea(id = "viewIds", rows = 5L, cols = 20L, ""),
          actionButton("view", label = "Filter View")
        ),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          downloadButton("downloadGVAFull", "Export All"),
          br(),
          br(),
          downloadButton("downloadGVASubset", "Export Current Subset")
        )

      )
    ),
    DT::dataTableOutput("gva")
  )
