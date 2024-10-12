require("shiny")

# UI ----
ui <- fluidPage(
  tag("link", list(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css?family=Raleway"
  )),
  includeCSS("./www/styles.css"),
  titlePanel("Multidimentional Top Scoring for Creativity Research"),
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # hex sticker ----
      img(
        id = "hex-sticker",
        src = "https://raw.githubusercontent.com/jakub-jedrusiak/mtscr/main/man/figures/mtscr-hex.svg",
        alt = "mtscr hex sticker",
      ),
      hr(),
      actionButton("import_window", "Import data"),
      uiOutput("args_dropdowns"),
      uiOutput("download_buttons"),
      uiOutput("wide_warning")
    ),
    ## Main panel ----
    mainPanel(
      width = 9,
      fluidRow(
        ### Model info ----
        uiOutput("models_summary_header"),
        tableOutput("models_summary"),
        ### Loading message ----
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id = "loadmessage")
        )
      ),
      fluidRow(
        uiOutput("scored_data_header"),
        DT::dataTableOutput("scored_data", width = "95%")
      )
    )
  ),
  hr(),
  div(class = "footer", includeHTML("./www/article_citation.html")),
)


# Server ----
server <- function(input, output, session) {
  ## Import data when run ----
  datamods::import_modal(
    id = "data_main",
    from = c("env", "file", "copypaste", "googlesheets", "url"),
    title = "Import data to be used in application"
  )

  ## Import button ----
  observeEvent(input$import_window, {
    datamods::import_modal(
      id = "data_main",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data to be used in application"
    )
  })

  imported <- datamods::import_server("data_main", return_class = "tbl_df")

  ## Dropdown lists with arguments for `mtscr_model()` ----
  output$args_dropdowns <- renderUI({
    req(imported$data())
    list(
      br(),
      selectInput("id_column", "Select ID column:", choices = colnames(imported$data())),
      selectInput("item_column", "Select item column:", choices = c("no item column", colnames(imported$data()))),
      selectInput("score_column", "Select score column:", choices = colnames(
        dplyr::select(
          imported$data(),
          dplyr::where(is.numeric)
        )
      )),
      selectInput("ties_method", "Select ties method", choices = c("random (better for ratings)", "average (better for continous scores)")),
      checkboxInput("normalise", "Normalise scores (recommended)", value = TRUE),
      actionButton("self_ranking_info", "What is self-ranking and how to format it?"),
      selectInput(
        "self_ranking",
        "Column with self-ranking:",
        choices = c(
          "no self-ranking",
          colnames(dplyr::select(
            imported$data(),
            dplyr::where(is.numeric)
          ))
        )
      ),
      sliderInput("top", "Max number of top answers to be included:", value = 1, min = 1, max = 10),
      actionButton("generate_model", "Generate model →")
    )
  })

  ## self_ranking info box ----

  observeEvent(input$self_ranking_info, {
    shinyWidgets::show_alert(
      title = NULL,
      text = "Name of the column containing answers' self-ranking. Provide if model should be based on top answers self-chosen by the participant. Every item should have its own ranks. Preferably it should be a complete ranking (each answer with its own relative rank) starting with 1 for the best answer. Otherwise the top answers should have a value of 1, and the other answers should have a value of 0. In that case, the Top answers argument doesn't change anything so leave the slider at 1. Ties method is not used if self-ranking was provided. See mtscr_self_rank dataset fo example.",
      type = "info"
    )
  })

  ## Generate model button ----
  observeEvent(input$generate_model, {
    ### Create model ----
    data <- imported$data()
    id_col <- input$id_column
    if (input$item_column == "no item column") {
      item_col <- NULL
    } else {
      item_col <- input$item_column
    }
    score_col <- input$score_column
    ties_method <- ifelse(input$ties_method == "random (better for ratings)", "random", "average")
    top <- seq(1, input$top)
    normalise <- input$normalise
    if (input$self_ranking == "no self-ranking") {
      self_ranking <- NULL
    } else {
      self_ranking <- input$self_ranking
    }
    model <- mtscr::mtscr_model(data, {{ id_col }}, {{ item_col }}, {{ score_col }}, top = top, ties_method = ties_method, normalise = normalise, self_ranking = {{ self_ranking }})
    if (length(top) == 1) {
      model <- list(model)
    }
    models_summary <- mtscr::mtscr_model_summary(model)

    ### Make UI for summaries ----
    output$models_summary_header <- renderUI(tags$b("Models summary:"))
    output$models_summary <- renderTable(models_summary)

    ### Make UI for scored data ----
    scored_data <- mtscr::mtscr_score(data, {{ id_col }}, {{ item_col }}, {{ score_col }}, top = top, format = "minimal", ties_method = ties_method, normalise = normalise, self_ranking = {{ self_ranking }})
    scored_data_whole <- mtscr::mtscr_score(data, {{ id_col }}, {{ item_col }}, {{ score_col }}, top = top, format = "full", ties_method = ties_method, normalise = normalise, self_ranking = {{ self_ranking }})
    output$scored_data_header <- renderUI(tags$b("Scored data:"))
    output$scored_data <- DT::renderDataTable(
      scored_data,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("csv", "excel")
      )
    )

    ### Download buttons ----
    output$download_buttons <- renderUI(
      list(
        br(),
        tags$b("Dowload scores:"),
        br(),
        downloadButton("scores_csv", ".csv"),
        downloadButton("scores_xlsx", ".xlsx"),
        br(),
        br(),
        tags$b("Dowload the whole database with scores:"),
        br(),
        downloadButton("whole_csv", ".csv"),
        downloadButton("whole_xlsx", ".xlsx")
      )
    )

    ## Download handlers ----
    output$scores_csv <- downloadHandler(
      filename = "scores.csv",
      content = function(file) {
        write.csv(scored_data, file)
      }
    )

    output$scores_xlsx <- downloadHandler(
      filename = "scores.xlsx",
      content = function(file) {
        writexl::write_xlsx(scored_data, file)
      }
    )

    output$whole_csv <- downloadHandler(
      filename = "whole.csv",
      content = function(file) {
        write.csv(scored_data_whole, file)
      }
    )

    output$whole_xlsx <- downloadHandler(
      filename = "whole.xlsx",
      content = function(file) {
        writexl::write_xlsx(scored_data_whole, file)
      }
    )
  })
}

# App function ----
shinyApp(ui, server)
