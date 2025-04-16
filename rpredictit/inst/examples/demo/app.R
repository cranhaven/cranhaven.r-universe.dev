library(shiny)
library(rpredictit)
library(DT)
library(dygraphs)

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    tags$style(
      HTML(
        "table.dataTable tr.selected td,table.dataTable td.selected
        {background-color: #edf2f3 !important;}"
      )
    ),
    fluidRow(
      column(3,
             wellPanel(
               div(id = "myapp",
                   DTOutput("contract_DT"),
                   actionButton("add_to_watchlist", "Add to Watchlist", icon = icon("plus"), width = "100%"),
                   br(),
                   tags$hr(),
                   fileInput("historical_csv",
                             "Upload Historical Data",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                   )
                   )
               )
             ),
      column(9,
             uiOutput("watchlist_refresh"),
             DTOutput("watchlist_DT"),
             dygraphOutput("historical_plot")
      )
    )
  ),

  server = function(input, output, session) {

    rv <- reactiveValues(contract_data = NULL, table_data = NULL, watchlist = NULL)

    markets <- reactive({
      rpredictit::all_markets()
    })

    output$contract_DT <- renderDT({
      req(markets())

      contract_data <- markets()
      table_data <- rpredictit::format_market_data(contract_data)

      rv$contract_data <- contract_data
      rv$table_data <- table_data

      # Set up datatables options config
      options_list <- list(
        ordering = TRUE, dom = "frt",
        pageLength = nrow(contract_data), paging = FALSE,
        columnDefs = list(list(className = "dt-left", targets = "_all")),
        scroller = TRUE, scrollY = "50vh"
      )

      table_data <- table_data %>%
        dplyr::select("Market", "Expiry", "Market id") %>%
        dplyr::distinct()

      rv$display_data <- table_data
      table_data <- table_data %>% dplyr::select(-c("Market id"))

      DT::datatable(table_data,
                    escape = FALSE,
                    fillContainer = FALSE,
                    rownames = FALSE,
                    class = "cell-border compact",
                    options = options_list
      )
    })

    observeEvent(input$add_to_watchlist, {
      req(input$contract_DT_rows_selected)

      id_idx <- rv$display_data$`Market id`[input$contract_DT_rows_selected]
      watchlist_data <- rv$contract_data %>%
        dplyr::filter(id %in% id_idx)

      watchlist <- rbind(rv$watchlist, watchlist_data) %>%
        dplyr::distinct_at(dplyr::vars("contract_id"), .keep_all = TRUE)

      rv$watchlist <- watchlist

      watchlist_ids <- unique(rv$watchlist$id)
    })

    output$watchlist_DT <- renderDT({
      req(rv$watchlist)

      watchlist_data <- rv$watchlist

      # Set up datatables options config
      options_list <- list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '90%'});",
          "$(this.api().table().body()).css({'font-size': '90%'});",
          "}"
        ),
        ordering = TRUE, dom = "frt",
        pageLength = nrow(watchlist_data), paging = FALSE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )

      # If the table gets too large, enable dynamic height scrolling
      if (nrow(watchlist_data) > 8) {
        options_list$scrollY <- "60vh"
        options_list$scroller <- TRUE
      }

      watchlist_data <- rpredictit::format_market_data(watchlist_data)
      updated_at <- watchlist_data$Timestamp[1]

      watchlist_data <- watchlist_data %>% dplyr::select(-c("Timestamp", "Market id", "Contract id"))
      DT::datatable(watchlist_data,
                    filter = "top",
                    caption = tags$em(paste0("Updated at ", updated_at)),
                    escape = FALSE,
                    fillContainer = FALSE,
                    rownames = FALSE,
                    class = "cell-border compact",
                    options = options_list
      )
    })

    # Dynamic UI to show the actionButton for refreshing watchlist only when the data exists
    output$watchlist_refresh <- renderUI({
      req(rv$watchlist)
      actionButton("refresh_watchlist", "Refresh", icon = icon("refresh"), width = "10%")
    })

    # observeEvent for retrieving most recent watchlist bid/ask data
    observeEvent(input$refresh_watchlist, {
      req(rv$watchlist)
      ids <- unique(rv$watchlist$id)

      data <- lapply(1:length(ids), function(x) rpredictit::single_market(ids[x]))
      data <- dplyr::bind_rows(data)

      rv$watchlist <- data
    })

    # Dygraph containing historical contract prices from an uploaded csv file
    output$historical_plot <- renderDygraph({
      req(input$historical_csv)
      inFile = input$historical_csv
      contract_data <- rpredictit::parse_historical_csv(inFile$datapath, filename = inFile$name)
      rpredictit::historical_plot(contract_data)
    })
  }
)
