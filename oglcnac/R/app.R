#' Launch oglcnac Shiny App
#'
#' This function launches a Shiny App for uploading, processing,
#' and downloading UniProt data in CSV, TSV, or Excel format.
#' Users can upload data, preview it, and select specific columns for processing.
#' The processed data can be viewed and downloaded.
#'
#' @return None
#' @export
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel tabsetPanel tabPanel
#'  titlePanel fileInput numericInput selectInput actionButton downloadButton verbatimTextOutput
#'  renderText observeEvent updateSelectInput downloadHandler reactiveVal
#'  req shinyApp fluidRow column tags p hr
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom bslib bs_theme
#' @importFrom readxl read_excel
#' @importFrom shiny dialogViewer runGadget
#'
#' @examples
#' if (interactive()) {
#'   oglcnac::launch_app()
#' }
launch_app <- function() {
  # Define the UI
  ui <- fluidPage(
    theme = bs_theme(
      version = 5L,
      primary = "#112446",
      secondary = "#cccccc",
      preset = "bootstrap",
      font_scale = 0.9,
      "accordion-body-padding-y" = "3px", # Reduced padding
      "accordion-body-padding-x" = "3px" # Reduced padding
    ),

    # Styled title area with compact padding
    tags$div(
      class = "title-area",
      style = "background-color: #112446; padding: 10px; color: white; text-align: center; margin-bottom: 10px;", # Reduced padding
      tags$h2("OGlcNAc App")
    ),

    # Increased container width to reduce scrolling and condensed layout
    tags$div(
      class = "container-fluid",
      style = "max-width: 1300px;", # Increase the width slightly to fit more content
      sidebarLayout(
        sidebarPanel(
          width = 4, # Adjust sidebar width to leave more space for content

          p("This application allows you to upload and process UniProt data files in CSV, TSV, or Excel format.
             You can preview the data, select specific columns for entry name, protein name, gene name, and other
             relevant fields. After processing the data, you can download the updated file for further analysis."),
          p("Please select a file to begin the data processing workflow."),
          hr(),
          fileInput("file", "Upload your Excel, CSV, or TSV file", accept = c(".xlsx", ".csv", ".tsv")),
          hr(),


          # Dropdown menus arranged in pairs to save vertical space
          fluidRow(
            column(6, numericInput("n_rows", "Process first N rows", value = 20, min = 1)),
            column(6, selectInput("accession_col", "Accession Column", choices = NULL))
          ),
          fluidRow(
            column(6, selectInput("accession_source_col", "Source Column", choices = NULL)),
            column(6, selectInput("entry_name_col", "Entry Name Column", choices = NULL))
          ),
          fluidRow(
            column(6, selectInput("protein_name_col", "Protein Name Column", choices = NULL)),
            column(6, selectInput("gene_name_col", "Gene Name Column", choices = NULL))
          ),

          # Add horizontal line to separate sections
          hr(),

          # Action buttons
          actionButton("process", "Process Data", class = "btn-primary"),
          downloadButton("download", "Download Processed Data", class = "btn-success")
        ),
        mainPanel(
          width = 8, # Expand the main panel to fit more content
          tabsetPanel(
            tabPanel("Preview Data", DTOutput("preview")),
            tabPanel("Processed Data", DTOutput("result")),
            tabPanel("Status", verbatimTextOutput("status"))
          )
        )
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {
    # Reactive value to store uploaded data
    uploaded_data <- reactiveVal()
    processed_data <- reactiveVal()

    # Reactive value to store logs
    logs <- reactiveVal("")

    # Custom log function to capture output
    log_console <- function(...) {
      current_log <- logs()
      new_log <- paste0(current_log, paste(..., collapse = " "), "\n")
      logs(new_log)
    }

    # Display logs in real-time
    output$status <- renderText({
      logs()
    })

    # Handle file upload and preview
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)

      # Load data based on file extension
      df <- switch(ext,
        csv = utils::read.csv(input$file$datapath),
        tsv = utils::read.delim(input$file$datapath),
        xlsx = readxl::read_excel(input$file$datapath),
        stop("Invalid file format")
      )

      uploaded_data(df)

      # Log file upload info
      log_console("File uploaded: ", input$file$name)

      # Update dropdown choices based on data columns
      updateSelectInput(session, "accession_col", choices = names(df), selected = "accession")
      updateSelectInput(session, "accession_source_col", choices = names(df), selected = "accession_source")
      updateSelectInput(session, "entry_name_col", choices = names(df), selected = "entry_name")
      updateSelectInput(session, "protein_name_col", choices = names(df), selected = "protein_name")
      updateSelectInput(session, "gene_name_col", choices = names(df), selected = "gene_name")

      # Preview the first 10 rows of the dataset without text wrapping and with horizontal scrolling
      output$preview <- renderDT({
        datatable(df, options = list(scrollX = TRUE, columnDefs = list(list(targets = "_all", className = "dt-nowrap"))))
      })

      # Log preview status
      log_console("Preview of the loaded data generated.")
    })

    # Process data when the process button is clicked
    observeEvent(input$process, {
      req(uploaded_data())

      # Log processing start
      log_console("Processing data...")

      # Limit rows if the user specified N rows
      df <- uploaded_data()
      if (!is.null(input$n_rows) && input$n_rows > 0) {
        df <- utils::head(df, input$n_rows)
      }

      # Process the data using the process_tibble_uniprot function
      processed_df <- process_tibble_uniprot(df,
        accession_col = input$accession_col,
        accession_source_col = input$accession_source_col,
        entry_name_col = input$entry_name_col,
        protein_name_col = input$protein_name_col,
        gene_name_col = input$gene_name_col
      )

      # Store the processed data
      processed_data(processed_df)

      # Display processed data without text wrapping and with horizontal scrolling
      output$result <- renderDT({
        datatable(processed_df, options = list(scrollX = TRUE, columnDefs = list(list(targets = "_all", className = "dt-nowrap"))))
      })

      # Log processing completion
      log_console("Data processed successfully!")
    })

    # Allow user to download processed data
    output$download <- downloadHandler(
      filename = function() {
        paste("processed_data.csv")
      },
      content = function(file) {
        df <- processed_data()
        utils::write.csv(df, file, row.names = FALSE)

        # Log download event
        log_console("Processed data downloaded.")
      }
    )
  }

  # Launch the app as a shiny gadget in a dialog with a slightly larger size
  viewer <- dialogViewer("OGlcNAc App", width = 1200, height = 800)
  runGadget(shinyApp(ui, server), viewer = viewer)
}
