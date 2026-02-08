#' uploading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uploading_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 6,
      title = "Inputs",
      status = "success",
      solidHeader = FALSE,
      fluidRow(
        column(
          width = 6,
          fileInput(
            inputId = ns("file_path_dfA"),
            label = "Select Sample data set",
            multiple = FALSE,
            accept = c(
              # "text/csv",
              # "text/comma-separated-values,text/plain",
              ".csv",
              ".xlsx",
              ".sas7bdat",
              ".sav",
              ".dta",
              ".tsv"
            )
          ),
          fileInput(
            inputId = ns("file_path_dfB"),
            label = "Select Matching data set",
            multiple = FALSE,
            accept = c(
              # "text/csv",
              # "text/comma-separated-values,text/plain",
              ".csv",
              ".xlsx",
              ".sas7bdat",
              ".sav",
              ".dta",
              ".tsv"
            )
          ),
          HTML("<p><i>Supported formats: Excel, csv, tsv, SAS, SPSS, Stata.</i></p>")
        ),
        column(
          width = 6,
          fluidRow(
            HTML("<h5><b>Download our demo data</b></h5>"),
            HTML("<h5>Sample data</h5>"),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/lkselectedrecs.xlsx'> <i class='fa fa-download'> </i> xlsx</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/lkselectedrecs.sas7bdat'> <i class='fa fa-download'> </i> sas7bdat</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/lkselectedrecs.sav'> <i class='fa fa-download'> </i> sav</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/lkselectedrecs.dta'> <i class='fa fa-download'> </i> dta</a>"
            ),
            # HTML(
            #   "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/lkselectedrecs.csv'> <i class='fa fa-download'> </i> csv</a>"
            # ),
            br(),
            br(),
            HTML("<h5>Matching data</h5>"),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/redcapoutput.xlsx'> <i class='fa fa-download'> </i> xlsx</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/redcapoutput.sas7bdat'> <i class='fa fa-download'> </i> sas7bdat</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/redcapoutput.sav'> <i class='fa fa-download'> </i> sav</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/redcapoutput.dta'> <i class='fa fa-download'> </i> dta</a>"
            ),
            # HTML(
            #   "<a href='https://raw.githubusercontent.com/cdc-addm/ShinyLink/main/inst/app/www/redcapoutput.csv'> <i class='fa fa-download'> </i> csv</a>"
            # ),
            br(),
            br(),
            HTML(
              "<p><i>Optimized for 1980 x 1080 resolution screen (1080p) and Google Chrome Web Browser Version 106.0.5249.119.</i></p>"
            )
          )
        )

      )
    ),
    box(
      width = 6,
      title = "Summary Statistics",
      status = "success",
      solidHeader = FALSE,
      plotOutput(ns("plot-upload"), height = "248px")

    ),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(outputId = ns('upload_dfA'), width = "100%"))
      )
    ),
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data set",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(outputId = ns('upload_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        # actionBttn(
        #   inputId = ns("Previous"),
        #   label = "Previous",
        #   style = "simple",
        #   color = "primary",
        #   icon = icon("arrow-left"),
        #   size = "sm"
        # ),
        align = "left",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      column(
        width = 6,
        actionBttn(
          inputId = ns("next_duplicate"),
          label = "Next: Remove Duplicate Rows",
          style = "simple",
          color = "primary",
          icon = icon("arrow-right"),
          size = "sm"
        ),
        align = "right",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      style = "margin-left: 0px;",
      style = "margin-right: 0px;"
    )
  )
}

#' uploading Server Functions
#'
#' @importFrom shinydashboard updateTabItems
#'
#' @noRd

mod_uploading_server <- function(id, state, parent){

  # File uploading limit: 25 MB
  options(shiny.maxRequestSize = 25 * 1024 ^ 2)

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    load_file <- function(name, path) {
      ext <- tools::file_ext(name)

      if (ext == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' is required to read Excel files. Please install it.")
        }
        df <- readxl::read_excel(path)
      }

      if (ext == "sas7bdat") {
        if (!requireNamespace("haven", quietly = TRUE)) {
          stop("Package 'haven' is required to read SAS files. Please install it.")
        }
        df <- haven::read_sas(path)
        df[df == ""] <- NA
      }

      if (ext == "sav") {
        if (!requireNamespace("haven", quietly = TRUE)) {
          stop("Package 'haven' is required to read SPSS files. Please install it.")
        }
        df <- haven::read_sav(path)
        df[df == ""] <- NA
      }

      if (ext == "dta") {
        if (!requireNamespace("haven", quietly = TRUE)) {
          stop("Package 'haven' is required to read Stata files. Please install it.")
        }
        df <- haven::read_dta(path)
        df[df == ""] <- NA
      }

      if (ext == "csv") {
        if (!requireNamespace("vroom", quietly = TRUE)) {
          stop("Package 'vroom' is required to read CSV files. Please install it.")
        }
        df <- vroom::vroom(path, delim = ",", col_types = list())
      }

      if (ext == "tsv") {
        if (!requireNamespace("vroom", quietly = TRUE)) {
          stop("Package 'vroom' is required to read TSV files. Please install it.")
        }
        df <- vroom::vroom(path, delim = "\t", col_types = list())
      }

      return(df)
    }

    dfA <- reactive({
      req(input$file_path_dfA)

      dfA <- load_file(basename(input$file_path_dfA$datapath),
                       input$file_path_dfA$datapath)
      # print(dfA)

      state$dfA_uploaded <- dfA
      return(dfA)
    })

    dfB <- reactive({
      req(input$file_path_dfB)

      dfB <- load_file(basename(input$file_path_dfB$datapath),
                       input$file_path_dfB$datapath)
      # print(dfB)

      state$dfB_uploaded <- dfB
      return(dfB)
    })

    output[["plot-upload"]] <- renderPlot({
      # Testing only
      # dfA <- readxl::read_excel('inst/app/www/lkselectedrecs_cleaned.xlsx')
      # dfB <- readxl::read_excel('inst/app/www/redcapoutput.xlsx')
      dfA <- dfA()
      dfB <- dfB()

      # Create data
      data <- tibble::tibble(
        Value = c(
          ncol(dfA),
          nrow(dfA),
          sum(duplicated(dfA) == TRUE),
          ncol(dfB),
          nrow(dfB),
          sum(duplicated(dfB) == TRUE)
        )
      )

      data$name <- factor(
        c(
          "Variables",
          "Entries",
          "Duplicates",
          "Variables",
          "Entries",
          "Duplicates"
        ),
        levels = c("Entries", "Variables", "Duplicates")
      )
      data$Group <-
        factor(c(rep("Sample Data", 3), rep("Matching Data", 3)),
               levels = c("Sample Data", "Matching Data"))

      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plotting. Please install it.")
      }
      if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Package 'scales' is required for plotting. Please install it.")
      }
      if (!requireNamespace("ggpubr", quietly = TRUE)) {
        stop("Package 'ggpubr' is required for plotting. Please install it.")
      }

      # Barplot
      p <- ggplot2::ggplot(data, ggplot2::aes(x=name, y=Value, fill=Group)) +
        ggplot2::geom_bar(position="dodge", stat="identity") +
        ggplot2::geom_text(ggplot2::aes(label=Value), vjust=1.6, color="white",
                  position = ggplot2::position_dodge(0.9), size=3.5)+
        ggplot2::theme_classic() +
        ggplot2::xlab("") + ggplot2::ylab("Counts") +
        ggplot2::scale_fill_manual(values = c( "#7eb7e8","#addc91"))
      p

      data1 <- data %>% dplyr::filter(name == "Entries")
      p1 <- ggplot2::ggplot(data1, ggplot2::aes(x=name, y=Value, fill=Group)) +
        ggplot2::geom_bar(position="dodge", stat="identity") +
        ggplot2::geom_text(ggplot2::aes(label=Value), vjust=1.6, color="white",
                  position = ggplot2::position_dodge(0.9), size=3.5)+
        ggplot2::theme_classic() +
        ggplot2::xlab("") + ggplot2::ylab("Counts") + ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
        ggplot2::scale_fill_manual(values = c( "#7eb7e8","#addc91"))
      p1

      data2 <- data %>% dplyr::filter(name == "Variables")
      p2 <- ggplot2::ggplot(data2, ggplot2::aes(x=name, y=Value, fill=Group)) +
        ggplot2::geom_bar(position="dodge", stat="identity") +
        ggplot2::geom_text(ggplot2::aes(label=Value), vjust=1.6, color="white",
                  position = ggplot2::position_dodge(0.9), size=3.5)+
        ggplot2::theme_classic() +
        ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
        ggplot2::scale_fill_manual(values = c( "#7eb7e8","#addc91"))
      p2

      data3 <- data %>% dplyr::filter(name == "Duplicates")
      if (sum(data3$Value) == 0) {
        p3 <- ggplot2::ggplot(data3, ggplot2::aes(x=name, y=Value, fill=Group)) +
          ggplot2::geom_bar(position="dodge", stat="identity") +
          ggplot2::geom_text(ggplot2::aes(label=Value), vjust=1.6, color="white",
                    position = ggplot2::position_dodge(0.9), size=3.5)+
          ggplot2::theme_classic() + ggplot2::ylim(0, 100) +
          ggplot2::xlab("") + ggplot2::ylab("")  +
          ggplot2::scale_fill_manual(values = c( "#7eb7e8","#addc91"))
        p3
      } else {
        p3 <- ggplot2::ggplot(data3, ggplot2::aes(x=name, y=Value, fill=Group)) +
        ggplot2::geom_bar(position="dodge", stat="identity") +
        ggplot2::geom_text(ggplot2::aes(label=Value), vjust=1.6, color="white",
                  position = ggplot2::position_dodge(0.9), size=3.5)+
        ggplot2::theme_classic() +
        ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
        ggplot2::scale_fill_manual(values = c( "#7eb7e8","#addc91"))
        p3
      }

      ggpubr::ggarrange(p1, p2, p3, ncol=3, nrow=1, common.legend = TRUE, legend="right")


    }, res = 120)


    output$upload_dfA <- DT::renderDataTable(
      dfA(),
      #  caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(13, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 13,
        dom = 'Blfrtip',
        buttons = list()
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output$upload_dfB <- DT::renderDataTable(
      dfB(),
      #  caption = 'Data in the Matching data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = TRUE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(13, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 13,
        dom = 'Blfrtip',
        buttons = list()
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    # Next page button redirection
    observeEvent(input$next_duplicate, {
      # Accessing parent namespace inside the Module
      updateTabItems(session = parent,
                     inputId = "tabs",
                     selected = "duplicate")
    })
  })
}




## To be copied in the UI
# mod_uploading_ui("uploading_1")

## To be copied in the server
# mod_uploading_server("uploading_1")

utils::globalVariables(c("name", "Value", "Group"))
