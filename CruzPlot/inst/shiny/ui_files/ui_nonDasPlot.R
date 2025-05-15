# UI code for plotting non-das data

ui.nonDasPlot <- function() {
  tabItem(
    tabName = "nonDASplot",
    fluidRow(
      box(status = "primary", width = 6, plotOutput("plot5", height = "auto")),
      tabBox(
        title = "", id = "tabset2", width = 6,
        tabPanel(
          title = "Non-DAS data",
          fluidRow(
            box(
              title = "Loaded data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 12,
              DT::dataTableOutput("cruzNonDasLoaded"),
              ui.new.line(),
              column(4, uiOutput("ndas_remove_execute")),
              column(3, textOutput("cruzNonDasRemove_text"))
            )
          ),
          fluidRow(
            box(
              title = "Load data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              helpText("Longitudes must be in -180 to 180 range.",
                       "See the manual for longitude and latitude column naming requirements"),

              fluidRow(
                column(12, fileInput("ndas_file", label = tags$h5("Load non-DAS CSV file")))
              ),
              textOutput("cruzNonDasFile_LonLat_text"),
              conditionalPanel(
                condition = "output.cruzNonDasFile_Conditional",
                radioButtons("ndas_plot_type", label = tags$h5("Type of data"), choices = list("Line" = 1, "Point" = 2), selected = 1),

                conditionalPanel(
                  condition = "input.ndas_plot_type==1",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("ndas_line_lty", label = tags$h5("Line type"), choices = cruz.line.type, selected = 1),
                      numericInput("ndas_line_lwd", label = tags$h5("Line width"), value = 1, min = 1, max = 6, step = 1)
                    ),
                    column(6, selectInput("ndas_line_col", label = tags$h5("Line color"), choices = cruz.palette.color,  selected = "black"))
                  )
                ),
                conditionalPanel(
                  condition = "input.ndas_plot_type==2",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("ndas_pt_pch", label = tags$h5("Point type"), choices = cruz.symbol.type, selected = 1),
                      numericInput("ndas_pt_cex", label = tags$h5("Point size"), value = 1, min = 0.1, max = 5, step = 0.1)
                    ),
                    column(
                      width = 6,
                      selectInput("ndas_pt_col", label = tags$h5("Point color"), choices = cruz.palette.color, selected = "black"),
                      numericInput("ndas_pt_lwd", label = tags$h5("Point line width"), value = 1, min = 1, max = 6, step = 1)
                    )
                  )
                ),
                actionButton("ndas_load_execute", "Add non-DAS data to CruzPlot"),
                textOutput("cruzNonDasAdd_text")
              )
            ),
            box(
              title = "Plot data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              checkboxInput("ndas_plot", label = tags$h5("Plot loaded non-DAS data"), value = FALSE),
              conditionalPanel(
                condition = "input.ndas_plot",
                ui.select.instructions(),
                uiOutput("ndas_toplot_uiOut_select")
              )
            )
          )
        )
      )
    )
  )
}
