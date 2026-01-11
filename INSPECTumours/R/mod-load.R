#' mod_load UI Function
#' @param id the module id
#'
#' @noRd
#'
#' @importFrom shinyalert useShinyalert
#' @importFrom shiny sidebarLayout sidebarPanel downloadLink br hr mainPanel
#' tabsetPanel uiOutput
#' @importFrom shinytoastr useToastr toastr_warning
#' @importFrom shinyFeedback useShinyFeedback
mod_load_ui <- function(id) {

  ns <- NS(id)

  tagList(useShinyalert(),
          useShinyFeedback(),
          useToastr(),
          sidebarLayout(
            sidebarPanel(

              mod_load_file_ui(ns("new"), "New study"),
              downloadLink(ns("download_data"), "Download example data"),
              br(),
              br(),
              mod_load_file_ui(ns("control"), "Historical control data"),

              div(style = "display: flex; align-items: flex-end",
                  div(
                    class = "flex_bt",
                    numericInput(
                      inputId = ns("min_points"),
                      label = "Min number of data points for one animal",
                      value = 3,
                      min = 3
                    )
                  ),
                  div(actionButton(
                    inputId = ns("apply_qc"), label = "Apply"
                  ))),
              hr(),
              actionButton(ns("clear"), "Clear all data")
            ),

            mainPanel(tabsetPanel(
              tabPanel(
                "Raw data",
                uiOutput(ns("exp")),
                uiOutput(ns("control"))
              ),

              tabPanel("QC New study",
                       mod_qc_ui(ns("new"), type = "new"),
                       mod_exclude_ui(ns("new_ex"), type = "new")
              ),

              tabPanel("QC Control Data",
                       mod_qc_ui(ns("control"), type = "control"),
                       mod_exclude_ui(ns("control_ex"), type = "control")
                       )


            ))
          ))
}

#' mod_load Server Function
#' @param r reactiveValues with data
#' @param id,input,output,session internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shinyalert shinyalert
#' @importFrom shiny downloadHandler renderUI h1 h2
#' @importFrom dplyr anti_join
#' @importFrom utils write.csv
#' @importFrom rlang .data
#' @importFrom shinyFeedback feedbackDanger
#' @importFrom DT renderDT DTOutput
mod_load_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # run nested modules
      mod_load_file_server(id = "new", r = r)
      mod_load_file_server(id = "control", r = r)

      mod_qc_server(id = "new", r = r)
      mod_qc_server(id = "control", r = r)

      mod_exclude_server(id = "new_ex", r = r, type = "new")
      mod_exclude_server(id = "control_ex", r = r, type = "control")



    tv_example <- INSPECTumours::example_data

    #example data
    output$download_data <- downloadHandler(
      filename = "example_data.csv",
      content = function(file) {
        write.csv(tv_example, file, row.names = FALSE)
      }
    )

    # raw table new data
    output$new_data <- renderDT({
      req(r$new_data)
      r$new_data
    })

    # summary table new data
    output$sum_new_data <- renderDT({
      req(r$new_data)
      r$summary_new_data <- aggregate_study_info(r$new_data)
    }, options = list(dom = "t"))


    # generate UI for new studies
    output$exp <- renderUI({
      req(r$new_data)
      ns <- session$ns
      tagList(
        h1("New Study"),
        DTOutput(ns("new_data")),
        h2("Summary"),
        DTOutput(ns("sum_new_data")))
    })


    # raw table Control
    output$control_data <- renderDT({
      req(r$h_control_data)
      r$h_control_data
    })


    # summary table control
    output$sum_control_data <- renderDT({
      req(r$h_control_data)
      r$summary_control <- aggregate_study_info(r$h_control_data)
    }, options = list(dom = "t"))


    # generate UI Control
    output$control <- renderUI({
      ns <- session$ns
      if (is.null(r$h_control_data)) {
        h1("No historical control data")
      } else {
        tagList(
          h1("Control"),
          DTOutput(ns("control_data")),
          h2("Summary"),
          DTOutput(ns("sum_control_data"))
        )
      }
    })

    # min datapoints filter
    observeEvent(input$apply_qc, {

      req(r$new_data)

      feedbackDanger("min_points",
                     input$min_points < 3,
                     "Please select number greater than or equal to 3")

      req(input$min_points >= 3)

      excl_min_points_newdata <- below_min_points(r$new_data, input$min_points)
      if (nrow(excl_min_points_newdata) > 0) {
        r$new_data <- anti_join(r$new_data, excl_min_points_newdata)
        r$new_control_data <- filter(r$new_data, .data$treatment == "Control")
        r$excluded_data <- rbind(r$excluded_data, excl_min_points_newdata)
        toastr_warning(paste(
          "Excluded from the new data: ",
          length(unique(excl_min_points_newdata$animal_id)),
          " animal(s)"
          ))
      }

      # save for the report
      r$min_points <- input$min_points

      req(r$h_control_data)

      excl_min_points_h_control <-
        below_min_points(r$h_control_data, input$min_points)

      if (nrow(excl_min_points_h_control) > 0) {
        r$h_control_data <-
          anti_join(r$h_control_data, excl_min_points_h_control)
        r$excluded_data <-
          rbind(r$excluded_data, excl_min_points_h_control)
        toastr_warning(paste(
          "Excluded from the historical control data: ",
          length(unique(excl_min_points_h_control$animal_id)),
          " animal(s)"
        ))
      }

    })

    observeEvent(input$clear, {
      session$reload()
    })

  })
}
