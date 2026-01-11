#' mod_exclude UI Function
#' @param id the module id
#' @param type historical control or new
#'
#' @noRd
#'
#' @importFrom shiny fluidRow column textInput uiOutput
mod_exclude_ui <- function(id, type) {
  ns <- NS(id)
  tagList(
    if (type == "new") {
      h4("Exclude data")
    } else {
      h4("Exclude historical control data")
    },
    fluidRow(
      column(3,
             selectInput(ns("study_id_ex"),
                         "Study Id",
                         choices = "",
                         selected = "")),
      column(3,
             selectInput(ns("animal_id_ex"),
                         "Animal Id",
                         choices = "",
                         selected = "")),
      column(3,
             selectInput(ns("day_ex"),
                         "Day",
                         choices = "",
                         selected = "")),

      column(3,
             textInput(ns("reason"),
                       "Reason"))
    ),
    actionButton(ns("exclude"), "Exclude", class = "btn-danger"),
    uiOutput(ns("excluded_table"))
  )
}

#' mod_exclude Server Function
#' @param id,input,output,session internal parameters for {shiny}
#' @param r reactiveValues with data
#' @param type historical control or new
#'
#' @noRd
#'
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shinyjs enable disable
#' @importFrom DT renderDT DTOutput
#' @importFrom shiny observe updateSelectInput updateTextInput renderUI isolate
#' @importFrom dplyr anti_join select pull
#' @importFrom rlang .data
#' @importFrom shinytoastr toastr_warning
mod_exclude_server <- function(id, r, type) {
  moduleServer(id, function(input, output, session) {

    # data --------------------------------------------------------------------
    df <- reactiveVal(NULL)

    observeEvent(r$new_data, {
      if (type == "new") {
        df(r$new_data)
      }
    })

    observeEvent(r$h_control_data, {
      if (type == "control") {
        df(r$h_control_data)
      }
    })

    # update selectors --------------------------------------------------------
    observeEvent(df(), {
      req(df())
      updateSelectInput(session, "study_id_ex", choices = unique(df()$study))
    })

    observeEvent(input$study_id_ex, {
      ids <- df() %>%
        filter(.data$study == input$study_id_ex) %>%
        pull(.data$animal_id) %>%
        unique()
      if (type == "control") {
        ids <- as.character(ids) %>% append("All")
      }
      updateSelectInput(session, "animal_id_ex", choices = ids)
    }, priority = 2, ignoreInit = TRUE)

    observe({
      req(input$animal_id_ex != "")
      days <- isolate(df()) %>%
        filter(.data$study == input$study_id_ex &
                 .data$animal_id == input$animal_id_ex) %>%
        pull(.data$day) %>%
        append("All")
      updateSelectInput(session, "day_ex", choices = days, selected = "All")
    }, priority = 1)

    # exclude data ------------------------------------------------------------

    # validation rule for the required field
    iv <- InputValidator$new()
    iv$add_rule("reason", sv_required())
    iv$enable()

    observeEvent(input$reason, {
      if (input$reason != "") {
        enable("exclude")
      } else {
        disable("exclude")
      }
    })

    observeEvent(input$exclude, {

      excluded_data <- exclude_data(df = df(),
                                    study_id_ex = input$study_id_ex,
                                    animal_id_ex = input$animal_id_ex,
                                    day_ex = input$day_ex,
                                    reason = input$reason)
      if (nrow(excluded_data) == 0) {
        toastr_warning("Something is wrong with the selection",
                       position = "bottom-right")
      }
      req(nrow(excluded_data) > 0)

      # anti_join() return all rows from x without a match in y.
      updated_df <- anti_join(df(), excluded_data)

      # update global data.frame
      if (type == "new") {
        r$new_data <- updated_df
        r$new_control_data <- filter(r$new_data, .data$treatment == "Control")
      } else if (type == "control") {
        r$h_control_data <- updated_df
      }

      # add to df with excluded rows
      r$excluded_data <- bind_rows(r$excluded_data, excluded_data)

      updateTextInput(session, "reason", value = "")

    })

    # render table with excluded data
    output$table <- renderDT({
      req(r$excluded_data)
      r$excluded_data
    })

    output$excluded_table <- renderUI({
      req(r$excluded_data)
      ns <- session$ns
      tagList(
        h4("Excluded data"),
        DTOutput(ns("table")))
    })


  })

}
