#' mod_qc UI Function
#' @param id the module id
#'
#' @noRd
#'
#' @importFrom plotly plotlyOutput
#' @importFrom shiny fluidRow br column
mod_qc_ui <- function(id, type) {
  ns <- NS(id)

  tagList(


    # controls for the QC New Data tab:
    if (type == "new") {
      fluidRow(
        br(),
        column(4,
               selectInput(ns("study_id"),
                           "Study Id",
                           choices = "",
                           selected = "")),
        column(4,
               selectInput(ns("treatments"),
                           label = "Treatments",
                           choices = "",
                           selected = "",
                           multiple = TRUE))
      )
    },

    # Plots
    fluidRow(plotlyOutput(ns("scatter_qc"))),
    fluidRow(plotlyOutput(ns("scatter_log_qc"))),

    if (type == "control") {
      plotlyOutput(ns("surv_plot"))
    }

  )

}
#' mod_qc Server Function
#' @param r reactiveValues with data
#' @param id,input,output,session internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom ggplot2 ggplot geom_line aes geom_hline labs scale_color_manual
#' theme_bw
#' @importFrom shiny updateSelectInput reactive
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr pull
mod_qc_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # value to store and modify data object
    df <- reactiveVal(NULL)


    observeEvent(r$new_data, {
      if (id == "new") {

        df(r$new_data)
      }
    }, ignoreNULL = FALSE)

    observeEvent(c(r$new_control_data, r$h_control_data), {

      if (id == "control") {
        all_controls <-
          bind_rows(
            "new" = r$new_control_data,
            "h_control" = r$h_control_data,
            .id = "control"
          )
        df(all_controls)
      }
    }, ignoreNULL = FALSE)


# update values of a select inputs ----------------------------------------
    observeEvent(df(), {
      if (id == "new") {
        updateSelectInput(session, "study_id", choices = unique(df()$study))
      }
    })

    observeEvent(input$study_id, {
      study_treatments <- df() %>%
        filter(.data$study == input$study_id) %>%
        pull(.data$treatment) %>%
        unique()
      updateSelectInput(session,
                        "treatments",
                        choices = study_treatments,
                        selected = study_treatments)
    }, ignoreInit = TRUE)

# plots -------------------------------------------------------------------

    data_pl <- reactive({
      if (id == "new" & !is.null(df())) {

        df <- filter(df(), .data$study == input$study_id &
                   .data$treatment %in% input$treatments)
        facet <- "treatment"
      } else {
        df <- df()
        facet <- "study"
      }
      return(list(df = df, facet = facet))

    })


    output$scatter_qc <- renderPlotly({
      req(nrow(data_pl()$df) > 0)
      p_title <- if (id == "new") {
        paste("Tumour volume,", input$study_id)
      } else {
        "Tumour volume for the control data"
      }
        plotly_volume(
          data_pl()$df,
          y_var = "tumour_volume",
          y_name = "Tumour volume (mm3)",
          faceting_var = data_pl()$facet,
          col_palette = az_pal,
          p_title = p_title
        )

    })


    output$scatter_log_qc <- renderPlotly({

      req(nrow(data_pl()$df) > 0)
      p_title <- if (id == "new") {
        paste("Log tumour volume,", input$study_id)
      } else {
        "Log tumour volume for the control data"
      }
      plotly_volume(
        data_pl()$df,
        y_var = "log_tv",
        y_name = "Log10(tumour volume)",
        faceting_var = data_pl()$facet,
        col_palette = az_pal,
        p_title = p_title
      )
    })


    output$surv_plot <- renderPlotly({

      req(nrow(data_pl()$df) > 0)
      df_survival <- calc_survived(data_pl()$df)

      plot_palette <- expand_palette(az_pal, length(unique(df_survival$study)))

      p <- ggplot(data = df_survival) +
        geom_line(
          aes(
            x = .data$day,
            y = .data$freq_survive * 100,
            color = .data$study
          ),
          size = 1,
          alpha = 0.7
        ) +
        geom_hline(aes(yintercept = 50), linetype = "dashed") +
        labs(title = "The percentage of survived animals",
             y = "Percentage of survival (%)", x  = "Day") +
        scale_color_manual(values = plot_palette) +
        theme_bw()

      ggplotly(p)
    })


  })
}
