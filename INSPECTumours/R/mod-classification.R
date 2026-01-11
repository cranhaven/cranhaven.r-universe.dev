#' mod_classification UI Function
#' @param id the module id
#'
#' @noRd
#'
#' @importFrom shiny NS tagList numericInput selectInput checkboxInput
#' actionButton div tags span sidebarLayout
#' sidebarPanel mainPanel tabsetPanel textOutput uiOutput
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @importFrom tippy tippy_this
mod_classification_ui <- function(id) {
  ns <- NS(id)


  tagList(sidebarLayout(
    sidebarPanel(

      div(
        style = "display: flex;",
        tags$h3("Analysis settings",
                id = "settings_text",
                style = "margin-top: 0px;"),
        span(icon("info-circle"), id = "info")
      ),

      tippy_this(
        elementId = "info",
        tooltip = "You can find information about settings in the Info tab",
        placement = "right"
      ),

      numericInput(
        inputId = ns("mod_end_day"),
        label = "End day of modelling",
        value = NULL
      ),

      numericInput(
        inputId = ns("cut_off"),
        value = NA,
        label = "Cut-off day for responder classification (optional)"
      ),

      numericInput(
        inputId = ns("con_meas"),
        label = "Consecutive measurements per week",
        value = 3
      ),

      selectInput(
        ns("model_type"),
        label = "Model type",
        choices = c("Two-stage non-linear model", "Linear model"),
        selected = "Two-stage non-linear model"
      ),

      checkboxInput(ns("qc_aware"), "QC check"),
      actionButton(ns("start_button"), "Start")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Control growth profile",
                 plotlyOutput(ns(
                   "control_growth_plot"
                 ))),

        tabPanel("Classification table",
                 DTOutput(ns(
                   "classification_table"
                 )),
                 textOutput(ns("not_reliable"))),

        tabPanel("Classification over tumour volume",
                 uiOutput(ns(
                   "classification_tv"
                 ))),

        tabPanel("Classification over growth rate",
                 plotlyOutput(ns(
                   "classification_gr"
                 ))),

        tabPanel("Waterfall plot",
                 uiOutput(ns("waterfall")))

      )
    )
  ))



}

#' mod_classification Server Function
#' @param id,input,output,session internal parameters for {shiny}
#' @param r reactiveValues with data
#'
#' @noRd
#'
#' @importFrom dplyr left_join
#' @importFrom shiny moduleServer observeEvent reactiveVal req h3 h4 observe
#' renderUI renderText
#' @importFrom DT renderDT datatable
#' @importFrom shinyjs toggleState
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom purrr imap map
#' @importFrom dplyr group_map desc
#' @importFrom stats pt
#' @importFrom rlang .data
#' @importFrom plotly ggplotly renderPlotly layout
mod_classification_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {


    # enable/disable Start Analysis button
    observe({
      toggleState(id = "start_button",
                  condition = input$qc_aware & !is.na(input$mod_end_day))
    })

    # validation rule for the required field - mod_end_day
    iv <- InputValidator$new()
    iv$add_rule("mod_end_day", sv_required())
    iv$enable()

    # show spinner while fitting models
    w <- set_waiter("Fitting models")

    observeEvent(input$start_button, {

      if (is.null(r$new_data)) {
        shinyalert("Please load data", type = "error")
      }
      req(r$new_data)

      # save settings
      r$mod_end_day <- input$mod_end_day
      r$con_meas <- input$con_meas
      r$model_type <- input$model_type
      r$cut_off <- input$cut_off

      file_names <- c(unique(r$new_data$file_name))
      if (!is.null(r$h_control_data)) {
        file_names <- c(file_names, unique(r$h_control_data$file_name))
      }
      r$file_names <- file_names

      # show loading spinner
      w$show()

      # use control data from the new study and historical control data
      # (if exists)
      if (!is.null(r$h_control_data)) {
        mod_data <- bind_rows(r$new_control_data, r$h_control_data)
      } else {
        mod_data <- r$new_control_data
      }

      out_model <- tryCatch({
        model_control(
          df_control = mod_data,
          df_newstudy = r$new_data,
          method = input$model_type,
          end_day = input$mod_end_day
        )
      },
      error = function(e) {
        print(e)
        shinyalert("Error while fitting the model", type = "error")
        return(NULL)
      })

      w$hide()
      req(!is.null(out_model))

      # plot a control growth profile
      r$p_control_growth <- mod_data %>%
        left_join(out_model$predict_control, by = c("study", "day")) %>%
        control_growth_plot(input$model_type, az_pal)

      tryCatch({

        # Classifying individual data points
        dat_classify <-
          classify_data_point(r$new_data, out_model$predict_newstudy)

        # apply cut-off
        if (!is.na(input$cut_off)) {
          dat_classify <- filter(dat_classify, .data$day <= input$cut_off)
        }

        # calc growth rate
        dat_gr <- dat_classify %>%
          group_by(.data$study, .data$animal_id) %>%
          group_map(~calc_gr(.x), .keep = TRUE) %>%
          bind_rows()

        # Classifying individual tumours
        dat_classify_tumour <- dat_classify %>%
          group_by(.data$study, .data$animal_id,
                   .data$treatment) %>%
          summarise(
            classify_tumour = get_responder(.data$classify_point,
                                            input$con_meas),
            N = length(.data$tumour_volume)
          ) %>%
          ungroup() %>%
          left_join(dat_gr, by = c("study", "treatment", "animal_id")) %>%
          mutate(p_value = 2 * pt(
            abs(.data$gr / .data$gr_se),
            df = .data$N - 1,
            lower.tail = FALSE
          ))

        # final classification
        r$classify_final <- classify_type_responder(dat_classify_tumour)

        # exclude  'Not reliable'
        r$classify_final_filtered <-
          r$classify_final %>%
          filter(
            .data$classification %in% c(
              "Non-responder",
              "Modest responder",
              "Stable responder",
              "Regressing responder"
            )
          ) %>%
          droplevels()

        # create ggplot2 object
        r$classification_gr_plot <- plot_class_gr(
          r$classify_final_filtered,
          classification_colors)

        tv_df <- dat_classify %>% left_join(r$classify_final,
                    by = c("study", "animal_id", "treatment"))

        # classification over tumour volume plot(s)
        r$classification_tv_plot <-
          lapply(unique(tv_df$study), function(study_name) {
            tv_subset <- filter(tv_df, .data$study == study_name)
            p <- plot_class_tv(tv_subset, classification_colors, study_name)
        })

        # waterfall plot(s)
        dat_last_measure <- r$new_data %>%
          group_by(.data$study, .data$treatment, .data$animal_id) %>%
          mutate(last_measure = ifelse(
            max(.data$day, na.rm = TRUE) >= input$mod_end_day,
            min(.data$day[which(.data$day >= input$mod_end_day)],
                na.rm = TRUE),
            max(.data$day, na.rm = TRUE)
          )) %>%
          filter(.data$day == .data$last_measure)

        mean_control <- filter(dat_last_measure,
                               .data$treatment == "Control") %>%
          group_by(.data$study) %>%
          summarise(mean_control = mean(.data$tumour_volume,
                                        na.rm = TRUE))

        dat_waterfall <- dat_last_measure %>%
          left_join(mean_control, by = "study") %>%
          mutate(
            percent_change_from_control_mean =
              (.data$tumour_volume - .data$mean_control) / mean_control * 100
          ) %>%
          left_join(r$classify_final,
                    by = c("study", "treatment", "animal_id")) %>%
          filter(.data$classification != "Not reliable") %>%
          arrange(
            .data$study,
            .data$treatment,
            desc(.data$percent_change_from_control_mean),
            .data$classification
          )


        r$classification_waterfall_plot <-
          lapply(unique(dat_waterfall$study), function(i) {
            waterfall_subset <- dat_waterfall %>%
              filter(.data$study == i)
            waterfall_subset$animal_id <-
              factor(waterfall_subset$animal_id,
                     levels = unique(waterfall_subset$animal_id))

            plot_waterfall(waterfall_subset, classification_colors, i)
          })

      },
      error = function(e) {
        print(e)
        shinyalert("Error during the classification process", type = "error")
      })

    })

    # outputs -----------------------------------------------------------------

    output$classification_table <- renderDT({
      req(r$classify_final)
      r$classify_final %>%
        mutate_if(is.numeric, ~ round(., 4)) %>%
        datatable(
          extensions = "Buttons",
          options = list(
            dom = "Blfrtip",
            buttons = list(
              list(extend = "csv", filename = "Tumour_classifier"),
              list(extend = "excel", filename = "Tumour_classifier")
            ),
            text = "Download"
          )
        )
    }, server = FALSE)

    output$not_reliable <- renderText({
      req(r$classify_final)
      "Non reliable means a tumour didn't grow at all. If this tumour is in the
      control group, we recommend you remove this animal in QC session and
      re-run the analysis. If this tumour is in the treated group, the tool
      will remove it automatically for all down-stream analysis."
    })

    output$control_growth_plot <- renderPlotly({
      req(r$p_control_growth)
      ggplotly(r$p_control_growth)
    })

    output$classification_gr <- renderPlotly({

      req(r$classification_gr_plot)
      p <- ggplotly(r$classification_gr_plot)
      p[["x"]][["data"]] <- map(p[["x"]][["data"]], ~ hide_outliers(.))
      p

    })


    output$classification_tv <- renderUI({

      req(r$classification_tv_plot)
      ns <- session$ns

      imap(r$classification_tv_plot, ~ {
        output[[paste0("tv_plot",  .y)]] <-
          renderPlotly({
            ggplotly(.x) %>% layout(margin = list(b = 95))
          })
        plotlyOutput(ns(paste0("tv_plot",  .y)))
      })

    })

    output$waterfall <- renderUI({

      req(r$classification_waterfall_plot)
      ns <- session$ns

      imap(r$classification_waterfall_plot, ~ {
        output[[paste0("waterfall_plot",  .y)]] <- renderPlot(.x)
        plotOutput(ns(paste0("waterfall_plot",  .y)))
      })
    })

  })
}
