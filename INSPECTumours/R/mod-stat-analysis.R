#' mod_stat_analysis UI Function
#' @param id
#'
#' @noRd
#'
#' @importFrom shiny NS fluidPage h3 br textOutput actionButton uiOutput
mod_stat_analysis_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h3("Statistical analysis (optional)"),
    textOutput(ns("header_text")),
    br(),
    actionButton(ns("start_button"), "Start"),
    br(),
    br(),
    uiOutput(ns("stat_results"))
  )

}


#' mod_stat_analysis Server Function
#' @param id,input,output,session internal parameters for {shiny}
#' @param r reactiveValues with data
#'
#' @noRd
#'
#' @importFrom shiny observe renderText renderPlot renderUI tabsetPanel
#' plotOutput h5
#' @importFrom rlang .data
mod_stat_analysis_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # enable/disable Start button
    observe({
      toggleState(id = "start_button",
                  condition = !is.null(r$classify_final_filtered))
    })

    # show spinner while fitting models
    w <- set_waiter("Statistical analysis in progress")

    observeEvent(input$start_button, {

      req(r$classify_final_filtered)

      w$show()

      tryCatch({

        # table representing number of animals in classification groups
        r$df_animal_number <-
          animal_info_classification(r$classify_final_filtered)

        # plot representing number of animals in classification groups
        r$plot_animal_number <-
          plot_animal_info(r$classify_final_filtered, classification_colors)

        # convert tumour classification into ordinal data (0 - non-responder,
        # 1 - modest responder, 2 - stable responder, 3 - regressing responder)
        reference <- "Control"
        final_cl <- r$classify_final_filtered
        dat_order <- final_cl %>%
          mutate(
            order = ordered(.data$classification,
                            levels = levels(.data$classification)),
            treatment = factor(.data$treatment,
                               levels = c(
                                 reference,
                                 unique(final_cl$treatment)[unique(final_cl$treatment) != reference]
                               ))
          )

        # is study single or multistudy
        if (length(unique(dat_order$study)) == 1) {
          formula <- "order ~ treatment"
        } else {
          formula <- "order ~ treatment + (1|study)"
        }

        # fit model
        model_classification <-
          ordered_regression(dat_order, formula = formula, n_cores = 4)

        # make predictions
        predict <-
          predict_regr_model(model_classification, dat_order)

        # assess the efficacy
        out_bayes <- assess_efficacy(predict)
        r$df_efficacy <- out_bayes

        # make predictions for subcategories
        d_posterior <-
          classify_subcategories(dat_order, model_classification)

        # calculate probability of categories
        r$df_categories <- calc_probability(d_posterior)
        # plot estimated proportions
        r$plot_categories <-
          plot_proportions(d_posterior, classification_colors)

        r$stat_analysis_done <- TRUE

      },
      error = function(e) {
        print(e)
        shinyalert("Error during statistical analysis", type = "error")
      })


      w$hide()
    })



# outputs -----------------------------------------------------------------
    output$header_text <- renderText({
      if (is.null(r$classify_final_filtered)) {
        "Will be available after classification"
      }
    })

    output$animals_df <- renderDT({
      datatable(
        r$df_animal_number,
        extensions = "Buttons",
        options = list(
          dom = "Blfrtip",
          buttons = list(
            list(extend = "csv", filename = "Classifier_animal_number"),
            list(extend = "excel", filename = "Classifier_animal_number")
          ),
          text = "Download"
        )
      )
    }, server = FALSE)


    output$animals_plot <- renderPlot({
      r$plot_animal_number
    })

    output$drug_efficacy <- renderDT({
      req(r$df_efficacy)
      r$df_efficacy %>% mutate_if(is.numeric, ~ round(., 4))
    })

    output$probability <- renderDT({
      datatable(
        r$df_categories,
        caption = "Probability of categories",
        extensions = "Buttons",
        rownames = FALSE
      )
    })

    output$proportion <- renderPlot({
      r$plot_categories
    })

    output$stat_results <- renderUI({
      req(r$stat_analysis_done)
      ns <- session$ns
      tabsetPanel(
        tabPanel("Animal summary",
                 DTOutput(ns("animals_df")),
                 plotOutput(ns("animals_plot"))),
        tabPanel("Drug efficacy",
                 DTOutput(ns("drug_efficacy")),
                 h5("Asterisks indicate that the drug was considered
                    significantly effective by comparing to the control")),

        tabPanel(
          "Classification subcategories",
          DTOutput(ns("probability")),
          plotOutput(ns("proportion"))
        )
      )
    })

  })
}
