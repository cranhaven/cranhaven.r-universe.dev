#' Characteristic limits Fitting Curve Hottable Server Module
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny rhandsontable
#' @noRd
mod_limits_fit_curve_hot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe(

    if (input$load_fit_data_check_limits == FALSE && input$curve_check_limits == FALSE && input$no_curve == FALSE) {
      updateAwesomeCheckbox(session, inputId = "no_curve", value = TRUE)
    })

    observeEvent(input$load_fit_data_check_limits, {
      if (input$load_fit_data_check_limits == TRUE) {
        updateAwesomeCheckbox(session, inputId = "curve_check_limits", value = FALSE)
        updateAwesomeCheckbox(session, inputId = "no_curve", value = FALSE)
      }
    })

    observeEvent(input$curve_check_limits, {
      if (input$curve_check_limits == TRUE) {
        updateAwesomeCheckbox(session, inputId = "load_fit_data_check_limits", value = FALSE)
        updateAwesomeCheckbox(session, inputId = "no_curve", value = FALSE)
      }
    })

    observeEvent(input$no_curve, {
      if (input$no_curve == TRUE) {
        updateAwesomeCheckbox(session, inputId = "curve_check_limits", value = FALSE)
        updateAwesomeCheckbox(session, inputId = "load_fit_data_check_limits", value = FALSE)
      }
    })

    table_reset <- reactiveValues(value = 0)

    observeEvent(input$formula_select_limits, {
      table_reset$value <- 1
    })

    previous_coeffs <- reactive({
      formula_select <- input$formula_select_limits

      if (formula_select == "lin-quad") {
        fit_coeffs_names <- c("coeff_C", "coeff_alpha", "coeff_beta")
      } else if (formula_select == "lin") {
        fit_coeffs_names <- c("coeff_C", "coeff_alpha")
      }

      full_data <- data.frame(
        matrix(
          0.0,
          nrow = length(fit_coeffs_names),
          ncol = 2
        )
      ) %>%
        `row.names<-`(fit_coeffs_names) %>%
        `colnames<-`(c("estimate", "std.error"))

      return(full_data)
    })

    changed_coeffs_data <- reactive({
      if (is.null(input$fit_coeffs_hot_limits) | isolate(table_reset$value == 1)) {
        table_reset$value <- 0
        return(previous_coeffs())
      } else if (!identical(previous_coeffs(), input$fit_coeffs_hot_limits)) {
        fit_coeffs_names <- row.names(hot_to_r(input$fit_coeffs_hot_limits))

        mytable <- as.data.frame(hot_to_r(input$fit_coeffs_hot_limits)) %>%
          dplyr::mutate_all(as.numeric) %>%
          `row.names<-`(fit_coeffs_names)

        return(mytable)
      }
    })


  # Output ---------------------------------------------------------------------
    output$fit_coeffs_hot_limits <- renderRHandsontable({
      num_cols <- ncol(changed_coeffs_data())

      hot <- changed_coeffs_data() %>%
        fix_coeff_names(type = "rows", output = "rhot") %>%
        rhandsontable(
          width = (50 + num_cols * 100),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 100, halign = "htCenter") %>%
        hot_cols(format = "0.000000")

      return(hot)
    })

  })
}
#' Characteristic Limits Fit Curve Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#'
#' @import shiny rhandsontable
#' @noRd
mod_limits_fit_curve_server <- function(id, aberr_module) {
  moduleServer(id, function(input, output, session) {

    data_file_curve <- eventReactive(input$button_get_limits_file, {

      isolate({
        load_fit_data <- input$load_fit_data_check_limits
        fit_data <- input$load_fit_data_limits
      })

      isolate({
        if (load_fit_data) {
          cat("3")
          fit_results_list <- readRDS(fit_data$datapath)
        } else {

          model_formula <- input$c
          # Parse formula
          fit_formula_tex <- parse_model_formula(model_formula)$fit_formula_tex

          fit_coeffs_raw <- hot_to_r(input$fit_coeffs_hot_limits)
          fit_coeffs <- fit_coeffs_raw %>%
            as.matrix()

          fit_results_list <- list(
            fit_formula_tex = fit_formula_tex,
            fit_coeffs = fit_coeffs
          )
        }
      })

      return(fit_results_list)
    })

    # Results outputs ----------------------------------------------------------
    output$fit_formula_tex <- renderUI({
      # Fitting formula
      if(input$load_fit_data_check_limits){
        withMathJax(paste0("$$", data_file_curve() [["fit_formula_tex"]], "$$"))
      }else if(input$no_curve){
        NULL
      }
    })

    output$fit_coeffs_limits <- renderRHandsontable({

      if(input$load_fit_data_check_limits){
        num_cols <- as.numeric(ncol(data_file_curve()[["fit_coeffs"]]))

        data_file_curve()[["fit_coeffs"]] %>%
          formatC(format = "e", digits = 3) %>%
          fix_coeff_names(type = "rows", output = "rhot") %>%
          # Convert to hot and format table
          rhandsontable(
            width = (50 + num_cols * 100),
            height = "100%"
          ) %>%
          hot_cols(colWidths = 100) %>%
          hot_cols(halign = "htCenter")
      }else if(input$no_curve){
        NULL
      }

    })

  })
}



#' Characteristic limits Results Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#' @param genome_factor Genomic conversion factor used in translocations.
#'
#' @import shiny rhandsontable utils
#' @importFrom rlang .data
#' @noRd

mod_limits_results_server <- function(id, aberr_module, genome_factor = NULL) {
  moduleServer(id, function(input, output = 0, session) {
    # Calculations ----
    reactive_tmp <- reactiveVal()

    # WITHOUT CURVE-------------------------------------------------------------

    data_no_curve <- eventReactive(input$button_get_limits, {
      req(input$button_get_limits)
      if (input$limits_select == "limits") {
        if(input$no_curve){

          fit_coeffs <- NULL
          calculate_table(input, project_yield, fit_coeffs)
        }
      }} )

    # CURVE MANUAL--------------------------------------------------------------

    data_curve_manual <- eventReactive(input$button_get_limits_curve_manual, {
      req(input$button_get_limits_curve_manual)
      if (input$limits_select == "limits") {
        if(input$curve_check_limits){
          req(any(hot_to_r(input$fit_coeffs_hot_limits) > 0))
            fit_coeffs_raw <- hot_to_r(input$fit_coeffs_hot_limits)
            fit_coeffs <- fit_coeffs_raw %>%
              as.matrix()
            calculate_table(input, project_yield, fit_coeffs)

        }
      }
    })
    # CURVE RDS-----------------------------------------------------------------
    data_curve_rds <- eventReactive(input$button_get_limits_file, {
      req(input$button_get_limits_file)
      if (input$limits_select == "limits") {
        if(input$load_fit_data_check_limits){
          req(input$load_fit_data_limits)
          fit_data <- input$load_fit_data_limits
          fit_results_list <- readRDS(fit_data$datapath)
          fit_coeffs <- fit_results_list$fit_coeffs

          calculate_table(input, project_yield, fit_coeffs)

        }
      }})

    #POISSON--------------------------------------------------------------------

    data_poisson <- reactive({

      if (input$limits_select == "p_poisson_test") {
        input$button_compare_control
        cli::cli_h1("Poisson test calculations")
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Perform poisson test", value = 0)

        isolate({
          control_data <- c(aberr = input$num_aberrs_control_comp,
                            cells = input$num_cells_control_comp)

          proband_data <- c(aberr = input$num_aberrs_proband_comp,
                            cells = input$num_cells_proband_comp)
        })

        results_list <- list(
          dec_thresh = matrix(rep(1, 18), nrow = 6, ncol = 3),
          p_value = matrix(c(
            control_data["aberr"] / control_data["cells"],
            proband_data["aberr"] / proband_data["cells"],
            stats::poisson.test(c(proband_data["aberr"], control_data["aberr"]),
                         c(proband_data["cells"], control_data["cells"]))$p.value
          ), ncol = 3, nrow = 1)
        )
      }
    })

    #OUTPUTS--------------------------------------------------------------------
    output$p_value <- renderRHandsontable({
      if (input$button_compare_control <= 0) {
        return(NULL)
      } else {
        num_cols <- as.numeric(ncol(data_poisson()[["p_value"]]))
        data_poisson()[["p_value"]] %>%
          formatC(format = "g", digits = 4) %>%
          rhandsontable(
            width = num_cols * 70,
            height = "100%", readOnly = TRUE, rowHeaders = FALSE,
            colHeaders = c("dics/cell (control)", "dics/cell (case)", "P-value")
          ) %>%
          hot_cols(colWidths = 70, halign = "htCenter" ) %>%
          hot_col(col = "P-value", renderer = "
          function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.NumericRenderer.apply(this, arguments);
            if (value < 0.05) {
              td.style.background = 'rgba(255, 0, 0, 0.2)';
            }
          }")
      }
    })

    output$decision_threshold <- renderRHandsontable({
      if(input$no_curve){
        data <- data_no_curve
      } else if (input$curve_check_limits){
        data <- data_curve_manual
      }else if (input$load_fit_data_check_limits){
        data <- data_curve_rds
      }
      num_cols <- as.numeric(ncol(data()[["dec_thresh"]])) + 1
      tmp.dat <- apply(data()[["dec_thresh"]], 2, as.numeric)
      tmp.dat <- round(cbind(tmp.dat[, 1:2], tmp.dat[, 2] + 1, tmp.dat[, 3:5]), 3)
      tmp.dat <- matrix(apply(tmp.dat, 2, as.character)[2:nrow(data()[["dec_thresh"]]), ], ncol = 6)

      reactive_tmp(tmp.dat)
      tmp.dat %>%
        rhandsontable(
          width = num_cols * 70,
          height = "100%", readOnly = TRUE, rowHeaders = FALSE,
          colHeaders = c("Number cells case", "Decision threshold (y)",
                         "Min. dics significant (y+1)", "Detection limit",
                         "Minimum resolvable dose (Gy)",
                         "Dose at detection limit (Gy)")
        ) %>%
        hot_cols(colWidths = 70, halign = "htCenter")
    })


    output$save_limits <- downloadHandler(
      filename = function() {
        paste("Charac_limits_", Sys.Date(), input$save_limits_format, sep = "")
      },
      content = function(file, verbose = TRUE) {

        data_to_save <- reactive_tmp()

        col_headers <- c("Number cells case", "Decision threshold (y)",
                         "Min. dics significant (y+1)", "Detection limit",
                         "Minimum resolvable dose (Gy)",
                         "Dose at detection limit (Gy)")

        colnames(data_to_save) <- col_headers

        if (input$save_limits_format == ".csv") {
          write.csv(data_to_save, file, row.names = FALSE)
        } else if (input$save_limits_format == ".tex") {
          if(verbose){
            print(xtable::xtable(data_to_save), type = "latex", file)
          }
        }
      }
    )

  })}
