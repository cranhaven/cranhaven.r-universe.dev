#' Dose Estimation Fitting Curve Hottable Server Module
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny rhandsontable
#' @noRd
mod_estimation_fit_curve_hot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reset tables ----
    table_reset <- reactiveValues(value = 0)
    table_var_reset <- reactiveValues(value = 0)

    observeEvent(input$formula_select, {
      table_reset$value <- 1
      table_var_reset$value <- 1
    })

    # Initialise data frames ----
    previous_coeffs <- reactive({
      # Create button dependency for updating dimensions
      #input$button_gen_table

      #isolate({
        formula_select <- input$formula_select
      #})

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

    previous_var <- reactive({
      # Create button dependency for updating dimensions
      #input$button_gen_table

      #isolate({
        model_formula <- input$formula_select
      #})

      fit_coeffs_names <- names_from_model_formula(model_formula)

      full_data <- matrix(
        0.0,
        nrow = length(fit_coeffs_names),
        ncol = length(fit_coeffs_names)
      ) %>%
        `row.names<-`(fit_coeffs_names) %>%
        `colnames<-`(fit_coeffs_names) %>%
        as.data.frame()

      return(full_data)
    })

    # Reactive data frames ----
    changed_coeffs_data <- reactive({
      # Create button dependency for updating dimensions
      #input$button_gen_table

      if (is.null(input$fit_coeffs_hot) | isolate(table_reset$value == 1)) {
        table_reset$value <- 0
        return(previous_coeffs())
      } else if (!identical(previous_coeffs(), input$fit_coeffs_hot)) {
        fit_coeffs_names <- row.names(hot_to_r(input$fit_coeffs_hot))

        mytable <- as.data.frame(hot_to_r(input$fit_coeffs_hot)) %>%
          dplyr::mutate_all(as.numeric) %>%
          `row.names<-`(fit_coeffs_names)

        return(mytable)
      }
    })

    changed_var_data <- reactive({
      # Create button dependency for updating dimensions
      #input$button_gen_table

      if (is.null(input$fit_var_cov_mat_hot) | isolate(table_var_reset$value == 1)) {
        table_var_reset$value <- 0
        return(previous_var())
      } else if (!identical(previous_var(), input$fit_var_cov_mat_hot)) {
        fit_coeffs_names <- row.names(hot_to_r(input$fit_var_cov_mat_hot))

        mytable <- as.data.frame(hot_to_r(input$fit_var_cov_mat_hot)) %>%
          dplyr::mutate_all(as.numeric) %>%
          `row.names<-`(fit_coeffs_names)

        return(mytable)
      }
    })

    # Output ----
    output$fit_coeffs_hot <- renderRHandsontable({
      # Read number of columns
      num_cols <- ncol(changed_coeffs_data())

      # Convert to hot and format table
      hot <- changed_coeffs_data() %>%
        fix_coeff_names(type = "rows", output = "rhot") %>%
        rhandsontable(
          width = (50 + num_cols * 100),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 100) %>%
        hot_cols(format = "0.000000")

      hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
      return(hot)
    })

    output$fit_var_cov_mat_hot <- renderRHandsontable({
      # Read number of columns
      num_cols <- ncol(changed_var_data())

      hot <- changed_var_data() %>%
        fix_coeff_names(type = "rows", output = "rhot") %>%
        fix_coeff_names(type = "cols", output = "rhot") %>%
        # Convert to hot and format table
        rhandsontable(
          width = (50 + num_cols * 100),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 100) %>%
        hot_cols(format = "0.00000000")

      hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
      return(hot)
    })
  })
}

#' Dose Estimation Fitting Curve Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#'
#' @import shiny rhandsontable
#' @noRd
mod_estimation_fit_curve_server <- function(id, aberr_module) {
  moduleServer(id, function(input, output, session) {
    # Calculations ----
    data <- reactive({
      input$button_view_fit_data

      isolate({
        load_fit_data <- input$load_fit_data_check
        fit_data <- input$load_fit_data
      })

      input$button_view_fit_data
      fit_results_list <- NULL

      isolate({
          if (load_fit_data) {

          if(!is.null(fit_data)){
            fit_results_list <- readRDS(fit_data$datapath)

            # Additional info for translocations module
            if (aberr_module == "translocations") {
              fit_genome_factor <- fit_results_list[["genome_factor"]]

              # Message about used translocation frequency
              if (fit_results_list[["frequency_select"]] == "measured_freq") {
                trans_frequency_message <- paste0("The provided observed fitting curve has been converted to full genome, with a genomic conversion factor of ", round(fit_genome_factor, 3), ".")
              } else {
                trans_frequency_message <- "The provided fitting curve is already full genome."
              }
              fit_results_list[["fit_trans_frequency_message"]] <- trans_frequency_message

              # Conversion of coefficients and statistics
              if (fit_results_list[["frequency_select"]] == "measured_freq") {
                # Update coefficients
                fit_results_list[["fit_coeffs"]][, "estimate"] <- fit_results_list[["fit_coeffs"]][, "estimate"] / fit_genome_factor
                fit_results_list[["fit_coeffs"]][, "std.error"] <- fit_results_list[["fit_coeffs"]][, "std.error"] / fit_genome_factor

                # Update variance-covariance matrix
                fit_results_list[["fit_var_cov_mat"]] <- fit_results_list[["fit_var_cov_mat"]] / fit_genome_factor^2

                # Update model-specific statistics
          #      fit_results_list[["fit_model_statistics"]] <- calculate_model_stats(
          #        model_data = fit_results_list[["fit_raw_data"]],
          #        fit_coeffs_vec = fit_results_list[["fit_coeffs"]][, "estimate"],
          #        fit_algorithm = fit_results_list[["fit_algorithm"]],
          #        response = "yield", link = "identity", type = "theory",
          #        genome_factor = fit_genome_factor,
          #        calc_type = "estimation"
          #      )
              }
            }
          }
        } else {
          model_formula <- input$formula_select
          # Parse formula
          fit_formula_tex <- parse_model_formula(model_formula)$fit_formula_tex

          fit_coeffs_raw <- hot_to_r(input$fit_coeffs_hot)
          fit_coeffs <- fit_coeffs_raw %>%
            # cbind(statistic = c(rep(0, nrow(fit_coeffs_raw)))) %>%
            as.matrix()

          if (input$use_var_cov_matrix) {
            fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot) %>%
              as.matrix()
          } else {
            # Calculate var-cov matrix when none is provided
            fit_var_cov_mat <- matrix(
              0,
              nrow = nrow(fit_coeffs),
              ncol = nrow(fit_coeffs)
            ) %>%
              `colnames<-`(rownames(fit_coeffs)) %>%
              `rownames<-`(rownames(fit_coeffs))

            for (x_var in rownames(fit_var_cov_mat)) {
              fit_var_cov_mat[[x_var, x_var]] <- fit_coeffs[x_var, "std.error"] * fit_coeffs[x_var, "std.error"]
            }
          }

          fit_cor_mat <- fit_var_cov_mat
          for (x_var in rownames(fit_var_cov_mat)) {
            for (y_var in colnames(fit_var_cov_mat)) {
              fit_cor_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var] /
                (fit_coeffs[x_var, "std.error"] * fit_coeffs[y_var, "std.error"])
            }
          }

          # Conversion of coefficients and statistics
          if (aberr_module == "translocations") {
            if (input$frequency_select == "measured_freq") {
              fit_genome_factor <- input$fit_genome_factor

              # Update coefficients
              fit_coeffs[, "estimate"] <- fit_coeffs[, "estimate"] / fit_genome_factor
              fit_coeffs[, "std.error"] <- fit_coeffs[, "std.error"] / fit_genome_factor

              # Update variance-covariance matrix
              fit_var_cov_mat <- fit_var_cov_mat / fit_genome_factor^2
            }
          }

          fit_results_list <- list(
            fit_formula_tex = fit_formula_tex,
            fit_coeffs = fit_coeffs,
            fit_var_cov_mat = fit_var_cov_mat,
            fit_cor_mat = fit_cor_mat
          )

          # Additional info for translocations module
          if (aberr_module == "translocations") {
            # Message about used translocation frequency
            if (input$frequency_select == "measured_freq") {
              trans_frequency_message <- paste0("The provided observed fitting curve has been converted to full genome, with a genomic conversion factor of ", input$fit_genome_factor, ".")
            } else {
              trans_frequency_message <- "The provided fitting curve is already full genome."
            }
            fit_results_list[["fit_trans_frequency_message"]] <- trans_frequency_message
          }
        }
      })

      return(fit_results_list)
    })

    # Results outputs ----
    output$fit_formula_tex <- renderUI({
      # Fitting formula
      if (input$button_view_fit_data <= 0) {
        return(NULL)
      }

      withMathJax(HTML(paste0("$$", data()[["fit_formula_tex"]], "$$")))
    })

    output$fit_trans_frequency_message <- renderUI({
      # Fitting formula
      if (input$button_view_fit_data <= 0 | aberr_module != "translocations") {
        return(NULL)
      }
      data()[["fit_trans_frequency_message"]]
    })

    output$fit_model_statistics <- renderRHandsontable({
      # Model-level statistics
      if (input$button_view_fit_data <= 0) {
        return(NULL)
      }
      num_cols <- as.numeric(ncol(data()[["fit_model_statistics"]]))

      data()[["fit_model_statistics"]] %>%
        # Convert to hot and format table
        rhandsontable(
          width = (num_cols * 70),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 70, halign = "htCenter")
    })

    output$fit_coeffs <- renderRHandsontable({
      # Coefficients 'fit_coeffs'
      if (input$button_view_fit_data <= 0) {
        return(NULL)
      }
      num_cols <- as.numeric(ncol(data()[["fit_coeffs"]]))

      data()[["fit_coeffs"]] %>%
        formatC(format = "e", digits = 3) %>%
        fix_coeff_names(type = "rows", output = "rhot") %>%
        # Convert to hot and format table
        rhandsontable(
          width = (50 + num_cols * 100),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 100) %>%
        hot_cols(halign = "htCenter")
    })

    output$fit_var_cov_mat <- renderRHandsontable({
      # Variance-covariance matrix 'var_cov_mat'
      if (input$button_view_fit_data <= 0) {
        return(NULL)
      }
      num_cols <- as.numeric(ncol(data()[["fit_var_cov_mat"]]))

      data()[["fit_var_cov_mat"]] %>%
        formatC(format = "e", digits = 3) %>%
        fix_coeff_names(type = "rows", output = "rhot") %>%
        fix_coeff_names(type = "cols", output = "rhot") %>%
        # Convert to hot and format table
        rhandsontable(
          width = (50 + num_cols * 100),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 100) %>%
        hot_cols(halign = "htCenter")
    })

    output$fit_cor_mat <- renderRHandsontable({
      # Correlation matrix 'cor_mat'
      if (input$button_view_fit_data <= 0) {
        return(NULL)
      }
      num_cols <- as.numeric(ncol(data()[["fit_cor_mat"]]))

      data()[["fit_cor_mat"]] %>%
        fix_coeff_names(type = "rows", output = "rhot") %>%
        fix_coeff_names(type = "cols", output = "rhot") %>%
        # Convert to hot and format table
        rhandsontable(
          width = (50 + num_cols * 100),
          height = "100%"
        ) %>%
        hot_cols(colWidths = 100, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })
  })
}

#' Dose Estimation Case Hottable Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#' @param genome_factor Genomic conversion factor used in translocations.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_estimation_case_hot_server <- function(id, aberr_module, genome_factor = NULL) {
  moduleServer(id, function(input, output, session) {
    # Reset table ----
    table_reset <- reactiveValues(value = 0)

    observeEvent(input$button_upd_params, {
      if (nrow(as.data.frame(hot_to_r(input$case_data_hot))) < 2) {
        updateSelectInput(
          session,
          "error_method_whole_select",
          choices = list(
            "Merkle's method (83%-83%)" = "merkle-83",
            "Merkle's method (95%-95%)" = "merkle-95",
            "Delta method (95%)" = "delta"
          ),
          selected = "merkle-83"
        )
        updateSelectInput(
          session,
          "exposure_select",
          choices = list(
            "Acute" = "acute",
            "Protracted" = "protracted",
            "Highly protracted" = "protracted_high"
          ),
          selected = "acute"
        )
        if(aberr_module == "micronuclei"){
          updateSelectInput(
            session,
            "assessment_select",
            choices = list(
              "Whole-body"    = "whole-body"
            ),
            selected = "whole-body"
          )
        }else{
          updateSelectInput(
            session,
            "assessment_select",
            choices = list(
              "Whole-body"    = "whole-body",
              "Partial-body"  = "partial-body",
              "Heterogeneous" = "hetero"
            ),
            selected = "whole-body"
          )
        }
      } else {

        updateSelectInput(
          session,
          "assessment_select",
          choices = list(
            "Whole-body"    = "whole-body",
            "Partial-body"  = "partial-body"
          ),
          selected = "whole-body"
        )

      }
    })

    observeEvent(input$button_upd_table, {
      table_reset$value <- 1
    })

    # Initialise data frame ----
    previous <- reactive({
      # Create button dependency for updating dimensions
      input$button_upd_table

      isolate({
        load_case_data <- input$load_case_data_check
        case_data <- input$load_case_data
        num_cases <- as.numeric(input$num_cases)
        #num_cases <- 1
        num_aberrs <- as.numeric(input$num_aberrs) + 1
      })

      if (!load_case_data) {
        # Base data frame
        full_data <- data.frame(
          matrix(
            0,
            nrow = num_cases,
            ncol = num_aberrs
          )
        ) %>%
          `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))
      } else {
        full_data <- utils::read.csv(case_data$datapath, header = TRUE) %>%
          dplyr::rename_with(
            .fn = toupper,
            .cols = dplyr::everything()
          ) %>%
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::starts_with("C"),
              .fns = as.integer
            )
          ) %>%
          dplyr::select(
            dplyr::starts_with("C")
          )
      }

      return(full_data)
    })

    # Reactive data frame ----
    changed_data <- reactive({
      # Create button dependency for updating dimensions
      input$button_upd_table
      input$button_upd_params

      isolate({
        if (is.null(input$case_data_hot) | isolate(table_reset$value == 1)) {
          table_reset$value <- 0
          mytable <- previous()
          mytable$ID <- ""

          # Initial rendering of the table
          mytable <- init_aberr_table(
            data = mytable,
            type = "case",
            aberr_module
          )
        } else if (!identical(previous(), input$case_data_hot)) {
          mytable <- as.data.frame(hot_to_r(input$case_data_hot))

          # Calculated columns
          mytable <- calculate_aberr_table(
            data = mytable,
            type = "case",
            assessment_u = 1,
            aberr_module = aberr_module
          )

          # Rename mean and std_err depending on aberration module
          if (aberr_module == "translocations") {
            genome_factor <- genome_factor$genome_factor()

            mytable <- mytable %>%
              dplyr::mutate(
                Xc = dplyr::case_when(
                  input$trans_confounders & input$trans_confounders_type == "sigurdson" ~
                    calculate_trans_rate_sigurdson(
                      .data$N, genome_factor,
                      age_value = input$trans_confounder_age,
                      sex_bool = input$trans_confounder_sex,
                      sex_value = input$trans_sex,
                      smoker_bool = input$trans_confounder_smoke,
                      ethnicity_value = input$trans_confounder_ethnicity,
                      region_value = input$trans_confounder_region
                    ),
                  input$trans_confounders & input$trans_confounders_type == "manual" ~
                    calculate_trans_rate_manual(.data$N, genome_factor, input$trans_expected_aberr_value),
                  TRUE ~ 0
                ),
                Fg = correct_negative_vals(.data$X - .data$Xc) / (.data$N * genome_factor),
                Fg_err = .data$Fp_err / sqrt(genome_factor)
              )
          }

          return(mytable)
        }
      })
    })

    # Output ----
    output$case_data_hot <- renderRHandsontable({
      # Read number of columns
      num_cols <- as.numeric(ncol(changed_data()))

      # Convert to hot and format table
      hot <- changed_data() %>%
        rhandsontable(
          width = ((num_cols + 1) * 50) + 20,
          height = "100%"
        ) %>%
        hot_cols(colWidths = 50, halign = "htCenter")%>%
        hot_col("ID", colWidths = 70, halign = "htCenter")
      # hot_table(highlightCol = TRUE, highlightRow = TRUE)

      if (aberr_module %in% c("dicentrics", "micronuclei")) {
        hot <- hot %>%
          hot_col(c(2, 3, seq(num_cols - 3, num_cols, 1)), readOnly = TRUE)%>%
          hot_col(num_cols - 0, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }")
      } else if (aberr_module == "translocations") {
        hot <- hot %>%
          hot_col(c(2, 3, seq(num_cols - 6, num_cols, 1)), readOnly = TRUE) %>%
          hot_col(num_cols - 3, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value >1.96) {
              td.style.background = 'pink';
             }
           }")
      }

      hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
      return(hot)
    })
  })
}

#' Dose Estimation Results Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#' @param genome_factor Genomic conversion factor used in translocations.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_estimation_results_server <- function(id, aberr_module, genome_factor = NULL) {
  moduleServer(id, function(input, output, session) {
    reactive_data_est <- reactiveValues(
      fitting_rds = NULL,
      fitting_manual = NULL
    )

    data <- reactive({
      # Calcs: get variables ----
      input$button_estimate

      # Initialise progress object
      cli::cli_h1("Dose estimation calculations")
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Performing dose estimation", value = 0)

      isolate({
        # Fit data
        load_fit_data <- input$load_fit_data_check
        fit_data <- input$load_fit_data
        exposure <- input$exposure_select
        assessment <- input$assessment_select
        badge_check <- input$badge_dose_check
        badge_dose <- input$badge_dose
        # Cases data
        case_data <- hot_to_r(input$case_data_hot)

        # Coefficient input selection
        fraction_coeff <- input$fraction_coeff_select
      })

      # Get error/calculation methods
      error_method <- input$error_method_whole_select

      # Get fitting data ----
      fit_results_list <- NULL

      if (load_fit_data) {

        if(!is.null(fit_data)){

          fit_results_list <- readRDS(fit_data$datapath)

          reactive_data_est$fitting_rds <- fit_results_list
          max_curve <- max(as.numeric(fit_results_list$fit_raw_data[, "D"]))
          # Additional info for translocations module
          if (aberr_module == "translocations") {
            fit_genome_factor <- fit_results_list[["genome_factor"]]

            # Conversion of coefficients and statistics
            if (fit_results_list[["frequency_select"]] == "measured_freq") {
              # Update coefficients
              fit_results_list[["fit_coeffs"]][, "estimate"] <- fit_results_list[["fit_coeffs"]][, "estimate"] / fit_genome_factor
              fit_results_list[["fit_coeffs"]][, "std.error"] <- fit_results_list[["fit_coeffs"]][, "std.error"] / fit_genome_factor

              # Update variance-covariance matrix
              fit_results_list[["fit_var_cov_mat"]] <- fit_results_list[["fit_var_cov_mat"]] / fit_genome_factor^2
            }
          }
        }
      } else {
        model_formula <- input$formula_select
        # Parse formula
        fit_formula_tex <- parse_model_formula(model_formula)$fit_formula_tex

        fit_coeffs_raw <- hot_to_r(input$fit_coeffs_hot)
        fit_coeffs <- fit_coeffs_raw %>%
          cbind(statistic = c(rep(0, nrow(fit_coeffs_raw)))) %>%
          as.matrix() %>%
          `rownames<-`(names_from_model_formula(model_formula))

        max_curve <- input$max_dose_curve
        if (input$use_var_cov_matrix) {
          fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot) %>%
            as.matrix() %>%
            `colnames<-`(rownames(fit_coeffs)) %>%
            `rownames<-`(rownames(fit_coeffs))
        } else {
          # Calculate var-cov matrix when none is provided
          fit_var_cov_mat <- matrix(
            0,
            nrow = nrow(fit_coeffs),
            ncol = nrow(fit_coeffs)
          ) %>%
            `colnames<-`(rownames(fit_coeffs)) %>%
            `rownames<-`(rownames(fit_coeffs))

          for (x_var in rownames(fit_var_cov_mat)) {
            fit_var_cov_mat[[x_var, x_var]] <- fit_coeffs[x_var, "std.error"] * fit_coeffs[x_var, "std.error"]
          }
        }

        # Conversion of coefficients and statistics
        if (aberr_module == "translocations") {
          if (input$frequency_select == "measured_freq") {
            fit_genome_factor <- input$fit_genome_factor

            # Update coefficients
            fit_coeffs[, "estimate"] <- fit_coeffs[, "estimate"] / fit_genome_factor
            fit_coeffs[, "std.error"] <- fit_coeffs[, "std.error"] / fit_genome_factor

            # Update variance-covariance matrix
            fit_var_cov_mat <- fit_var_cov_mat / fit_genome_factor^2
          }
        }

        fit_results_list <- list(
          fit_formula_tex = fit_formula_tex,
          fit_coeffs = fit_coeffs,
          fit_var_cov_mat = fit_var_cov_mat
        )
      }

     if(!is.null(fit_results_list)){

        # Parse fitting data
        fit_coeffs <- fit_results_list[["fit_coeffs"]]
        fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
        fit_formula_tex <- fit_results_list[["fit_formula_tex"]]


        # Protracted variables ----
        if (exposure == "protracted") {
          protracted_time <- input$protracted_time
          protracted_life_time <- input$protracted_life_time
          protracted_g_value <- protracted_g_function(protracted_time, protracted_life_time)
        } else if (exposure == "protracted_high") {
          protracted_g_value <- 0
          # Used in report (dummy values)
          protracted_time <- NA
          protracted_life_time <- NA
        } else {
          protracted_g_value <- 1
          # Used in report (dummy values)
          protracted_time <- NA
          protracted_life_time <- NA
        }

        # Confidence intervals ----

        # Select CIs depending on selected method
        if (grepl("merkle", error_method, fixed = TRUE)) {
          conf_int_curve <- as.numeric(paste0("0.", gsub("\\D", "", error_method)))
          conf_int_yield <- conf_int_curve
        } else if (error_method == "delta") {
          conf_int_curve <- 0.83
          conf_int_delta <- 0.95
        }

        # Calculations ----

        # Parse genome fraction
        if (aberr_module == "translocations") {
          parsed_genome_factor <- genome_factor$genome_factor()
        } else {
          parsed_genome_factor <- 1
        }

        # Calculate whole-body results
        if (grepl("merkle", error_method, fixed = TRUE)) {
          cli::cli_alert_info("Performing whole-body dose estimation (Merkle's method)...")
          progress$set(detail = "Performing whole-body dose estimation", value = 2 / 6)

          if (input$load_case_data_check == TRUE || input$num_cases == nrow(case_data)){
            results_whole <- estimate_whole_body_merkle(
            num_cases = nrow(case_data),
            case_data,
            fit_coeffs,
            fit_var_cov_mat,
            conf_int_yield,
            conf_int_curve,
            protracted_g_value,
            parsed_genome_factor,
            aberr_module
          )
          }else{
            results_whole <-NULL
          }

        } else if (error_method == "delta") {
          cli::cli_alert_info("Performing whole-body dose estimation (delta method)...")
          progress$set(detail = "Performing whole-body dose estimation", value = 2 / 6)
          if (input$load_case_data_check == TRUE || input$num_cases == nrow(case_data)){
            results_whole <- estimate_whole_body_delta(
              num_cases = nrow(case_data),
              case_data,
              fit_coeffs,
              fit_var_cov_mat,
              conf_int_delta,
              protracted_g_value,
              aberr_module
            )
          }else{
            results_whole <-NULL
          }

        }

        # Parse results
          est_doses_whole <- list()

          for (i in seq_along(results_whole)) {
            est_doses <- results_whole[[i]][["est_doses"]]
            est_doses_whole[[i]] <- est_doses
          }

          est_doses_whole <- do.call(rbind, est_doses_whole)
          est_doses_whole <- cbind(ID = case_data$ID, est_doses_whole)

          est_yields_whole <- list()
          AIC_whole <- list()

          for (i in seq_along(results_whole)) {
            est_yields <- results_whole[[i]][["est_yield"]]
            est_yields_whole[[i]] <- est_yields
            AIC_whole[[i]] <- results_whole[[i]][["AIC"]]
          }

          est_yields_whole <- do.call(rbind, est_yields_whole)
          est_yields_whole <- cbind(ID = case_data$ID, est_yields_whole)

          AIC_whole <- do.call(rbind, AIC_whole)


        if (assessment == "partial-body") {
          #Error message if there are no C2
          if (any(case_data$C2 == "0")) {
            showModal(modalDialog(
              title = "Error",
              "Partial body calculations can not be performed in one or more cases due to lack of C2 cells (two dicentrics/cell).",
              footer = modalButton("Close")
            ))
          }

          # Input of the parameter gamma
          if (fraction_coeff == "gamma") {
            gamma <- input$gamma_coeff
          } else if (fraction_coeff == "d0") {
            gamma <- 1 / input$d0_coeff
          }

          # Calculate partial results
          cli::cli_alert_info("Performing partial-body dose estimation (Dolphin's method)...")
          progress$set(detail = "Performing partial-body dose estimation", value = 3 / 6)
          if (input$load_case_data_check == TRUE || input$num_cases == nrow(case_data)){
            results_partial <- estimate_partial_body_dolphin(
              num_cases = nrow(case_data),
              case_data,
              fit_coeffs,
              fit_var_cov_mat,
              conf_int = 0.95,
              protracted_g_value,
              parsed_genome_factor,
              gamma,
              aberr_module
            )
          }else{
            results_partial <-NULL
          }
          str(results_partial)

          # Parse results

            est_doses_partial <- list()
            est_yields_partial <- list()
            est_frac_partial <- list()
            est_metaphases_frac_partial <- list()
            AIC_partial <- list()

            for (i in seq_along(results_partial)) {
              est_doses_partial[[i]] <- results_partial[[i]][["est_doses"]]
              est_yields_partial[[i]] <- results_partial[[i]][["est_yield"]]
              est_frac_partial[[i]] <- results_partial[[i]][["est_frac"]]
              est_metaphases_frac_partial[[i]] <- results_partial[[i]][["est_metaphases_frac"]]
              AIC_partial[[i]] <- results_partial[[i]][["AIC"]]
            }

            est_doses_partial <- do.call(rbind, est_doses_partial)
            est_doses_partial <- cbind(ID = case_data$ID, est_doses_partial)
            est_yields_partial <- do.call(rbind, est_yields_partial)
            est_yields_partial <- cbind(ID = case_data$ID, est_yields_partial)
            est_frac_partial <- do.call(rbind, est_frac_partial)
            est_frac_partial <- cbind(ID = case_data$ID, est_frac_partial)
            est_metaphases_frac_partial <- do.call(rbind,  est_metaphases_frac_partial)
            est_metaphases_frac_partial <- cbind(ID = case_data$ID, est_metaphases_frac_partial)
            AIC_partial <- do.call(rbind, AIC_partial)
            AIC_partial <- cbind(ID = case_data$ID,  AIC_partial)


        } else if (assessment == "hetero") {
          #Error message if there are no C2
          if (case_data$C2 == "0") {
            showModal(modalDialog(
              title = "Error",
              "Heterogeneous calculations can not be performed due to lack of C2 cells (two dicentrics/cell).",
              footer = modalButton("Close")
            ))
          }
          # Input of the parameter gamma and its variance
          if (fraction_coeff == "gamma") {
            gamma <- input$gamma_coeff
            gamma_error <- input$gamma_error
          } else if (fraction_coeff == "d0") {
            gamma <- 1 / input$d0_coeff
            gamma_error <- 0
          }

          # Calculate heterogeneous result
          cli::cli_alert_info("Performing heterogeneous dose estimation (mixed Poisson model)...")
          progress$set(detail = "Performing heterogeneous dose estimation", value = 3 / 6)

          # Wrap mixed Poisson model in try() to ensure convergence
          for (i in 1:5) {
            try({
              results_hetero <- estimate_hetero_mixed_poisson(
                case_data,
                fit_coeffs,
                fit_var_cov_mat,
                conf_int = 0.95,
                protracted_g_value,
                gamma = gamma,
                gamma_error = gamma_error
              )
              break # break/exit the for-loop
            })
          }
          if (!exists("results_hetero")) {
            cli::cli_alert_danger("The algorithm did not converge!")
            showNotification(
              ui = "The algorithm did not converge!\nPlease try again.",
              type = "error"
            )
          }

          # Parse results
          est_mixing_prop_hetero <- results_hetero[["est_mixing_prop"]]
          est_yields_hetero <- results_hetero[["est_yields"]]
          est_doses_hetero <- results_hetero[["est_doses"]]
          est_frac_hetero <- results_hetero[["est_frac"]]
          AIC_hetero <- results_hetero[["AIC"]]
        }


        # Make plot ----
        cli::cli_alert_info("Plotting dose estimation results...")
        progress$set(detail = "Plotting dose estimation results", value = 4 / 6)

        # Data set for dose plotting
        if (assessment == "whole-body") {
          est_doses <- list(whole = results_whole)
        } else if (assessment == "partial-body") {
          est_doses <- list(whole = results_whole, partial = results_partial)
        } else if (assessment == "hetero") {
          est_doses <- list(whole = results_whole, hetero = results_hetero)
        }

        # Name of the aberration to use in the y-axis
        aberr_name <- to_title(aberr_module)
        if (aberr_module == "translocations") {
          if (nchar(input$trans_name) > 0) {
            aberr_name <- input$trans_name
          }
        }

        if (input$load_case_data_check == TRUE || input$num_cases == nrow(case_data)){

            if (input$num_cases < 2){
            # Get dose estimation curve
            gg_curve <- plot_estimated_dose_curve(
              est_doses,
              fit_coeffs,
              fit_var_cov_mat,
              protracted_g_value,
              conf_int_curve = conf_int_curve,
              aberr_name,
              "UI"
            )
            gg_curve_save <- plot_estimated_dose_curve(
              est_doses,
              fit_coeffs,
              fit_var_cov_mat,
              protracted_g_value,
              conf_int_curve = conf_int_curve,
              aberr_name,
              "save"
            )
            }else{
              if(assessment == "whole-body") {
              gg_curve <- plot_triage(
                num_cases = input$num_cases,
                est_doses_whole,
                est_doses_partial= NULL,
                assessment="whole",
                "UI")

              gg_curve_save <- plot_triage(
                num_cases = input$num_cases,
                est_doses_whole,
                est_doses_partial= NULL,
                assessment="whole",
                "save")

              }
              if(assessment == "partial-body") {
                gg_curve <- plot_triage(
                  num_cases = input$num_cases,
                  est_doses_whole,
                  est_doses_partial,
                  assessment="partial",
                  "UI")

                gg_curve_save <- plot_triage(
                  num_cases = input$num_cases,
                  est_doses_whole,
                  est_doses_partial,
                  assessment="partial",
                  "save")

              }}
        }else{
            gg_curve <- NULL
            gg_curve_save <- NULL
          }


        # Return list ----

        cli::cli_alert_info("Processing results...")
        progress$set(detail = "Processing results", value = 5 / 6)


        # Make basic list of results to return
        est_results_list <- list(
          aberr_module=aberr_module,
          # Used in app
          assessment = assessment,
          # Whole-body
          est_doses_whole = est_doses_whole,
          est_yields_whole = est_yields_whole,
          # Partial
          est_doses_partial = NA,
          est_yields_partial = NA,
          est_frac_partial = NA,
          est_metaphases_frac_partial = NA,
          # Heterogeneous
          est_mixing_prop_hetero = NA,
          est_yields_hetero = NA,
          est_doses_hetero = NA,
          est_frac_hetero = NA,
          # AICs
          AIC_whole = AIC_whole,
          AIC_partial = NA,
          AIC_hetero = NA,
          # Plot
          gg_curve = gg_curve,
          gg_curve_save = gg_curve_save,
          # Required for report
          fit_coeffs = fit_coeffs,
          protraction = c(0, 0, 0),
          fit_formula_tex = fit_formula_tex,
          case_data = case_data,
          case_description = input$case_description,
          results_comments = input$results_comments,
          IRR=NA,
          max_dose_curve=max_curve
        )

        if (assessment == "partial-body") {
          # Partial
          est_results_list[["est_doses_partial"]] <- est_doses_partial
          est_results_list[["est_yields_partial"]] <- est_yields_partial
          est_results_list[["est_frac_partial"]] <- est_frac_partial
          est_results_list[["est_metaphases_frac_partial"]] <- est_metaphases_frac_partial
          # Reset Heterogeneous
          est_results_list[["est_mixing_prop_hetero"]] <- NA
          est_results_list[["est_yields_hetero"]] <- NA
          est_results_list[["est_doses_hetero"]] <- NA
          est_results_list[["est_frac_hetero"]] <- NA
          # AICs
          est_results_list[["AIC_partial"]] <- AIC_partial
          est_results_list[["AIC_hetero"]] <- NA
        } else if (assessment == "hetero") {
          # Heterogeneous
          est_results_list[["est_mixing_prop_hetero"]] <- est_mixing_prop_hetero
          est_results_list[["est_yields_hetero"]] <- est_yields_hetero
          est_results_list[["est_doses_hetero"]] <- est_doses_hetero
          est_results_list[["est_frac_hetero"]] <- est_frac_hetero
          # Reset Partial
          est_results_list[["est_doses_partial"]] <- NA
          est_results_list[["est_frac_partial"]] <- NA
          est_results_list[["est_metaphases_frac_partial"]] <- NA
          # AICs
          est_results_list[["AIC_partial"]] <- NA
          est_results_list[["AIC_hetero"]] <- AIC_hetero
        }

        # Check if protracted correction was applied
        if (exposure == "protracted" & any(grep("beta", fit_formula_tex))) {
          est_results_list[["protraction"]] <- c(1, protracted_time, protracted_life_time)
        }

        # Additional results if using translocations
        if (aberr_module == "translocations") {
          est_results_list[["genome_factor"]] <- genome_factor$genome_factor()
          est_results_list[["chromosome_table"]] <- hot_to_r(input$chromosome_table)
          est_results_list[["trans_sex"]] <- input$trans_sex

          if (!input$trans_confounders) {
            est_results_list[["confounders"]] <- NULL
          } else if (input$trans_confounders & input$trans_confounders_type == "sigurdson") {
            est_results_list[["confounders"]] <- c(
              age_value = input$trans_confounder_age,
              sex_bool = input$trans_confounder_sex,
              smoker_bool = input$trans_confounder_smoke,
              ethnicity_value = input$trans_confounder_ethnicity,
              region_value = input$trans_confounder_region
            )
          } else if (input$trans_confounders & input$trans_confounders_type == "manual") {
            est_results_list[["confounders"]] <- input$trans_expected_aberr_value
          }
        }

        if(aberr_module == "dicentrics"){
          if(assessment == "whole-body" & exposure == "acute" & badge_check){
            est_results_list[["IRR"]] <- calculate_aberr_IRR(case_data,  fit_coeffs, badge_dose)
          }
        }

        return(est_results_list)
    }
      })


    # Results outputs ----

    # renderUI: Estimate results tabBox ----
    output$estimation_results_ui <- renderUI({
     if(!is.null(data())){
      assessment <- input$assessment_select

      # Help modal
      hetero_modal <- bsplus::bs_modal(
        id = session$ns("help_dose_mixed_yields_modal"),
        title = "Help: Heterogeneous exposures",
        size = "large",
        body = tagList(
          include_help("estimation/dose_mixed_yields.md")
        )
      )
      if(aberr_module == "dicentrics"){
        aberr_yield <- "dicentrics"
      }else if(aberr_module == "micronuclei"){
        aberr_yield <- "micronuclei"
      }else{
        aberr_yield <- "translocations"
      }

      if (assessment == "whole-body") {
        # Whole-body
        if (input$num_cases < 2){
         if(aberr_module == "dicentrics"){
          if(input$badge_dose_check & !is.na(data()[["IRR"]][1])){
            rel_chance <- ifelse(
              data()[["IRR"]][3] >= 1,
              paste0(format(data()[["IRR"]][3], scientific = TRUE, digits = 3), ":1"),
              paste0("1:", format(1 / data()[["IRR"]][3], scientific = TRUE, digits = 3))
            )
            return_tabbox <- tabBox(
              id = "estimation_results_tabs",
              width = 13,
              side = "left",
              title = help_modal_button(
                container = "tabbox",
                session$ns("help_dose_mixed_yields"),
                session$ns("help_dose_mixed_yields_modal")
              ),
              tabPanel(
                title = "Whole-body",
                h5(paste0("Whole-body exposure estimation: Yield (", aberr_yield, "/cells)")),
                div(
                  class = "hot-improved",
                  rHandsontableOutput(session$ns("est_yields_whole"))
                ),
                br(),
                h5("Whole-body exposure estimation: Dose (Gy)"),
                div(
                  class = "hot-improved",
                  rHandsontableOutput(session$ns("est_doses_whole"))
                ),
                br(),
                h5("Incidence rate ratio (IRR)"),
                div(
                  class = "hot-improved",
                  rHandsontableOutput(session$ns("IRR"))
                ),
                br(),

                h6(paste("The relative chance for observing",
                         data()[["IRR"]][2] ,"dicentrics in", data()[["IRR"]][1] ,"cells is",
                         rel_chance,
                         "for a dose of 0 Gy vs the suspected dose of", input$badge_dose ,"Gy (see odds-ratio in IAEA2011)"),
                   style = "line-height: 1.75;")

              ))

          }else{
            return_tabbox <- tabBox(
              id = "estimation_results_tabs",
              width = 13,
              side = "left",
              title = help_modal_button(
                container = "tabbox",
                session$ns("help_dose_mixed_yields"),
                session$ns("help_dose_mixed_yields_modal")
              ),
              tabPanel(
                title = "Whole-body",
                h5(paste0("Whole-body exposure estimation: Yield (", aberr_yield, "/cells)")),
                div(
                  class = "hot-improved",
                  rHandsontableOutput(session$ns("est_yields_whole"))
                ),
                br(),
                h5("Whole-body exposure estimation: Dose (Gy)"),
                div(
                  class = "hot-improved",
                  rHandsontableOutput(session$ns("est_doses_whole"))
                )
              )
            )
          }
         }else{
           return_tabbox <- tabBox(
             id = "estimation_results_tabs",
             width = 13,
             side = "left",
             title = help_modal_button(
               container = "tabbox",
               session$ns("help_dose_mixed_yields"),
               session$ns("help_dose_mixed_yields_modal")
             ),
             tabPanel(
               title = "Whole-body",
               h5(paste0("Whole-body exposure estimation: Yield (", aberr_yield, "/cells)")),
               div(
                 class = "hot-improved",
                 rHandsontableOutput(session$ns("est_yields_whole"))
               ),
               br(),
               h5("Whole-body exposure estimation: Dose (Gy)"),
               div(
                 class = "hot-improved",
                 rHandsontableOutput(session$ns("est_doses_whole"))
               )
             )
           )
           }

        }else if(input$num_cases >=2){
          return_tabbox <- tabBox(
            id = "estimation_results_tabs",
            width = 13,
            side = "left",
            title = help_modal_button(
              container = "tabbox",
              session$ns("help_dose_mixed_yields"),
              session$ns("help_dose_mixed_yields_modal")
            ),
            tabPanel(
              title = "Whole-body",
              h5(paste0("Whole-body exposure estimation: Yield (", aberr_yield, "/cells)")),
              div(
                class = "hot-improved",
                rHandsontableOutput(session$ns("est_yields_whole"))
              ),
              br(),
              h5("Whole-body exposure estimation: Dose (Gy)"),
              div(
                class = "hot-improved",
                rHandsontableOutput(session$ns("est_doses_whole"))
              )
            )
          )
        }

      } else if (assessment == "partial-body") {
        # Partial-body
        return_tabbox <- tabBox(
          id = "estimation_results_tabs",
          width = 13,
          side = "left",
          title = help_modal_button(
            container = "tabbox",
            session$ns("help_dose_mixed_yields"),
            session$ns("help_dose_mixed_yields_modal")
          ),
          tabPanel(
            title = "Whole-body",
            h5(paste0("Whole-body exposure estimation: Yield (", aberr_yield, "/cells)")),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_yields_whole"))
            ),
            br(),
            h5("Whole-body exposure estimation: Dose (Gy)"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_doses_whole"))
            )

            # br(),
            # h5("Relative quality of the estimation"),
            # div(
            #   class = "hot-improved",
            #   rHandsontableOutput(session$ns("AIC_whole"))
            # )
          ),
          tabPanel(
            title = "Partial-body",
            h5(paste0("Partial-body exposure estimation: Yield (", aberr_yield, "/cells)")),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_yields_partial"))
            ),
            br(),
            h5("Partial-body exposure estimation: Dose (Gy)"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_doses_partial"))
            ),
            br(),
            h5("Observed fraction of cells scored which were irradiated"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_metaphases_frac_partial"))
            ),
            br(),
            h5("Initial fraction of irradiated cells"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_frac_partial"))
            )

            # br(),
            # h5("Relative quality of the estimation"),
            # div(
            #   class = "hot-improved",
            #   rHandsontableOutput(session$ns("AIC_partial"))
            # )
          )
        )
      } else if (assessment == "hetero") {
        # Heterogeneous
        return_tabbox <- tabBox(
          id = "estimation_results_tabs",
          width = 13,
          side = "left",
          title = help_modal_button(
            container = "tabbox",
            session$ns("help_dose_mixed_yields"),
            session$ns("help_dose_mixed_yields_modal")
          ),
          tabPanel(
            title = "Whole-body",
            h5("Whole-body exposure estimation"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_yields_whole"))
            ),
            br(),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_doses_whole"))
            )

            # br(),
            # h5("Relative quality of the estimation"),
            # div(
            #   class = "hot-improved",
            #   rHandsontableOutput(session$ns("AIC_whole"))
            # )
          ),
          tabPanel(
            title = "Heterogeneous",
            h5("Observed fraction of irradiated cells and its yield"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_mixing_prop_hetero"))
            ),
            br(),
            h5("Heterogeneous exposure estimation"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_yields_hetero"))
            ),
            br(),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_doses_hetero"))
            ),
            br(),
            h5("Initial fraction of irradiated cells"),
            div(
              class = "hot-improved",
              rHandsontableOutput(session$ns("est_frac_hetero"))
            )

            # br(),
            # h5("Relative quality of the estimation"),
            # div(
            #   class = "hot-improved",
            #   rHandsontableOutput(session$ns("AIC_hetero"))
            # )
          )
        )
      } else {
        return(NULL)
      }

      return(tagList(return_tabbox, hetero_modal))
    }
    })

    # Estimated yield (whole-body)
    output$est_yields_whole <- renderRHandsontable({
  if (input$button_estimate <= 0) {
    return(NULL)
  }
      if(input$load_case_data_check == TRUE || input$num_cases == nrow(data()[["case_data"]])){
        data()[["est_yields_whole"]]%>%
          as.data.frame() %>%
          # Convert to hot and format table
          rhandsontable(
            width = 400,
            height = "100%",
            rowHeaderWidth = 80
          ) %>%
          hot_cols(colWidths = 80, halign = "htCenter") %>%
          hot_cols(format = "0.000")
      }else{
        NULL
      }})

    # Estimated recieved dose (whole-body)

    output$est_doses_whole <- renderRHandsontable({
      if (input$button_estimate <= 0) {
        return(NULL)
      }
      if(input$load_case_data_check == TRUE || input$num_cases == nrow(data()[["case_data"]])){
          data()[["est_doses_whole"]]%>%
            as.data.frame() %>%
          # Convert to hot and format table
          rhandsontable(
            width = 400,
            height = "100%",
            rowHeaderWidth = 80
          ) %>%
          hot_cols(colWidths = 80, halign = "htCenter") %>%
          hot_cols(format = "0.000")
      }else{
        NULL}
      })

    # Incidence ratio
    output$IRR <- renderRHandsontable({
      if (input$button_estimate <=  0 | !input$badge_dose_check) {
        return(NULL)
      }
      data()[["IRR"]] %>%
        as.data.frame() %>%
        # Convert to hot and format table
        rhandsontable(
          width = 400,
          height = "100%",
          rowHeaders = "",
          rowHeaderWidth = 80
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # Estimated yield (partial-body)
    output$est_yields_partial <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
        return(NULL)
      }
      if(input$load_case_data_check == TRUE || input$num_cases == nrow(data()[["case_data"]])){

          data()[["est_yields_partial"]] %>%
            as.data.frame() %>%
            # Fix possible NA values
            dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
            # Convert to hot and format table
            rhandsontable(
              width = 400,
              height = "100%",
              rowHeaderWidth = 80
            ) %>%
            hot_cols(colWidths = 80, halign = "htCenter") %>%
            hot_cols(format = "0.000")
        }else{
          NULL
        }})

    # Estimated recieved dose (partial-body)
    output$est_doses_partial <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
        return(NULL)
      }
      if(input$load_case_data_check == TRUE || input$num_cases == nrow(data()[["case_data"]])){

          data()[["est_doses_partial"]] %>%
            as.data.frame() %>%
            # Fix possible NA values
            dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
            # Convert to hot and format table
            rhandsontable(
              width = 400,
              height = "100%",
              rowHeaderWidth = 80
            ) %>%
            hot_cols(colWidths = 80, halign = "htCenter") %>%
            hot_cols(format = "0.000")
        }else{
          NULL
        }})


    # Estimated fraction of cells scored which were irradiated (partial-body)
    output$est_metaphases_frac_partial <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
        return(NULL)
      }
        data()[["est_metaphases_frac_partial"]] %>%
          # Fix possible NA values
          dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
          # Rename columns and rows
          `colnames<-`(c("ID","estimate", "std.err"))%>%
          # Convert to hot and format table
          rhandsontable(
            width = 400,
            height = "100%",
            rowHeaderWidth = 80
          ) %>%
          hot_cols(colWidths = 80, halign = "htCenter") %>%
          hot_cols(format = "0.000")

    })

    # Estimated fraction of irradiated blood (partial-body)
    output$est_frac_partial <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
        return(NULL)
      }
        data()[["est_frac_partial"]] %>%
          as.data.frame() %>%
          # Fix possible NA values
          dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
          # Convert to hot and format table
          rhandsontable(
            width = 400,
            height = "100%",
            rowHeaderWidth = 80
          ) %>%
          hot_cols(colWidths = 80, halign = "htCenter") %>%
          hot_cols(format = "0.000")

    })

    # Estimated fractions of irradiated cells (heterogeneous)
    output$est_mixing_prop_hetero <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
        return(NULL)
      }
      data()[["est_mixing_prop_hetero"]] %>%
        # Fix possible NA values
        dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
        # Rename columns and rows
        `colnames<-`(c("yield", "yield.err", "frac", "frac.err")) %>%
        `row.names<-`(c("dose1", "dose2")) %>%
        # Convert to hot and format table
        rhandsontable(
          width = 405,
          height = "100%",
          rowHeaderWidth = 85
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # Estimated yields (heterogeneous)
    output$est_yields_hetero <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
        return(NULL)
      }
      data()[["est_yields_hetero"]] %>%
        t() %>%
        as.data.frame() %>%
        # Fix possible NA values
        dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
        # Rename columns and rows
        `colnames<-`(c("lower", "estimate", "upper")) %>%
        `row.names<-`(c("yield1", "yield2")) %>%
        # Convert to hot and format table
        rhandsontable(
          width = 400,
          height = "100%",
          rowHeaderWidth = 85
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # Estimated recieved doses (heterogeneous)
    output$est_doses_hetero <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
        return(NULL)
      }
      data()[["est_doses_hetero"]] %>%
        t() %>%
        as.data.frame() %>%
        # Fix possible NA values
        dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
        # Rename columns and rows
        `colnames<-`(c("lower", "estimate", "upper")) %>%
        `row.names<-`(c("dose1 (Gy)", "dose2 (Gy)")) %>%
        # Convert to hot and format table
        rhandsontable(
          width = 400,
          height = "100%",
          rowHeaderWidth = 85
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # Estimated fractions of irradiated blood (heterogeneous)
    output$est_frac_hetero <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
        return(NULL)
      }
      data()[["est_frac_hetero"]] %>%
        # Fix possible NA values
        dplyr::mutate(dplyr::across(where(is.logical), as.double)) %>%
        # Rename columns and rows
        `colnames<-`(c("estimate", "std.err")) %>%
        `row.names<-`(c("dose1", "dose2")) %>%
        # Convert to hot and format table
        rhandsontable(
          width = 300,
          height = "100%",
          rowHeaderWidth = 85
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # AIC for estimated dose (whole)
    output$AIC_whole <- renderRHandsontable({
      if (input$button_estimate <= 0) {
        return(NULL)
      }
      data()[["AIC_whole"]] %>%
        matrix() %>%
        `colnames<-`(c("AIC")) %>%
        rhandsontable(
          width = 80,
          height = "100%"
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # AIC for estimated dose (partial-body)
    output$AIC_partial <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
        return(NULL)
      }
      data()[["AIC_partial"]] %>%
        matrix() %>%
        `colnames<-`(c("AIC")) %>%
        rhandsontable(
          width = 80,
          height = "100%"
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # AIC for estimated dose (heterogeneous)
    output$AIC_hetero <- renderRHandsontable({
      if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
        return(NULL)
      }
      data()[["AIC_hetero"]] %>%
        matrix() %>%
        `colnames<-`(c("AIC")) %>%
        rhandsontable(
          width = 80,
          height = "100%"
        ) %>%
        hot_cols(colWidths = 80, halign = "htCenter") %>%
        hot_cols(format = "0.000")
    })

    # Plot of the data and fitted curve
    output$plot <- renderPlot(
      res = 120,
      {
        if (input$button_estimate <= 0) {
          return(NULL)
        }

        data()[["gg_curve"]]

      }
    )


    # Export plot ----
    output$save_plot <- downloadHandler(
      filename = function() {
        paste("estimation-curve-", Sys.Date(), input$save_plot_format, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot = data()[["gg_curve_save"]], filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format)
        )
      }
    )

    #Save rds files
    output$save_data <- downloadHandler(
      filename = function() {
        paste("NAME_Estimation_results", Sys.Date(), input$save_data_format, sep = "")
      },
      content = function(file) {
        data_list <- data()
        if(input$load_fit_data_check ||aberr_module == "translocations"){
        data_list[["fit_formula_tex"]] <- NULL
        data_list[["fit_coeffs"]] <- NULL
        }
        est_results_list <- c(data_list, reactive_data_est$fitting_rds)
        est_results_list[["gg_curve"]] <- NULL
        est_results_list[["gg_curve_save"]] <- NULL
        est_results_list[["biodosetools_version"]] <- NULL
        if (aberr_module == "translocations") {
          data_list[["fit_formula_tex"]] <- NULL
          est_results_list[["genome_factor"]] <- NULL
          est_results_list[["chromosome_table"]] <- NULL
          est_results_list[["trans_sex"]] <- NULL
        }

        if(!input$load_fit_data_check){
          est_results_list[["irr_conds"]] <- NULL
          est_results_list$irr_conds <- list(
            irradiator_name = c(
              label = "Name of the irradiator used",
              text = input$irr_cond_irradiator_name
            ),
            radiation_quality = c(
              label = "Radiation quality",
              text = input$irr_cond_radiation_quality
            ),
            dose_rate = c(
              label = "Dose rate (Gy/min)",
              text = input$irr_cond_dose_rate
            ),
            dose_quantity = c(
              label = "Dose quantity",
              text = input$irr_cond_dose_quantity
            ),
            whole_blood = c(
              label = "Whole blood or isolated lymphocytes",
              text = input$irr_cond_whole_blood
            ),
            temperature = c(
              label = "Temperature (\u00B0C) during irradiation",
              text = input$irr_cond_temperature
            ),
            time = c(
              label = "Time incubations",
              text = input$irr_cond_time
            ),
            beam_characteristics = c(
              label = "Beam characteristics",
              text = input$irr_cond_beam_characteristics
            ),
            scoring_method = c(
              label = "Scoring method",
              text = input$scoring_method
            ),
            origin_curve = c(
              label = "Origin of the curve",
              text = input$origin_curve
            ),
            cal_air_water = c(
              label = "Calibration of the source",
              text = input$cal_air_water
            ),
            irrad_air_water = c(
              label = "Irradiation medium",
              text = input$irrad_air_water
            )
          )
        }

        if (input$save_data_format == ".rds") {
          est_results_list_rds <- Filter(function(x) !all(is.na(x) | x == ""), est_results_list)
          saveRDS(est_results_list_rds, file = file)
        } else {
          est_results_list_xlsx <- Filter(function(x) !all(is.na(x) | x == ""), est_results_list)
          write.xlsx(est_results_list_xlsx, file)
        }
      }
    )

    # Export report ----
    output$save_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
        paste0(aberr_module, "-estimation-report-", Sys.Date(), input$save_report_format)
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        temp_report <- file.path(tempdir(), "report.Rmd")
        local_report <- load_rmd_report(
          paste0(
            "estimation-report-",
            gsub("^\\.", "", input$save_report_format),
            ".Rmd"
          )
        )

        file.copy(local_report, temp_report, overwrite = TRUE)
        if(!input$load_fit_data_check){
          curve_data <- list(
            irradiator_name = list(
              label = "Name of the irradiator used",
              text = input$irr_cond_irradiator_name
            ),
            radiation_quality = list(
              label = "Radiation quality",
              text = input$irr_cond_radiation_quality
            ),
            dose_rate = list(
              label = "Dose rate (Gy/min)",
              text = input$irr_cond_dose_rate
            ),
            dose_quantity = list(
              label = "Dose quantity",
              text = input$irr_cond_dose_quantity
            ),
            whole_blood = list(
              label = "Whole blood or isolated lymphocytes",
              text = input$irr_cond_whole_blood
            ),
            temperature = list(
              label = "Temperature (\u00B0C) during irradiation",
              text = input$irr_cond_temperature
            ),
            time = list(
              label = "Time of incubation (h) at 37(\u00B0C) after irradiation",
              text = input$irr_cond_time
            ),
            beam_characteristics = list(
              label = "Beam characteristics",
              text = input$irr_cond_beam_characteristics
            ),
            scoring_method = list(
              label = "Scoring method",
              text = input$scoring_method
            ),
            origin_curve = list(
              label = "Origin of the curve",
              text = input$origin_curve
            ),
            cal_air_water = list(
              label = "Calibration of the source",
              text = input$cal_air_water
            ),
            irrad_air_water = list(
              label = "Irradiation medium",
              text = input$irrad_air_water
            )
          )
        }else{
             curve_data <- reactive_data_est$fitting_rds$irr_conds
           }

        # Set up parameters to pass to Rmd document
        params <- list(
          curve_data = curve_data,
          est_results_list = data(),
          aberr_module = aberr_module
        )


        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          input = temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
