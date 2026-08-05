#' Fitting Counts Hottables Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_fitting_counts_hot_server <- function(id, aberr_module) {
  moduleServer(id, function(input, output, session) {

     observeEvent(input$load_count_data_check, {
       if (input$load_count_data_check == TRUE) {
         updateAwesomeCheckbox(session, inputId = "use_aggr_count_data_check", value = FALSE)
       }
      })
     observeEvent(input$use_aggr_count_data_check, {
       if (input$use_aggr_count_data_check == TRUE) {
         updateAwesomeCheckbox(session, inputId = "load_count_data_check", value = FALSE)
       }
      })


    # Reset table ----
    table_reset <- reactiveValues(value = 0)

    observeEvent(input$button_upd_table, {
      table_reset$value <- 1
    })

    # Initialise data frame ----
    previous <- reactive({
      # Create button dependency for updating dimensions
      input$button_upd_table

      isolate({
        load_count_data <- input$load_count_data_check
        use_aggr_count_data <- input$use_aggr_count_data_check
        count_data <- input$load_count_data
        num_doses <- as.numeric(input$num_doses)
        num_aberrs <- as.numeric(input$num_aberrs) + 1
      })

      if (!load_count_data) {
        if (!use_aggr_count_data) {
          # Doses data frame
          data_doses <- data.frame(
            D = rep(0.0, num_doses)
          )

          # Base data frame
          data_base <- data.frame(
            matrix(
              0,
              nrow = num_doses,
              ncol = num_aberrs
            )
          ) %>%
            `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

          # Full data frame
          full_data <- cbind(data_doses, data_base) %>%
            dplyr::mutate(
              D = as.numeric(.data$D)
            )
        } else {
          full_data <- data.frame(
            D = rep(0.0, num_doses),
            N = rep(0, num_doses),
            X = rep(0, num_doses)
          ) %>%
            dplyr::mutate(
              D = as.numeric(.data$D),
              N = as.integer(.data$N),
              X = as.integer(.data$X)
            )
        }
      } else {
        if (!use_aggr_count_data) {
          full_data <- utils::read.csv(count_data$datapath, header = TRUE) %>%
            # Force column naming
            `colnames<-`(c("D", paste0("C", seq(0, ncol(.) - 2, 1)))) %>%
            dplyr::mutate(
              dplyr::across(
                .cols = dplyr::starts_with("C"),
                .fns = as.integer
              )
            ) %>%
            dplyr::mutate(
              D = as.numeric(.data$D)
            )
        } else {
          full_data <- utils::read.csv(count_data$datapath, header = TRUE) %>%
            # Force column naming
            `colnames<-`(c("D", "N", "X")) %>%
            dplyr::mutate(
              dplyr::across(
                .cols = c("N", "X"),
                .fns = as.integer
              )
            )
        }
      }

      return(full_data)
    })

    # Reactive data frame ----
    changed_data <- reactive({
      # Create button dependency for updating dimensions
      input$button_upd_table

      isolate({
        use_aggr_count_data <- input$use_aggr_count_data_check
      })

      # Create button dependency for updating N, X, DI, u values
      if (!use_aggr_count_data) {
        input$button_upd_params
      }

      isolate({
        if (is.null(input$count_data_hot) | isolate(table_reset$value == 1)) {
          table_reset$value <- 0
          mytable <- previous()

          # Initial rendering of the table
          if (!use_aggr_count_data) {
            # Calculated columns
            mytable <- init_aberr_table(
              data = mytable,
              type = "count",
              aberr_module
            )
          }
          return(mytable)
        } else if (!identical(previous(), input$count_data_hot)) {
          mytable <- as.data.frame(hot_to_r(input$count_data_hot))

          if (!use_aggr_count_data) {
            # Expected u-value for assessment
            assessment_u <- 1

            if (aberr_module == "micronuclei") {
              #assessment_u <- 1.17
              assessment_u <- 1
            }

            # Calculated columns
            mytable <- calculate_aberr_table(
              data = mytable,
              type = "count",
              assessment_u = assessment_u
            )
          } else {
            mytable <- mytable %>%
              dplyr::mutate(
                D = as.numeric(.data$D)
              )
          }

          return(mytable)
        }
      })
    })

    # Output ----
    output$count_data_hot <- renderRHandsontable({
      num_cols <- as.numeric(ncol(changed_data()))
      col_headers <- colnames(changed_data())
      col_headers[1] <- paste(col_headers[1], "(Gy)")

      hot <- changed_data() %>%
        rhandsontable(
          width = (80 + num_cols * 60),
          height = "100%",
          colHeaders = col_headers
        ) %>%
        hot_cols(colWidths = 50, halign = "htCenter") %>%
        hot_col(c(num_cols), format = "0", colWidths = 60) %>%
        hot_col(c(num_cols-1), format = "0", colWidths = 60) %>%
        hot_col(c(num_cols-2), format = "0.0", colWidths = 60) %>%

        hot_table(highlightCol = TRUE, highlightRow = TRUE)

      if (num_cols > 3) {
        hot <- hot %>%
          hot_col(c(2, 3, seq(num_cols - 3, num_cols, 1)), readOnly = TRUE) %>%
          hot_col(c(num_cols), format = "0.000", colWidths = 60) %>%
          hot_col(c(num_cols-1), format = "0.000", colWidths = 60) %>%
          hot_col(c(num_cols-2), format = "0.000", colWidths = 60) %>%
          hot_col(c(num_cols-3), format = "0.000", colWidths = 60) %>%
          hot_col(c(2), colWidths = 60) %>%
          hot_col(c(3), format = "0") %>%
          hot_col(num_cols, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }")
      }

      hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
      return(hot)
    })
  })
}

#' Fitting Results Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param aberr_module Aberration module.
#' @param genome_factor Genomic conversion factor used in translocations.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_fitting_results_server <- function(id, aberr_module, genome_factor = NULL) {
  moduleServer(id, function(input, output, session) {
    # Calculations ----

    # Reactive environment
    data <- reactive({
      input$button_fit

      # Initialise progress object
      cli::cli_h1("Dose-effect fitting calculations")
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Performing fitting", value = 0)

      isolate({
        count_data <- hot_to_r(input$count_data_hot)

        model_formula <- input$formula_select
        model_family <- input$family_select
      })

      if (aberr_module == "translocations") {
        req(input$frequency_select, hot_to_r(input$chromosome_table), genome_factor$genome_factor())
        frequency_select <- input$frequency_select
        chromosome_table <- hot_to_r(input$chromosome_table)
        genome_factor <- genome_factor$genome_factor()

        # Modify N for translocations using full genome frequency
        if (frequency_select == "full_gen_freq") {
          input$button_fit

          isolate({
            count_data <- count_data %>%
              dplyr::mutate(
                N = .data$N * genome_factor
              )
          })
        }
      }

      # Calculations process
      input$button_fit

      isolate({
        # Calculate dose-effect fitting curve
        cli::cli_alert_info("Performing dose-effect fitting...")
        progress$set(detail = "Performing dose-effect fitting", value = 1 / 4)
        fit_results_list <- fit(
          count_data,
          model_formula,
          model_family,
          fit_link = "identity",
          aberr_module
        )

        # Name of the aberration to use in the y-axis
        aberr_name <- to_title(aberr_module)
        if (aberr_module == "translocations") {
          if (nchar(input$trans_name) > 0) {
            aberr_name <- input$trans_name
          }
        }

        # Get dose estimation curve
        cli::cli_alert_info("Plotting fitting results...")
        progress$set(detail = "Plotting fitting results", value = 2 / 4)
        gg_curve <- plot_fit_dose_curve(
          fit_results_list,
          aberr_name,
          place = "UI"
        )
        gg_curve_save <- plot_fit_dose_curve(
          fit_results_list,
          aberr_name,
          place = "save"
        )

        # Make list of results to return
        results_list <- fit_results_list
        results_list[["fit_raw_data"]] <- hot_to_r(input$count_data_hot)
        results_list[["gg_curve"]] <- gg_curve
        results_list[["gg_curve_save"]] <- gg_curve_save

        # Additional results if using translocations
        if (aberr_module == "translocations") {
          results_list[["genome_factor"]] <- genome_factor
          results_list[["chromosome_table"]] <- chromosome_table
          results_list[["frequency_select"]] <- frequency_select
          results_list[["trans_sex"]] <- input$trans_sex
        }

        # Add irradiation conditions
        cli::cli_alert_info("Storing irradiation conditions...")
        progress$set(detail = "Storing irradiation conditions", value = 3 / 4)
        results_list[["irr_conds"]] <- list(
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

        cli::cli_alert_success("Fitting performed successfully")
        progress$set(detail = "Done", value = 1)
        showNotification(
          ui = "Fitting performed successfully"
        )

        return(results_list)
      })
    })

    # Results outputs ----
    output$fit_formula_tex <- renderUI({
      # Fitting formula
      if (input$button_fit <= 0) {
        return(NULL)
      }
      withMathJax(paste0("$$", data()[["fit_formula_tex"]], "$$"))
    })

    output$fit_model_summary <- renderUI({
      # Fitting formula
      if (input$button_fit <= 0) {
        return(NULL)
      }
      data()[["fit_model_summary"]] %>%
        gsub("<=", "\u2264", .)
    })

    output$fit_model_statistics <- renderRHandsontable({
      # Model-level statistics
      if (input$button_fit <= 0) {
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
      if (input$button_fit <= 0) {
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
      if (input$button_fit <= 0) {
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
      if (input$button_fit <= 0) {
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

    output$plot <- renderPlot(
      # Plot of the data and fitted curve
      res = 120,
      {
        if (input$button_fit <= 0) {
          return(NULL)
        }
        data()[["gg_curve"]]
      }
    )

    # Export count data ----
    output$save_count_data <- downloadHandler(
      filename = function() {
        paste("count-data-", Sys.Date(), input$save_count_data_format, sep = "")
      },
      content = function(file, verbose = TRUE) {
        if (input$save_count_data_format == ".csv") {
          utils::write.csv(hot_to_r(input$count_data_hot), file, row.names = FALSE)
        } else if (input$save_count_data_format == ".tex") {
          if(verbose){
            print(xtable::xtable(hot_to_r(input$count_data_hot)), type = "latex", file)
          }

        }
      }
    )

    # Export fit coefficients ----
    output$save_fit_data <- downloadHandler(
      filename = function() {
        paste(aberr_module, "-fitting-results-", Sys.Date(), input$save_fit_data_format, sep = "")
      },
      content = function(file) {
        if (input$save_fit_data_format == ".rds") {
          # Read results_list
          results_list <- data()

          # Add additional values to list
          results_list[["gg_curve"]] <- NULL
          results_list[["gg_curve_save"]] <- NULL
          results_list[["biodosetools_version"]] <- utils::packageVersion(pkg = "biodosetools")

          # Export RDS file
          saveRDS(results_list, file = file)
        }
      }
    )

    # Export plot ----
    output$save_plot <- downloadHandler(
      filename = function() {
        paste(aberr_module, "-fitting-curve-", Sys.Date(), input$save_plot_format, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot = data()[["gg_curve_save"]], filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format)
        )
      }
    )

    # Export report ----
    output$save_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
        paste0(aberr_module, "-fitting-report-", Sys.Date(), input$save_report_format)
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        temp_report <- file.path(tempdir(), "report.Rmd")
        local_report <- load_rmd_report(
          paste0(
            "fitting-report-",
            gsub("^\\.", "", input$save_report_format),
            ".Rmd"
          )
        )

        file.copy(local_report, temp_report, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          fit_results_list = data(),
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
