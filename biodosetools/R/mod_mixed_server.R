#' Dicentrics Dose Estimation Mixed Fields Server Module-Case table-------------
#'
#' @param id Namespace for the {shiny} module.
#' @param aberr_module dicentrics or translocations
#' @import shiny
#'
#' @noRd
mod_mixed_case_table_hot_server <- function(id, aberr_module) {
  moduleServer(id, function(input, output, session) {

    reactive_i <- reactive({req(input$num_max_dic_cell)})

    output$case_data_only_mx <- renderRHandsontable({
      column_names <- c("ID", "N", "X")
      df_list <- list()

      for (j in seq_len(input$num_case_mx_only)) {
        df <- data.frame(
          ID = "",
          N = 0,
          X = 0,
          stringsAsFactors = FALSE
        )
        df_list[[j]] <- df
      }

      if (!is.null(input$case_data_only_mx)) {
        existing_df <- hot_to_r(input$case_data_only_mx)

        for (j in seq_len(min(nrow(existing_df), input$num_case_mx_only))) {
          update_cols <- intersect(names(df_list[[j]]), names(existing_df))
          df_list[[j]][update_cols] <- existing_df[j, update_cols, drop = FALSE]
        }
      }

      final_df <- do.call(rbind, df_list)

      rhandsontable(final_df) %>%
        hot_col(col = (2:3), format = "0") %>%
        hot_cols(halign = "htCenter")
    })


    output$case_data_hot_mx <- renderRHandsontable({
      i <- reactive_i()
      column_names <- c("ID", "N", "X", paste0("C", 0:i), "y", "y_err", "DI", "u")
      df_list <- list()

      for (j in seq_len(input$num_case_mx)) {
        df <- data.frame(
          ID = "",
          N = 0,
          X = 0,
          matrix(0, nrow = 1, ncol = i + 1, dimnames = list(NULL, paste0("C", 0:i))),
          y = 0,
          y_err = 0,
          DI = 0,
          u = 0,
          stringsAsFactors = FALSE
        )
        df_list[[j]] <- df
      }

      if (!is.null(input$case_data_hot_mx)) {
        existing_df <- hot_to_r(input$case_data_hot_mx)

        for (j in seq_len(min(nrow(existing_df), input$num_case_mx))) {
          update_cols <- intersect(names(df_list[[j]]), names(existing_df))
          df_list[[j]][update_cols] <- existing_df[j, update_cols, drop = FALSE]
        }
      }

      for (j in seq_len(input$num_case_mx)) {
        df <- df_list[[j]]
        C_columns <- df[, grepl("^C", names(df)), drop = FALSE]
        C_columns_factor <- C_columns * (0:i)

        df$N <- rowSums(C_columns, na.rm = TRUE)
        df$X <- rowSums(C_columns_factor, na.rm = TRUE)
        df$y <- df$X / df$N
        Var <- rowSums((0:i - df$y)^2 * C_columns) / (df$N - 1)
        df$y_err <- sqrt(Var / df$N)
        df$DI <-  Var / df$y
        df$u <- (df$DI - 1) * sqrt((df$N - 1) / (2 * (1 - 1 / df$X)))

        df_list[[j]] <- df
      }

      final_df <- do.call(rbind, df_list)
      num_cols <- ncol(final_df)

      rhandsontable(final_df) %>%
        hot_col(col = (i + 5):(i + 8), format = "0.000", halign = "htCenter") %>%
        hot_col(col = 1, colWidths = 70, halign = "htCenter") %>%
        hot_col(col = 2:(i + 4), format = "0", halign = "htCenter") %>%
        hot_col(c(2, 3, seq(num_cols - 3, num_cols, 1)), readOnly = TRUE, halign = "htCenter") %>%
        hot_col(col = "u", halign = "htCenter", renderer = "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        if (value > 1.96) {
          td.style.background = 'rgba(255, 0, 0, 0.2)';
        }
      }")
    })

  })}

#' Dicentrics Dose Estimation Mixed Fields Server Module------------------------
#'
#' @param id Namespace for the {shiny} module.
#' @param aberr_module dicentrics or translocations
#' @import shiny
#' @importFrom gridExtra grid.arrange
#' @noRd
mod_mixed_hot_server <- function(id, aberr_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reactive_data <- reactiveValues(
      gamma_data = NULL,
      neutrons_data = NULL,
      fit_coeffs_gamma = NULL,
      formula_gamma = NULL,
      fit_coeffs_neutrons = NULL,
      formula_neutrons = NULL,
      yield = NULL,
      est = NULL,
      plot_gamma= NULL,
      plot_neutron= NULL,
      data_table = NULL,
      cov_mat_gamma= NULL,
      cov_mat_neutrons = NULL,
      df_dose_g = NULL,
      df_yield_g = NULL,
      df_dose_n = NULL,
      df_yield_n = NULL,
      df_dose_total = NULL,
      df_yield_total =NULL,
      plot_list_gamma = list(),
      plot_list_neutron = list()

    )


    #RDS CURVES ------------------------------------------------------------------
    observeEvent(!input$curve_manual_mixed, {
      observeEvent(input$load_fit_data_gamma, {
        load_rds_data(input$load_fit_data_gamma, "gamma", reactive_data, output, session)
      })

      observeEvent(input$load_fit_data_neutrons, {
        load_rds_data(input$load_fit_data_neutrons, "neutrons", reactive_data, output, session)
      })
    })
    #Manual curves-----------------------------------------------------------------
    observeEvent(input$curve_manual_mixed, {
      load_manual_data("gamma", input, reactive_data, output, session)
      load_manual_data("neutrons", input, reactive_data, output, session)
    })

    #Dose estimation--------------------------------------------------------------
    output$correct_dose_input <- renderUI({
      if (input$option_2_check) {
        tagList(
          div(
            class = "hot-improved",
            rHandsontableOutput(ns("case_data_only_mx"))
          ),
          # Button
          br(),
          actionButton(
            ns("button_view_fit_data_mx"),
            class = "inputs-button",
            label = "Estimate dose"
          ))
      } else {
        tagList(
        # Cases table
        div(
          class = "hot-improved",
          rHandsontableOutput(ns("case_data_hot_mx"))
        ),
        # Button
        br(),
        actionButton(
          ns("button_upd_params_mx"),
          class = "inputs-button",
          label = "Estimate dose"
        ))
      }
    })

    # If case table is used
    observeEvent(input$button_upd_params_mx, {
      df <- hot_to_r(input$case_data_hot_mx)
      table <- input$case_data_hot_mx
      cells <- list()
      dics <- list()
      for( i in 1:input$num_case_mx){
        cells[[i]] <- df[i,"N"]
        dics[[i]] <- df[i,"X"]
      }

      ratio <- input$num_ratio
      num_cases <- input$num_case_mx

      if (input$curve_manual_mixed) { #If manual curve is used
#        if(input$check_neutrons_mx_manual){
#          p <- input$p_m/100
#          df[["photon%"]] <- input$p_m/100
#        }else{
          p = 0
#        }
        dose_estimation_mx(input, reactive_data, "gamma")
        dose_estimation_mx(input, reactive_data, "neutrons")


        reactive_data$est <- fun.estimate.criticality(num_cases, dics, cells,  reactive_data$fit_coeffs_gamma[, 1], reactive_data$cov_mat_gamma,
                                                      reactive_data$fit_coeffs_neutrons[, 1], reactive_data$cov_mat_neutrons, ratio, p)

        reactive_data$yield <- update_outputs(num_cases, reactive_data, output, yield_fun, manual = TRUE, table)

        # Store for report
        df$Ratio <- input$num_ratio
        reactive_data$data_table <- df

      } else { #If rds file is used
#        if(input$check_neutrons_mx_file){
#          p <- input$p_f/100
#          df[["photon%"]] <- input$p_f/100
#        }else{
          p = 0
#        }
        req(reactive_data$fit_coeffs_gamma)
        req(reactive_data$fit_coeffs_neutrons)
        reactive_data$est <- fun.estimate.criticality(num_cases, dics, cells,  reactive_data$fit_coeffs_gamma[, 1], reactive_data$cov_mat_gamma,
                                                      reactive_data$fit_coeffs_neutrons[, 1], reactive_data$cov_mat_neutrons, ratio, p)


        reactive_data$yield <- update_outputs(num_cases, reactive_data, output, yield_fun, manual= FALSE, table)

        df$Ratio <- input$num_ratio
        reactive_data$data_table <- df
      }
    })

    # If only dics and cells are used
    observeEvent(input$button_view_fit_data_mx, {

      df <- hot_to_r(input$case_data_only_mx)
      table <- input$case_data_only_mx
      cells <- list()
      dics <- list()
      for( i in 1:input$num_case_mx_only){
        cells[[i]] <- df[i,"N"]
        dics[[i]] <- df[i,"X"]
      }

      ratio <- input$num_ratio
      num_cases <- input$num_case_mx_only


      if (input$curve_manual_mixed) { #If manual curve is used
#        if(input$check_neutrons_mx_manual){
#          p <- input$p_m/100
#        }else{
          p = 0
#        }
        dose_estimation_mx(input, reactive_data, "gamma")
        dose_estimation_mx(input, reactive_data, "neutrons")

        reactive_data$est <- fun.estimate.criticality(num_cases, dics, cells,  reactive_data$fit_coeffs_gamma[, 1], reactive_data$cov_mat_gamma,
                                                      reactive_data$fit_coeffs_neutrons[, 1], reactive_data$cov_mat_neutrons, ratio, p)

        reactive_data$yield <- update_outputs(num_cases, reactive_data, output, yield_fun, manual= TRUE, table)

      } else { #If rds file is used
#        if(input$check_neutrons_mx_file){
#          p <- input$p_f/100
#        }else{
          p = 0
#        }

        req(reactive_data$fit_coeffs_gamma)
        req(reactive_data$fit_coeffs_neutrons)
        reactive_data$est <- fun.estimate.criticality(num_cases, dics, cells,  reactive_data$fit_coeffs_gamma[, 1], reactive_data$cov_mat_gamma,
                                                      reactive_data$fit_coeffs_neutrons[, 1], reactive_data$cov_mat_neutrons, ratio, p)

        reactive_data$yield <- update_outputs(num_cases, reactive_data, output, yield_fun, manual= FALSE, table)
      }

      # Update data table for report
#      if(input$check_neutrons_mx_manual){
 #       reactive_data$data_table <- data.frame(
#          Parameter = c("Ratio", "photon%","Number of cells", "Number of dicentrics"),
 #         Value = c(input$num_ratio, input$p_m, input$num_cells_mx, input$num_dics_mx))
#      }else if(input$check_neutrons_mx_file){
#        reactive_data$data_table <- data.frame(
#          Parameter = c("Ratio", "photon%","Number of cells", "Number of dicentrics"),
#          Value = c(input$num_ratio, input$p_f, input$num_cells_mx, input$num_dics_mx))
#      }else{
        reactive_data$data_table <- data.frame(
          Parameter = c("Ratio", "Number of cells", "Number of dicentrics"),
          Value = c(input$num_ratio, input$num_cells_mx, input$num_dics_mx))
#      }

    })


    #Plots------------------------------------------------------------------------

    observe({
      req(reactive_data$df_dose_g)

      output$cases_tab <- renderUI({
        selectInput(
          ns("select_case"),
          width = 165,
          label = "Select sample to plot",
          choices = reactive_data$df_dose_g[,1],
          selected = reactive_data$df_dose_g[,1][1]
        )

      })})

    dose_plot <- reactive({
      ID <- input$select_case
      return(ID)
    })

    name_num <- reactive({
      req(reactive_data$df_dose_g)
      df_num_name <- cbind(num = length(reactive_data$df_dose_g[,1]), reactive_data$df_dose_g[,1])

      for(i in 1:length(reactive_data$df_dose_g[,1])){
        if(input$select_case == reactive_data$df_dose_g[,1][i]){
          name_num <- i
        }
      }
      return(name_num)
    })

    observeEvent(input$plot_button, {
      req(dose_plot())
      manual <- input$curve_manual_mixed
      generate_plot_and_download("gamma", reactive_data, output, input, manual, dose_plot, name_num)
      generate_plot_and_download("neutrons", reactive_data, output, input, manual, dose_plot, name_num)
    })



    #Excel saves------------------------------------------------------
    output$save_results_mx <- downloadHandler(
      filename = function() {
        paste("Estimation_mx_", Sys.Date(), input$save_results_mx_format, sep = "")
      },
      content = function(file) {
        data_to_save <- list(
          "est_doses_gamma" = reactive_data$df_dose_g,
          "est_doses_neutrons" = reactive_data$df_dose_n,
          "est_doses_total" = reactive_data$df_dose_total,
          "est_yields_gamma" = reactive_data$df_yield_g,
          "est_yields_neutrons" = reactive_data$df_yield_n,
          "est_yields_total" = reactive_data$df_yield_total,
          "case_data" = reactive_data$data_table,
          "fit_coeffs_gamma" = reactive_data$fit_coeffs_gamma,
          "fit_coeffs_neutrons" = reactive_data$fit_coeffs_neutrons,
          "fit_formula_tex_gamma" = reactive_data$formula_gamma,
          "fit_formula_texa_neutrons" = reactive_data$formula_neutrons
          )

        if (input$save_results_mx_format == ".xlsx") {
          write.xlsx(data_to_save, file, rowNames = TRUE)
        }else if(input$save_results_mx_format == ".rds"){
          saveRDS(data_to_save, file)
        }else{
          stop("Unsupported file format selected.")
        } })


    #Report-----------------------------------------------------------

    output$save_report_mx <- downloadHandler(
      filename = function() {
        paste0(aberr_module, "-estimation-report-mx", Sys.Date(), input$save_report_format)
      },
      content = function(file) {
        plot_report_gamma <- tempfile(fileext = ".png")
        if (length(reactive_data$plot_list_gamma()) == 1) {
          combined_plot_gamma <- reactive_data$plot_list_gamma()[[1]]
        } else {
          combined_plot_gamma <- do.call(gridExtra::grid.arrange, c(reactive_data$plot_list_gamma(), ncol = 2))
        }
        ggplot2::ggsave(
          filename = plot_report_gamma,
          plot = combined_plot_gamma,
          width = 10, height = 6, dpi = 96
        )

        plot_report_neutron <- tempfile(fileext = ".png")
        if (length(reactive_data$plot_list_neutron()) == 1) {
          combined_plot_neutron <- reactive_data$plot_list_neutron()[[1]]
        } else {
          combined_plot_neutron <- do.call(gridExtra::grid.arrange, c(reactive_data$plot_list_neutron(), ncol = 2))
        }
        ggplot2::ggsave(
          filename = plot_report_neutron,
          plot = combined_plot_neutron,
          width = 10, height = 6, dpi = 96
        )
        local_report <- load_rmd_report(
          paste0(
            "mx-report-",
            gsub("^\\.", "", input$save_report_format),
            ".Rmd"
          )
        )

        temp_report <- file.path(tempdir(), "report.Rmd")
        file.copy(local_report, temp_report, overwrite = TRUE)

        # To pass to Rmd document
        params <- list(
          aberr_module = aberr_module,
          comments = input$results_comments,
          case_description = input$case_description_mx,
          table_dose_g = reactive_data$df_dose_g,
          table_dose_n = reactive_data$df_dose_n,
          table_dose_t = reactive_data$df_dose_total,
          table_yield_g = reactive_data$df_yield_g,
          table_yield_n = reactive_data$df_yield_n,
          table_yield_t = reactive_data$df_yield_total,
          data_input = reactive_data$data_table,
          curve_input_gamma = reactive_data$fit_coeffs_gamma,
          curve_input_neutrons = reactive_data$fit_coeffs_neutrons,
          curve_formula_gamma = reactive_data$formula_gamma,
          curve_formula_neutrons = reactive_data$formula_neutrons,
          plot_gamma = plot_report_gamma,
          plot_neutron = plot_report_neutron)

        rmarkdown::render(
          input = temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      })
  })}
