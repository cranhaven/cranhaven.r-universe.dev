#' Exploratory Graph Analysis (EGA) Server Module Logic
#'
#' Handles the server-side logic for performing EGA, including input validation,
#' running the EGAnet::EGA function, rendering results (network matrix, plot,
#' dimensionality summary, item-community assignments), and providing download options.
#'
#' @param id Module namespace ID.
#' @param data A reactive expression returning the current dataset.
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @param language Optional reactive interface language.
#'
#' @import shiny
#' @importFrom EGAnet EGA
#' @importFrom utils write.csv
#' @importFrom grDevices svg dev.off
#' @importFrom graphics plot text
#' @importFrom stats var na.omit
#' @references Christensen, A. P., & Golino, H. (2021). Estimating the
#'   stability of the number of factors via Bootstrap Exploratory Graph
#'   Analysis: A tutorial. *Psych, 3*(3), 479-500.
#' @noRd
ega_server <- function(id, data, error_recorder = NULL, language = NULL) {
  moduleServer(id, function(input, output, session) {

    # Helper for safe access
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }
    round_numeric_columns <- function(value, digits = 3L) {
      value <- as.data.frame(value, check.names = FALSE)
      numeric_columns <- vapply(value, is.numeric, logical(1))
      value[numeric_columns] <- lapply(value[numeric_columns], round, digits = digits)
      value
    }

    # reactiveValues to store EGA results
    ega_analysis_results_rv <- reactiveValues(
      ega_object = NULL,
      network_matrix = NULL,
      dimensionality_summary = NULL,
      item_community_assignments = NULL,
      bootega_object = NULL,
      bootega_stability = NULL,
      bootega_summary_table = NULL,
      bootega_frequency_table = NULL,
      bootega_dimension_table = NULL,
      bootega_item_table = NULL,
      bootega_item_plot = NULL,
      bootega_correlation = NULL,
      bootega_used_fallback = FALSE,
      bootega_status = NULL
    )
    ega_cache <- new_session_cache()
    bootega_cache <- new_session_cache()

    observeEvent(data(), {
      clear_session_cache(ega_cache)
      ega_analysis_results_rv$ega_object <- NULL
      ega_analysis_results_rv$network_matrix <- NULL
      ega_analysis_results_rv$dimensionality_summary <- NULL
      ega_analysis_results_rv$item_community_assignments <- NULL
      ega_analysis_results_rv$bootega_object <- NULL
      ega_analysis_results_rv$bootega_stability <- NULL
      ega_analysis_results_rv$bootega_summary_table <- NULL
      ega_analysis_results_rv$bootega_frequency_table <- NULL
      ega_analysis_results_rv$bootega_dimension_table <- NULL
      ega_analysis_results_rv$bootega_item_table <- NULL
      ega_analysis_results_rv$bootega_item_plot <- NULL
      ega_analysis_results_rv$bootega_correlation <- NULL
      ega_analysis_results_rv$bootega_used_fallback <- FALSE
      ega_analysis_results_rv$bootega_status <- NULL
      clear_session_cache(bootega_cache)
    }, ignoreNULL = TRUE)

    observeEvent(input$run_ega_button, {
      # --- Input Validations ---
      validate(
        need(data(), "Please upload your dataset to run EGA."),
        need(input$ega_estimation_method_select, "Please select an EGA estimation method."),
        need(input$ega_correlation_type_radio, "Please select a correlation type for EGA.")
      )

      current_data <- data()

      validate(
        need(all(sapply(current_data, is.numeric)), "All columns in the dataset must be numeric for EGA."),
        need(ncol(current_data) > 1, "Dataset must have at least two variables for EGA."),
        need(nrow(current_data) > ncol(current_data), "Sample size should ideally be greater than the number of variables for stable network estimation.")
      )

      cache_key <- session_cache_key(
        input$ega_estimation_method_select,
        input$ega_correlation_type_radio,
        input$ega_algorithm_select %||% "walktrap"
      )
      cached_result <- session_cache_get(ega_cache, cache_key)
      if (!is.null(cached_result)) {
        ega_analysis_results_rv$ega_object <- cached_result$ega_object
        ega_analysis_results_rv$network_matrix <- cached_result$network_matrix
        ega_analysis_results_rv$dimensionality_summary <- cached_result$dimensionality_summary
        ega_analysis_results_rv$item_community_assignments <- cached_result$item_community_assignments
        showNotification("Saved session result was used.", type = "message")
        return()
      }

      col_variances <- apply(current_data, 2, var, na.rm = TRUE)
      validate(
        need(all(col_variances > 1e-6), "One or more variables have zero or near-zero variance. Please remove them or check your data.")
      )

      progress_id <- showNotification("Running Exploratory Graph Analysis...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      # --- Perform EGA Analysis ---
      tryCatch({
        correlation_method_for_ega <- input$ega_correlation_type_radio

        ega_output <- EGAnet::EGA(
          data      = current_data,
          model     = input$ega_estimation_method_select,
          algorithm = input$ega_algorithm_select %||% "walktrap",
          corr      = correlation_method_for_ega,
          plot.EGA = TRUE,
          plot.type = "qgraph",
          plot.args = list(
            vsize = 7,
            label.cex = 1,
            edge.width = 1.5,
            layout = "spring",
            theme = "TeamFortress",
            legend.cex = 0.7,
            GLratio = 1.5
          ),
          verbose = FALSE
        )

        ega_analysis_results_rv$ega_object <- ega_output

        if (!is.null(ega_output$network)) {
          ega_analysis_results_rv$network_matrix <- as.data.frame(as.matrix(ega_output$network))
        } else {
          ega_analysis_results_rv$network_matrix <- data.frame(Message = "Network matrix not available from EGA output.")
        }

        dim_summary_text <- paste0(
          "Number of Dimensions (Communities) Identified: ", ega_output$n.dim %||% "N/A", "\n\n",
          "Item to Community Assignments:\n"
        )
        ega_analysis_results_rv$dimensionality_summary <- dim_summary_text

        if(!is.null(ega_output$wc)){
          item_comm_df <- data.frame(
            Item = names(ega_output$wc),
            Community = ega_output$wc,
            stringsAsFactors = FALSE
          )
          ega_analysis_results_rv$item_community_assignments <- item_comm_df
        } else {
          ega_analysis_results_rv$item_community_assignments <- data.frame(Message="Community assignments (wc) not available.")
        }

        session_cache_set(ega_cache, cache_key, list(
          ega_object = ega_analysis_results_rv$ega_object,
          network_matrix = ega_analysis_results_rv$network_matrix,
          dimensionality_summary = ega_analysis_results_rv$dimensionality_summary,
          item_community_assignments = ega_analysis_results_rv$item_community_assignments
        ))

        # --- Render Outputs ---
        output$ega_network_table_output <- renderTable({
          validate(need(!is.null(ega_analysis_results_rv$network_matrix) && nrow(ega_analysis_results_rv$network_matrix) > 0 && !("Message" %in% colnames(ega_analysis_results_rv$network_matrix)),
                        "Network matrix is not available or empty."))
          round(ega_analysis_results_rv$network_matrix, 3)
        }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)

        output$ega_network_plot_output <- renderPlot({
          validate(need(!is.null(ega_analysis_results_rv$ega_object), "EGA result object is not available for plotting."))
          plot(ega_analysis_results_rv$ega_object)
        }, width = 750, height = 550)

        output$ega_dimensionality_summary_output <- renderPrint({
          req(ega_analysis_results_rv$dimensionality_summary)
          cat(ega_analysis_results_rv$dimensionality_summary)
        })

        output$ega_item_community_table_output <- renderTable({
          req(ega_analysis_results_rv$item_community_assignments)
          validate(need(!("Message" %in% colnames(ega_analysis_results_rv$item_community_assignments)), ""))
          ega_analysis_results_rv$item_community_assignments
        }, striped = TRUE, hover = TRUE, bordered = TRUE)

        showNotification("EGA analysis completed successfully!", type = "message", duration = 4)

      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("EGA", "Analysis error")
        user_error_message <- if (!is.null(conditionMessage(e))) conditionMessage(e) else "An unspecified error occurred."
        showNotification(paste("Error (EGA):", user_error_message), type = "error", duration = 10)

        ega_analysis_results_rv$ega_object <- NULL
        ega_analysis_results_rv$network_matrix <- data.frame(Error = paste("Analysis failed:", user_error_message))
        ega_analysis_results_rv$dimensionality_summary <- paste("Analysis failed:", user_error_message)
        ega_analysis_results_rv$item_community_assignments <- data.frame(Error = paste("Analysis failed:", user_error_message))

        output$ega_network_table_output <- renderTable({ ega_analysis_results_rv$network_matrix })
        output$ega_network_plot_output <- renderPlot({ plot(NULL, xlim=c(0,1),ylim=c(0,1),main="Plot Error"); text(0.5,0.5,user_error_message)})
        output$ega_dimensionality_summary_output <- renderPrint({ cat(ega_analysis_results_rv$dimensionality_summary) })
        output$ega_item_community_table_output <- renderTable({ ega_analysis_results_rv$item_community_assignments })
      })
    })

    observeEvent(input$run_bootega_button, {
      analysis_stage <- "input validation"
      validate(
        need(data(), "Please upload your dataset to run Bootstrap EGA."),
        need(input$ega_estimation_method_select, "Please select an EGA estimation method."),
        need(input$ega_correlation_type_radio, "Please select a correlation type for EGA.")
      )

      current_data <- data()
      validate(
        need(all(vapply(current_data, is.numeric, logical(1))), "All columns must be numeric for Bootstrap EGA."),
        need(ncol(current_data) > 1L, "Bootstrap EGA requires at least two variables."),
        need(nrow(current_data) > ncol(current_data), "The sample size should be greater than the number of variables.")
      )

      analysis_data <- prepare_bootega_data(current_data)
      col_variances <- vapply(analysis_data, stats::var, numeric(1), na.rm = TRUE)
      validate(
        need(all(is.finite(col_variances) & col_variances > 1e-6), "Remove variables with zero or near-zero variance before Bootstrap EGA.")
      )

      tryCatch({
        analysis_stage <- "bootstrap settings"
        settings <- validate_bootega_settings(
          input$bootega_iterations,
          input$bootega_cores,
          input$bootega_seed
        )
        bootstrap_type <- input$bootega_type %||% "parametric"
        validate(
          need(bootstrap_type %in% c("parametric", "resampling"), "Select a valid Bootstrap EGA type.")
        )

        cache_key <- session_cache_key(
          input$ega_estimation_method_select,
          input$ega_correlation_type_radio,
          input$ega_algorithm_select %||% "walktrap",
          bootstrap_type,
          settings$iter,
          settings$ncores,
          settings$seed,
          isTRUE(input$bootega_typical_structure)
        )
        cached_result <- session_cache_get(bootega_cache, cache_key)
        if (!is.null(cached_result)) {
          ega_analysis_results_rv$bootega_object <- cached_result$bootega_object
          ega_analysis_results_rv$bootega_stability <- cached_result$bootega_stability
          ega_analysis_results_rv$bootega_summary_table <- cached_result$bootega_summary_table
          ega_analysis_results_rv$bootega_frequency_table <- cached_result$bootega_frequency_table
          ega_analysis_results_rv$bootega_dimension_table <- cached_result$bootega_dimension_table
          ega_analysis_results_rv$bootega_item_table <- cached_result$bootega_item_table
          ega_analysis_results_rv$bootega_item_plot <- cached_result$bootega_item_plot
          ega_analysis_results_rv$bootega_correlation <- cached_result$bootega_correlation
          ega_analysis_results_rv$bootega_used_fallback <- isTRUE(cached_result$bootega_used_fallback)
          saved_status <- local_text(
            "Saved Bootstrap EGA result was used.",
            "Kaydedilmi\u015f Bootstrap EGA sonucu kullan\u0131ld\u0131."
          )
          if (ega_analysis_results_rv$bootega_used_fallback) {
            saved_status <- paste0(
              saved_status,
              " ",
              local_text(
                "The automatic ordinal correlation could not be estimated; Pearson correlation was used.",
                "Otomatik ordinal korelasyon matrisi hesaplanamad\u0131\u011f\u0131 i\u00e7in Pearson korelasyonu kullan\u0131ld\u0131."
              )
            )
          }
          ega_analysis_results_rv$bootega_status <- saved_status
          showNotification(ega_analysis_results_rv$bootega_status, type = "message")
          return()
        }

        ega_analysis_results_rv$bootega_status <- paste0(
          local_text(
            "Bootstrap Exploratory Graph Analysis is running. Bootstrap samples: ",
            "Bootstrap Ke\u015ffedici Grafik Analizi \u00e7al\u0131\u015f\u0131yor. Bootstrap \u00f6rneklemleri: "
          ),
          settings$iter
        )
        progress_id <- showNotification(
          ega_analysis_results_rv$bootega_status,
          duration = NULL,
          type = "message"
        )
        on.exit(removeNotification(progress_id), add = TRUE)

        analysis_stage <- "EGAnet::bootEGA"
        bootega_run <- run_bootega_with_correlation_fallback(
          data = analysis_data,
          corr = input$ega_correlation_type_radio,
          model = input$ega_estimation_method_select,
          algorithm = input$ega_algorithm_select %||% "walktrap",
          iter = settings$iter,
          type = bootstrap_type,
          ncores = settings$ncores,
          EGA.type = "EGA",
          plot.itemStability = FALSE,
          typicalStructure = isTRUE(input$bootega_typical_structure),
          plot.typicalStructure = FALSE,
          seed = settings$seed,
          verbose = FALSE
        )
        bootega_output <- bootega_run$result
        ega_analysis_results_rv$bootega_correlation <- bootega_run$correlation
        ega_analysis_results_rv$bootega_used_fallback <- isTRUE(bootega_run$used_fallback)
        analysis_stage <- "EGAnet::dimensionStability"
        stability_output <- EGAnet::dimensionStability(
          bootega_output,
          IS.plot = FALSE
        )
        analysis_stage <- "result preparation"
        prepared <- prepare_bootega_results(bootega_output, stability_output)

        ega_analysis_results_rv$bootega_object <- bootega_output
        ega_analysis_results_rv$bootega_stability <- stability_output
        ega_analysis_results_rv$bootega_summary_table <- prepared$summary_table
        ega_analysis_results_rv$bootega_frequency_table <- prepared$frequency_table
        ega_analysis_results_rv$bootega_dimension_table <- prepared$dimension_table
        ega_analysis_results_rv$bootega_item_table <- prepared$item_table
        ega_analysis_results_rv$bootega_item_plot <- prepared$item_plot
        ega_analysis_results_rv$bootega_status <- paste0(
          local_text(
            "Bootstrap EGA completed successfully. Bootstrap samples: ",
            "Bootstrap EGA ba\u015far\u0131yla tamamland\u0131. Bootstrap \u00f6rneklemleri: "
          ),
          bootega_output$iter %||% settings$iter,
          "; ",
          local_text("type: ", "t\u00fcr: "),
          bootega_output$type %||% bootstrap_type,
          "."
        )
        if (ega_analysis_results_rv$bootega_used_fallback) {
          ega_analysis_results_rv$bootega_status <- paste0(
            ega_analysis_results_rv$bootega_status,
            " ",
            local_text(
              "The automatic ordinal correlation could not be estimated; Pearson correlation was used.",
              "Otomatik ordinal korelasyon matrisi hesaplanamad\u0131\u011f\u0131 i\u00e7in Pearson korelasyonu kullan\u0131ld\u0131."
            )
          )
        }

        session_cache_set(bootega_cache, cache_key, list(
          bootega_object = ega_analysis_results_rv$bootega_object,
          bootega_stability = ega_analysis_results_rv$bootega_stability,
          bootega_summary_table = ega_analysis_results_rv$bootega_summary_table,
          bootega_frequency_table = ega_analysis_results_rv$bootega_frequency_table,
          bootega_dimension_table = ega_analysis_results_rv$bootega_dimension_table,
          bootega_item_table = ega_analysis_results_rv$bootega_item_table,
          bootega_item_plot = ega_analysis_results_rv$bootega_item_plot,
          bootega_correlation = ega_analysis_results_rv$bootega_correlation,
          bootega_used_fallback = ega_analysis_results_rv$bootega_used_fallback
        ))

        showNotification(
          local_text(
            "Bootstrap Exploratory Graph Analysis completed.",
            "Bootstrap Ke\u015ffedici Grafik Analizi tamamland\u0131."
          ),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Bootstrap EGA", "Analysis error")
        ega_analysis_results_rv$bootega_status <- paste0(
          local_text("Bootstrap EGA error: ", "Bootstrap EGA hatas\u0131: "),
          conditionMessage(e),
          local_text(" [stage: ", " [a\u015fama: "),
          analysis_stage,
          "]"
        )
        showNotification(
          ega_analysis_results_rv$bootega_status,
          type = "error",
          duration = 12
        )
      })
    })

    output$bootega_status <- renderText({
      ega_analysis_results_rv$bootega_status %||% local_text(
        "Set the bootstrap options and run Bootstrap Exploratory Graph Analysis.",
        "Bootstrap ayarlar\u0131n\u0131 belirleyip Bootstrap Ke\u015ffedici Grafik Analizini \u00e7al\u0131\u015ft\u0131r\u0131n."
      )
    })

    output$bootega_summary_table <- renderTable({
      req(ega_analysis_results_rv$bootega_summary_table)
      round_numeric_columns(ega_analysis_results_rv$bootega_summary_table)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$bootega_frequency_table <- renderTable({
      req(ega_analysis_results_rv$bootega_frequency_table)
      round_numeric_columns(ega_analysis_results_rv$bootega_frequency_table)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$bootega_dimension_stability_table <- renderTable({
      req(ega_analysis_results_rv$bootega_dimension_table)
      round_numeric_columns(ega_analysis_results_rv$bootega_dimension_table)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$bootega_item_stability_table <- renderTable({
      req(ega_analysis_results_rv$bootega_item_table)
      round_numeric_columns(ega_analysis_results_rv$bootega_item_table)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$bootega_item_stability_plot <- renderPlot({
      req(ega_analysis_results_rv$bootega_item_plot)
      print(ega_analysis_results_rv$bootega_item_plot)
    }, width = 850, height = 550)

    output$download_bootega_summary <- downloadHandler(
      filename = function() paste0("bootEGA_summary_", Sys.Date(), ".csv"),
      content = function(file) {
        req(ega_analysis_results_rv$bootega_summary_table)
        utils::write.csv(ega_analysis_results_rv$bootega_summary_table, file, row.names = FALSE)
      }
    )

    output$download_bootega_frequency <- downloadHandler(
      filename = function() paste0("bootEGA_dimension_frequency_", Sys.Date(), ".csv"),
      content = function(file) {
        req(ega_analysis_results_rv$bootega_frequency_table)
        utils::write.csv(ega_analysis_results_rv$bootega_frequency_table, file, row.names = FALSE)
      }
    )

    output$download_bootega_dimension_stability <- downloadHandler(
      filename = function() paste0("bootEGA_dimension_stability_", Sys.Date(), ".csv"),
      content = function(file) {
        req(ega_analysis_results_rv$bootega_dimension_table)
        utils::write.csv(ega_analysis_results_rv$bootega_dimension_table, file, row.names = FALSE)
      }
    )

    output$download_bootega_item_stability <- downloadHandler(
      filename = function() paste0("bootEGA_item_stability_", Sys.Date(), ".csv"),
      content = function(file) {
        req(ega_analysis_results_rv$bootega_item_table)
        utils::write.csv(ega_analysis_results_rv$bootega_item_table, file, row.names = FALSE)
      }
    )

    output$download_bootega_item_plot <- downloadHandler(
      filename = function() {
        paste0(
          "bootEGA_item_stability_",
          Sys.Date(),
          ".",
          input$bootega_plot_download_format %||% "png"
        )
      },
      content = function(file) {
        req(ega_analysis_results_rv$bootega_item_plot)
        format <- input$bootega_plot_download_format %||% "png"
        if (format == "svg") {
          grDevices::svg(file, width = 10, height = 7.5)
        } else if (format == "jpg") {
          grDevices::jpeg(file, width = 10, height = 7.5, units = "in", res = 300, quality = 95)
        } else {
          grDevices::png(file, width = 10, height = 7.5, units = "in", res = 300)
        }
        on.exit(grDevices::dev.off(), add = TRUE)
        print(ega_analysis_results_rv$bootega_item_plot)
      }
    )

    output$download_bootega_apa7 <- downloadHandler(
      filename = function() paste0("bootEGA_APA7_report_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        req(
          ega_analysis_results_rv$bootega_summary_table,
          ega_analysis_results_rv$bootega_frequency_table,
          ega_analysis_results_rv$bootega_dimension_table,
          ega_analysis_results_rv$bootega_item_table
        )
        report_language <- fafa_language(language)
        correlation_text <- if (identical(
          ega_analysis_results_rv$bootega_correlation,
          "pearson"
        )) {
          fafa_text(report_language, "Pearson correlation", "Pearson korelasyonu")
        } else {
          fafa_text(
            report_language,
            "Automatic correlation selection",
            "Otomatik korelasyon se\u00e7imi"
          )
        }
        if (ega_analysis_results_rv$bootega_used_fallback) {
          correlation_text <- paste0(
            correlation_text,
            fafa_text(
              report_language,
              " (used because the automatic ordinal correlation matrix was not positive-definite)",
              " (otomatik ordinal korelasyon matrisi pozitif tan\u0131ml\u0131 olmad\u0131\u011f\u0131 i\u00e7in kullan\u0131ld\u0131)"
            )
          )
        }
        write_apa7_report(
          file,
          title = fafa_text(
            report_language,
            "Bootstrap Exploratory Graph Analysis Report",
            "Bootstrap Ke\u015ffedici Grafik Analizi Raporu"
          ),
          subtitle = paste0("FAfA ", fafa_package_version(), " - APA 7"),
          sections = list(
            list(
              title = fafa_text(report_language, "Analysis Settings", "Analiz Ayarlar\u0131"),
              text = paste0(
                fafa_text(report_language, "Correlation method: ", "Korelasyon y\u00f6ntemi: "),
                correlation_text
              )
            ),
            list(
              title = fafa_text(report_language, "Bootstrap Summary", "Bootstrap \u00d6zeti"),
              table = ega_analysis_results_rv$bootega_summary_table,
              note = fafa_text(
                report_language,
                "The table summarizes the bootstrap distribution of the estimated number of dimensions.",
                "Tablo, tahmin edilen boyut say\u0131s\u0131n\u0131n bootstrap da\u011f\u0131l\u0131m\u0131n\u0131 \u00f6zetlemektedir."
              )
            ),
            list(
              title = fafa_text(report_language, "Dimension Frequency", "Boyut S\u0131kl\u0131\u011f\u0131"),
              table = ega_analysis_results_rv$bootega_frequency_table
            ),
            list(
              title = fafa_text(report_language, "Dimension Stability", "Boyut Kararl\u0131l\u0131\u011f\u0131"),
              table = ega_analysis_results_rv$bootega_dimension_table,
              note = fafa_text(
                report_language,
                "Structural consistency is the proportion of bootstrap samples in which an empirical dimension is exactly replicated.",
                "Yap\u0131sal tutarl\u0131l\u0131k, ampirik bir boyutun tam olarak tekrarland\u0131\u011f\u0131 bootstrap \u00f6rneklemlerinin oran\u0131d\u0131r."
              )
            ),
            list(
              title = fafa_text(report_language, "Item Stability", "Madde Kararl\u0131l\u0131\u011f\u0131"),
              table = ega_analysis_results_rv$bootega_item_table,
              note = fafa_text(
                report_language,
                "Item stability is the proportion of bootstrap samples in which an item is assigned to its empirical dimension.",
                "Madde kararl\u0131l\u0131\u011f\u0131, bir maddenin ampirik boyutuna atand\u0131\u011f\u0131 bootstrap \u00f6rneklemlerinin oran\u0131d\u0131r."
              )
            ),
            list(
              title = fafa_text(report_language, "Reference", "Kaynak"),
              text = paste(
                "Christensen, A. P., & Golino, H. (2021).",
                "Estimating the stability of psychological dimensions via Bootstrap Exploratory Graph Analysis:",
                "A Monte Carlo simulation and tutorial. Psych, 3(3), 479-500."
              )
            )
          ),
          language = report_language
        )
      }
    )

    output$download_ega_network_button <- downloadHandler(
      filename = function() {
        paste0("ega_network_matrix_", input$ega_estimation_method_select, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        validate(need(!is.null(ega_analysis_results_rv$network_matrix) && !("Error" %in% colnames(ega_analysis_results_rv$network_matrix)) && nrow(ega_analysis_results_rv$network_matrix) > 0,
                      "Network matrix is not available for download or contains an error."))
        write.csv(ega_analysis_results_rv$network_matrix, file, row.names = TRUE)
      }
    )
    output$download_ega_apa7 <- downloadHandler(
      filename = function() paste0("ega_APA7_report_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        req(
          ega_analysis_results_rv$ega_object,
          ega_analysis_results_rv$network_matrix,
          ega_analysis_results_rv$item_community_assignments
        )
        report_language <- fafa_language(language)
        dimension_count <- ega_analysis_results_rv$ega_object$n.dim %||% NA
        write_apa7_report(
          file,
          title = fafa_text(report_language, "Exploratory Graph Analysis Report", "Ke\u015ffedici Grafik Analizi Raporu"),
          subtitle = paste0("FAfA ", fafa_package_version(), " - APA 7"),
          sections = list(
            list(
              title = fafa_text(report_language, "Dimensionality", "Boyutluluk"),
              text = paste0(
                fafa_text(report_language, "The number of identified communities was ", "Belirlenen topluluk say\u0131s\u0131 "),
                dimension_count,
                "."
              ),
              table = ega_analysis_results_rv$item_community_assignments
            ),
            list(
              title = fafa_text(report_language, "Network Matrix", "A\u011f Matrisi"),
              table = ega_analysis_results_rv$network_matrix,
              note = fafa_text(
                report_language,
                "The table contains the estimated network edge weights.",
                "Tablo tahmin edilen a\u011f kenar a\u011f\u0131rl\u0131klar\u0131n\u0131 i\u00e7ermektedir."
              )
            )
          ),
          language = report_language
        )
      }
    )

    output$download_ega_plot_button <- downloadHandler(
      filename = function() {
        paste0("ega_network_plot_", input$ega_estimation_method_select, "_", Sys.Date(), ".",
               input$ega_plot_download_format %||% "png")
      },
      content = function(file) {
        validate(need(!is.null(ega_analysis_results_rv$ega_object), "EGA results are not available for plot download."))
        format <- input$ega_plot_download_format %||% "png"
        if (format == "svg") {
          grDevices::svg(file, width = 10, height = 7.5)
        } else if (format == "jpg") {
          grDevices::jpeg(file, width = 10, height = 7.5, units = "in", res = 300, quality = 95)
        } else {
          grDevices::png(file, width = 10, height = 7.5, units = "in", res = 300)
        }
        on.exit(grDevices::dev.off(), add = TRUE)
        plot(ega_analysis_results_rv$ega_object)
      }
    )
  })
}
