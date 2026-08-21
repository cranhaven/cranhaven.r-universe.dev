#' Missing Value Handling Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @param project_state Optional shared project-state object.
#' @param restore_state Optional reactive used while loading a project.
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @param language Optional reactive interface language.
#' @return Reactive containing the processed (imputed) dataset.
#' @import shiny
#' @importFrom naniar vis_miss mcar_test
#' @importFrom ggplot2 theme element_text
#' @importFrom stats na.omit median
#' @importFrom utils write.csv
#' @importFrom graphics image
#' @export
mod_missing_server <- function(id, data, project_state = NULL,
                               restore_state = NULL,
                               error_recorder = NULL,
                               language = NULL) {
  moduleServer(id, function(input, output, session) {

    processed_data_rv <- reactiveVal(NULL)
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }

    # A new upstream dataset resets the current imputation result.
    observeEvent(data(), {
      req(data())
      processed_data_rv(data())
    })

    if (!is.null(project_state)) {
      observe({
        project_state$missing <- list(
          method = input$imputation_method %||% "none",
          processed_data = processed_data_rv()
        )
      })
    }

    if (!is.null(restore_state)) {
      observeEvent(restore_state(), {
        if (!identical(restore_state()$stage, "missing")) return()
        saved <- restore_state()$module_state$missing %||% list()
        method <- saved$method %||% "none"
        updateSelectInput(session, "imputation_method", selected = method)
        if (is.data.frame(saved$processed_data)) {
          processed_data_rv(saved$processed_data)
        } else {
          tryCatch({
            processed_data_rv(apply_imputation_method(data(), method))
          }, error = function(e) {
            if (is.function(error_recorder)) {
              error_recorder("Missing values", "Project restoration error")
            }
            processed_data_rv(data())
            showNotification(
              local_text(
                "Missing-value settings could not be restored; the current data was kept.",
                "Kay\u0131p veri ayarlar\u0131 geri y\u00fcklenemedi; mevcut veri korundu."
              ),
              type = "warning"
            )
          })
        }
      }, ignoreNULL = TRUE)
    }

    output$missing_plot <- renderPlot({
      req(processed_data_rv())
      current_df <- processed_data_rv()

      if (requireNamespace("naniar", quietly = TRUE)) {
        plot_result <- naniar::vis_miss(current_df) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
          ggplot2::labs(
            y = local_text("Observations", "G\u00f6zlemler"),
            fill = NULL
          )
        if (identical(fafa_language(language), "tr")) {
          plot_result <- plot_result + ggplot2::scale_fill_manual(
            values = c("Missing" = "#8B0000", "Present" = "grey80"),
            labels = c("Missing" = "Kay\u0131p", "Present" = "Mevcut")
          )
        }
        plot_result
      } else {
        image(
          is.na(current_df),
          main = local_text(
            "Black areas represent missing values",
            "Siyah alanlar kay\u0131p verileri g\u00f6sterir"
          ),
          axes = FALSE
        )
      }
    })

    mcar_result_trigger <- eventReactive(input$run_mcar_button, {
      req(data())
      raw_df <- data()

      if (sum(is.na(raw_df)) == 0) {
        return(local_text(
          "No missing values found. MCAR test not applicable.",
          "Kay\u0131p veri bulunmad\u0131. MCAR testi uygulanamaz."
        ))
      }

      showNotification(
        local_text("Running Little's MCAR Test...", "Little MCAR testi \u00e7al\u0131\u015ft\u0131r\u0131l\u0131yor..."),
        type = "message"
      )

      if (requireNamespace("naniar", quietly = TRUE)) {
        tryCatch({
          res <- naniar::mcar_test(raw_df)
          p_val <- res$p.value
          interp <- if(!is.na(p_val) && p_val < 0.05) {
            local_text(
              "RESULT: p < 0.05\nInterpretation: Data is likely NOT MCAR (Systematic missingness).",
              "SONU\u00c7: p < 0,05\nYorum: Veriler b\u00fcy\u00fck olas\u0131l\u0131kla MCAR de\u011fildir (sistematik kay\u0131pl\u0131k)."
            )
          } else {
            local_text(
              "RESULT: p > 0.05\nInterpretation: Data is likely Missing Completely at Random (MCAR).",
              "SONU\u00c7: p > 0,05\nYorum: Veriler b\u00fcy\u00fck olas\u0131l\u0131kla tamamen rastgele kay\u0131pt\u0131r (MCAR)."
            )
          }
          paste0(
            local_text("Statistic: ", "\u0130statistik: "), round(res$statistic, 2),
            "\n", local_text("df: ", "sd: "), res$df,
            "\n", local_text("p-value: ", "p-de\u011feri: "),
            format.pval(p_val, eps=0.001), "\n\n", interp
          )
        }, error = function(e) {
          if (is.function(error_recorder)) error_recorder("Missing values", "MCAR test error")
          paste(
            local_text(
              "Error running MCAR test. Ensure all columns are numeric.\nDetails:",
              "MCAR testi \u00e7al\u0131\u015ft\u0131r\u0131lamad\u0131. T\u00fcm s\u00fctunlar\u0131n say\u0131sal oldu\u011fundan emin olun.\nAyr\u0131nt\u0131lar:"
            ),
            e$message
          )
        })
      } else {
        local_text("Package 'naniar' is required.", "'naniar' paketinin kurulmas\u0131 gerekir.")
      }
    })

    output$mcar_output <- renderPrint({
      req(mcar_result_trigger())
      cat(mcar_result_trigger())
    })

    output$missing_summary_table <- renderTable({
      req(processed_data_rv())
      df <- processed_data_rv()
      miss_counts <- colSums(is.na(df))
      summary_df <- data.frame(Variable = names(df), Missing_Count = miss_counts, Missing_Percentage = round((miss_counts / nrow(df)) * 100, 2))
      summary_df <- summary_df[summary_df$Missing_Count > 0, , drop = FALSE]
      if (identical(fafa_language(language), "tr")) {
        names(summary_df) <- c("De\u011fi\u015fken", "Kay\u0131p_Say\u0131s\u0131", "Kay\u0131p_Y\u00fczdesi")
      }
      summary_df
    }, rownames = FALSE, striped = TRUE)

    observeEvent(input$apply_imputation, {
      req(data())
      raw_df <- data()
      method <- input$imputation_method

      # Advanced methods can take noticeably longer on large datasets.
      if(grepl("missForest", method) || method %in% c("amelia", "mice")) {
        showNotification(
          local_text(
            "Running advanced imputation... Please wait.",
            "Geli\u015fmi\u015f veri atama i\u015flemi y\u00fcr\u00fct\u00fcl\u00fcyor... L\u00fctfen bekleyin."
          ),
          type = "message", duration = 5
        )
      }

      imputation_succeeded <- TRUE
      clean_df <- tryCatch({
        apply_imputation_method(raw_df, method)
      }, error = function(e) {
        imputation_succeeded <<- FALSE
        if (is.function(error_recorder)) error_recorder("Missing values", "Imputation error")
        showNotification(
          paste(local_text("Imputation Error:", "Veri Atama Hatas\u0131:"), e$message),
          type = "error", duration = 8
        )
        return(raw_df)
      })

      processed_data_rv(clean_df)

      if(method != "none" && imputation_succeeded) {
        showNotification(
          paste(
            local_text("Applied:", "Uygulanan y\u00f6ntem:"), method,
            local_text("| Data updated.", "| Veri g\u00fcncellendi.")
          ),
          type = "message"
        )
      }
    })

    output$download_data <- downloadHandler(
      filename = function() paste0("processed_data_", input$imputation_method, "_", Sys.Date(), ".csv"),
      content = function(file) {
        req(processed_data_rv())
        utils::write.csv(processed_data_rv(), file, row.names = FALSE)
      }
    )

    return(processed_data_rv)
  })
}
