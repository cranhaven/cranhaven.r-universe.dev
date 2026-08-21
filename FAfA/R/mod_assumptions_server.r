#' Assumptions Server Logic
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @param language Optional reactive interface language.
#' @export
assumptions_server <- function(id, data, error_recorder = NULL,
                               language = NULL) {
  moduleServer(id, function(input, output, session) {
    results_rv <- reactiveValues(desc = NULL, multi = NULL, norm = NULL)
    assumptions_cache <- reactiveVal(NULL)

    observeEvent(data(), {
      assumptions_cache(NULL)
      results_rv$desc <- NULL
      results_rv$multi <- NULL
      results_rv$norm <- NULL
    }, ignoreNULL = TRUE)

    get_assumptions <- function() {
      cached <- assumptions_cache()
      if (!is.null(cached)) return(cached)
      calculated <- assumptions(data())
      assumptions_cache(calculated)
      calculated
    }

    observeEvent(input$run_descriptives_button, {
      req(data())
      tryCatch({
        results_rv$desc <- get_assumptions()$descriptives
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Assumptions", "Descriptives error")
        showNotification(conditionMessage(e), type = "error")
      })
    })

    observeEvent(input$run_collinearity_button, {
      req(data())
      tryCatch({
        results_rv$multi <- get_assumptions()$multicollinearity
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Assumptions", "Collinearity error")
        showNotification(conditionMessage(e), type = "error")
      })
    })

    observeEvent(input$run_normality_tests_button, {
      req(data())
      progress_id <- showNotification("Running normality tests...",
                                      duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        res            <- get_assumptions()
        norm_df        <- res$mvn_table

        # Format p-values for display
        norm_df[["p-value"]] <- sapply(norm_df[["p-value"]], function(p) {
          if (is.na(p)) return(NA_character_)
          if (p < 0.001) "< .001" else as.character(round(p, 3))
        })

        results_rv$norm <- norm_df
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Assumptions", "Normality error")
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
      })
    })

    output$descriptives_table_output          <- renderTable({ results_rv$desc  }, rownames = TRUE)
    output$collinearity_table_output          <- renderTable({ results_rv$multi })
    output$multivariate_normality_table_output <- renderTable({
      validate(need(results_rv$norm, "Click 'Run Normality Tests' to compute results."))
      results_rv$norm
    }, striped = TRUE, bordered = TRUE, na = "-")

    output$download_descriptives_button <- downloadHandler(
      filename = "descriptives.csv",
      content  = function(f) {
        req(results_rv$desc)
        write.csv(results_rv$desc, f)
      }
    )
    output$download_assumptions_apa7 <- downloadHandler(
      filename = function() paste0("assumption_checks_APA7_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        req(results_rv$desc)
        report_language <- fafa_language(language)
        sections <- list(list(
          title = fafa_text(report_language, "Descriptive Statistics", "Betimsel \u0130statistikler"),
          table = results_rv$desc,
          note = fafa_text(
            report_language,
            "Statistics were calculated using the active FAfA dataset.",
            "\u0130statistikler etkin FAfA veri seti kullan\u0131larak hesaplanm\u0131\u015ft\u0131r."
          )
        ))
        if (!is.null(results_rv$multi)) {
          sections <- c(sections, list(list(
            title = fafa_text(report_language, "Collinearity Diagnostics", "E\u015f Do\u011frusall\u0131k Tan\u0131lamalar\u0131"),
            table = results_rv$multi
          )))
        }
        if (!is.null(results_rv$norm)) {
          sections <- c(sections, list(list(
            title = fafa_text(report_language, "Multivariate Normality", "\u00c7ok De\u011fi\u015fkenli Normallik"),
            table = results_rv$norm
          )))
        }
        write_apa7_report(
          file,
          title = fafa_text(report_language, "Assumption Checks Report", "Varsay\u0131m Kontrolleri Raporu"),
          subtitle = paste0("FAfA ", fafa_package_version(), " - APA 7"),
          sections = sections,
          language = report_language
        )
      }
    )
  })
}
