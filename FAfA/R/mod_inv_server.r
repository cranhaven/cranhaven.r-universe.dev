#' Measurement Invariance Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @param language Optional reactive interface language.
#' @export
inv_server <- function(id, data, error_recorder = NULL, language = NULL) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }

    analysis_results_rv <- reactiveValues(
      fit_measures_df = NULL,
      model_comparison_df = NULL,
      ordinal_diagnostics_df = NULL,
      ordinal_recode_df = NULL,
      ordinal_diagnostics_status = NULL
    )


    observe({
      req(data())
      current_data <- data()
      analysis_results_rv$fit_measures_df <- NULL
      analysis_results_rv$model_comparison_df <- NULL
      analysis_results_rv$ordinal_diagnostics_df <- NULL
      analysis_results_rv$ordinal_recode_df <- NULL
      analysis_results_rv$ordinal_diagnostics_status <- NULL
      col_names <- names(current_data)
      numeric_names <- col_names[vapply(current_data, is.numeric, logical(1))]
      updateSelectizeInput(session, "builder_items", choices = numeric_names)
      updateSelectizeInput(session, "builder_cov_items", choices = col_names)

      is_cat <- sapply(current_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(stats::na.omit(x))) <= 10))
      updateSelectInput(session, "grouping_variable_select", choices = c("", col_names[is_cat]))
    })

    append_syntax <- function(new_text) {
      current <- input$inv_model_syntax
      if (is.null(current) || current == "") {
        updateTextAreaInput(session, "inv_model_syntax", value = new_text)
      } else {
        updateTextAreaInput(session, "inv_model_syntax", value = paste(current, new_text, sep = "\n"))
      }
    }

    observeEvent(input$btn_add_to_model, {
      req(input$builder_factor_name, input$builder_items)
      f_name <- trimws(input$builder_factor_name)
      f_name <- iconv(f_name, from = "", to = "ASCII//TRANSLIT", sub = "_")
      f_name <- make.names(gsub("[^A-Za-z0-9_.]", "_", f_name))
      items_str <- paste(input$builder_items, collapse = " + ")
      new_line <- paste0(f_name, " =~ ", items_str)
      append_syntax(new_line)

      updateTextInput(session, "builder_factor_name", value = "")
      updateSelectizeInput(session, "builder_items", selected = character(0))
    })

    observeEvent(input$btn_add_cov, {
      req(input$builder_cov_items)
      if(length(input$builder_cov_items) != 2) {
        showNotification("Select exactly 2 variables for covariance.", type = "warning")
        return()
      }
      new_line <- paste0(input$builder_cov_items[1], " ~~ ", input$builder_cov_items[2])
      append_syntax(new_line)

      updateSelectizeInput(session, "builder_cov_items", selected = character(0))
    })

    observeEvent(input$run_invariance_button, {
      validate(
        need(data(), "Upload data."),
        need(input$inv_model_syntax, "Define model."),
        need(input$grouping_variable_select, "Select grouping var."),
        need(length(input$invariance_levels_checkbox) > 0, "Select levels.")
      )

      tryCatch({
        analysis_results_rv$fit_measures_df <- NULL
        analysis_results_rv$model_comparison_df <- NULL
        analysis_results_rv$ordinal_diagnostics_df <- NULL
        analysis_results_rv$ordinal_recode_df <- NULL
        analysis_results_rv$ordinal_diagnostics_status <- NULL

        clean_res <- clean_missing_data(data(), remove_na = FALSE)
        dat <- clean_res$cleaned_data
        grp <- input$grouping_variable_select
        if (!grp %in% names(dat)) stop("The grouping variable is not in the active dataset.")
        dat <- dat[!is.na(dat[[grp]]), , drop = FALSE]
        dat[[grp]] <- droplevels(as.factor(dat[[grp]]))
        if (nlevels(dat[[grp]]) < 2) stop("The grouping variable must contain at least two groups.")

        model_parts <- lavaan::lavaanify(input$inv_model_syntax)
        factor_names <- unique(model_parts$lhs[model_parts$op == "=~"])
        manifest_vars <- setdiff(unique(model_parts$rhs[model_parts$op == "=~"]), factor_names)
        missing_vars <- setdiff(manifest_vars, names(dat))
        if (length(missing_vars)) {
          stop(paste("Variables not found in the active dataset:", paste(missing_vars, collapse = ", ")))
        }

        is_poly <- identical(input$correlation_matrix_type, "poly")
        est <- if (is_poly) "WLSMV" else "MLR"
        ordered_arg <- if (is_poly) manifest_vars else FALSE

        if (is_poly) {
          complete_rows <- stats::complete.cases(dat[, c(grp, manifest_vars), drop = FALSE])
          dat <- dat[complete_rows, , drop = FALSE]
          dat[[grp]] <- droplevels(as.factor(dat[[grp]]))
          if (nlevels(dat[[grp]]) < 2L) {
            stop("Ordinal listwise deletion left fewer than two groups.")
          }

          empty_cells <- find_empty_ordinal_cells(dat, grp, manifest_vars)
          analysis_results_rv$ordinal_diagnostics_df <- empty_cells

          if (nrow(empty_cells)) {
            empty_action <- input$ordinal_empty_category_action %||% "collapse"
            if (identical(empty_action, "stop")) {
              affected <- unique(paste0(
                empty_cells$Variable,
                " (group ",
                empty_cells$Group,
                ": ",
                empty_cells$Empty_Categories,
                ")"
              ))
              stop(
                paste0(
                  "Ordinal indicators contain categories that are empty within a group: ",
                  paste(affected, collapse = "; "),
                  ". Review the Ordinal Data Check tab, merge categories, remove the affected item, or analyze the indicators as continuous when substantively appropriate."
                )
              )
            }

            collapsed <- collapse_empty_ordinal_categories(dat, grp, manifest_vars)
            dat <- collapsed$data
            analysis_results_rv$ordinal_recode_df <- collapsed$audit
            remaining_empty <- find_empty_ordinal_cells(dat, grp, manifest_vars)
            if (nrow(remaining_empty)) {
              stop("Some ordinal categories remain empty after automatic merging. Recode these categories manually or remove the affected variables.")
            }
            analysis_results_rv$ordinal_diagnostics_status <- local_text(
              "Empty ordinal categories were merged consistently across all groups for this analysis. The original dataset was not changed.",
              "Bo\u015f ordinal kategoriler bu analiz i\u00e7in t\u00fcm gruplarda tutarl\u0131 bi\u00e7imde birle\u015ftirildi. \u00d6zg\u00fcn veri seti de\u011fi\u015ftirilmedi."
            )
            showNotification(
              analysis_results_rv$ordinal_diagnostics_status,
              type = "warning",
              duration = 12
            )
          } else {
            analysis_results_rv$ordinal_diagnostics_status <- local_text(
              "No empty ordinal categories were found within groups.",
              "Gruplar i\u00e7inde bo\u015f ordinal kategori bulunmad\u0131."
            )
          }
        }

        progress_id <- showNotification("Running invariance models...", duration = NULL, type = "message")
        on.exit(removeNotification(progress_id), add = TRUE)

        mods <- list()
        levels <- input$invariance_levels_checkbox
        model_errors <- character(0)

        run_mod <- function(model_name, equality = NULL) {
          tryCatch(
            lavaan::cfa(
              input$inv_model_syntax, data = dat, group = grp,
              group.equal = equality, estimator = est, ordered = ordered_arg,
              missing = if (is_poly) "listwise" else "fiml"
            ),
            error = function(e) {
              model_errors[[model_name]] <<- conditionMessage(e)
              NULL
            }
          )
        }

        scalar_constraints <- if (is_poly) c("loadings", "thresholds") else c("loadings", "intercepts")
        strict_constraints <- c(scalar_constraints, "residuals")
        if("configural" %in% levels) mods$configural <- run_mod("Configural")
        if("metric" %in% levels)     mods$metric     <- run_mod("Metric", "loadings")
        if("scalar" %in% levels)     mods$scalar     <- run_mod("Scalar", scalar_constraints)
        if("strict" %in% levels)     mods$strict     <- run_mod("Strict", strict_constraints)

        valid_mods <- Filter(Negate(is.null), mods)
        if (!length(valid_mods)) {
          stop(paste(c("All requested models failed:", unname(model_errors)), collapse = "\n"))
        }

        res_list <- lapply(names(valid_mods), function(n) {
          m <- valid_mods[[n]]
          fm <- lavaan::fitMeasures(m)
          cfi <- fm["cfi.robust"] %||% fm["cfi.scaled"] %||% fm["cfi"]
          rmsea <- fm["rmsea.robust"] %||% fm["rmsea.scaled"] %||% fm["rmsea"]
          chisq <- fm["chisq.scaled"] %||% fm["chisq"]
          degrees <- fm["df.scaled"] %||% fm["df"]
          data.frame(Model = n, CFI = cfi, RMSEA = rmsea, ChiSq = chisq, df = degrees)
        })
        analysis_results_rv$fit_measures_df <- do.call(rbind, res_list)
        analysis_results_rv$fit_measures_df$DeltaCFI <- c(NA_real_, diff(analysis_results_rv$fit_measures_df$CFI))
        analysis_results_rv$fit_measures_df$DeltaRMSEA <- c(NA_real_, diff(analysis_results_rv$fit_measures_df$RMSEA))
        numeric_columns <- setdiff(names(analysis_results_rv$fit_measures_df), "Model")
        analysis_results_rv$fit_measures_df[numeric_columns] <- lapply(
          analysis_results_rv$fit_measures_df[numeric_columns], round, digits = 3
        )

        if(length(valid_mods) > 1) {
          analysis_results_rv$model_comparison_df <- tryCatch(
            measurement_invariance_lrt(valid_mods),
            error = function(e) data.frame(Message = conditionMessage(e))
          )
        } else {
          analysis_results_rv$model_comparison_df <- data.frame(
            Message = "Select at least two invariance levels for model comparison."
          )
        }

        if (length(model_errors)) {
          showNotification(
            paste("Some models could not be fitted:", paste(names(model_errors), collapse = ", ")),
            type = "warning", duration = 10
          )
        }
        showNotification("Invariance analysis completed.", type = "message")
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Invariance", "Analysis error")
        analysis_results_rv$fit_measures_df <- NULL
        analysis_results_rv$model_comparison_df <- NULL
        showNotification(conditionMessage(e), type = "error", duration = 12)
      })
    })

    output$invariance_fit_measures_table <- renderTable({ analysis_results_rv$fit_measures_df }, striped=TRUE)
    output$model_comparison_table <- renderTable({ analysis_results_rv$model_comparison_df }, rownames=FALSE, striped=TRUE)
    output$ordinal_diagnostics_status <- renderText({
      analysis_results_rv$ordinal_diagnostics_status %||% local_text(
        "Run an ordinal invariance analysis to check response categories within groups.",
        "Gruplardaki yan\u0131t kategorilerini kontrol etmek i\u00e7in ordinal bir de\u011fi\u015fmezlik analizi \u00e7al\u0131\u015ft\u0131r\u0131n."
      )
    })
    output$ordinal_diagnostics_table <- renderTable({
      req(analysis_results_rv$ordinal_diagnostics_df)
      analysis_results_rv$ordinal_diagnostics_df
    }, rownames = FALSE, striped = TRUE)
    output$ordinal_recode_table <- renderTable({
      req(analysis_results_rv$ordinal_recode_df)
      analysis_results_rv$ordinal_recode_df
    }, rownames = FALSE, striped = TRUE)

    output$download_fit_measures_button <- downloadHandler(
      filename="inv_fit.csv", content=function(f) {
        req(analysis_results_rv$fit_measures_df)
        write.csv(analysis_results_rv$fit_measures_df, f, row.names = FALSE)
      }
    )
    output$download_model_comparison_button <- downloadHandler(
      filename="inv_comp.csv", content=function(f) {
        req(analysis_results_rv$model_comparison_df)
        write.csv(analysis_results_rv$model_comparison_df, f, row.names = FALSE)
      }
    )
    output$download_invariance_apa7 <- downloadHandler(
      filename = function() paste0("measurement_invariance_APA7_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        req(analysis_results_rv$fit_measures_df)
        report_language <- fafa_language(language)
        sections <- list(list(
          title = fafa_text(report_language, "Invariance Fit Indices", "De\u011fi\u015fmezlik Uyum \u0130ndeksleri"),
          table = analysis_results_rv$fit_measures_df,
          note = fafa_text(
            report_language,
            "Changes in CFI and RMSEA should be interpreted together with theory and model complexity.",
            "CFI ve RMSEA de\u011fi\u015fimleri kuram ve model karma\u015f\u0131kl\u0131\u011f\u0131yla birlikte yorumlanmal\u0131d\u0131r."
          )
        ))
        if (!is.null(analysis_results_rv$ordinal_diagnostics_df) &&
            nrow(analysis_results_rv$ordinal_diagnostics_df)) {
          sections <- c(sections, list(list(
            title = fafa_text(
              report_language,
              "Empty Ordinal Categories",
              "Bo\u015f Ordinal Kategoriler"
            ),
            table = analysis_results_rv$ordinal_diagnostics_df,
            note = fafa_text(
              report_language,
              "These categories were not observed in at least one group before model estimation.",
              "Bu kategoriler model kestiriminden \u00f6nce en az bir grupta g\u00f6zlenmemi\u015ftir."
            )
          )))
        }
        if (!is.null(analysis_results_rv$ordinal_recode_df) &&
            nrow(analysis_results_rv$ordinal_recode_df)) {
          sections <- c(sections, list(list(
            title = fafa_text(
              report_language,
              "Ordinal Category Merges",
              "Ordinal Kategori Birle\u015ftirmeleri"
            ),
            table = analysis_results_rv$ordinal_recode_df,
            note = fafa_text(
              report_language,
              "Merges were applied consistently across groups to the analysis copy; the original dataset was not changed.",
              "Birle\u015ftirmeler analiz kopyas\u0131nda gruplar aras\u0131nda tutarl\u0131 bi\u00e7imde uygulanm\u0131\u015f, \u00f6zg\u00fcn veri seti de\u011fi\u015ftirilmemi\u015ftir."
            )
          )))
        }
        if (!is.null(analysis_results_rv$model_comparison_df)) {
          sections <- c(sections, list(list(
            title = fafa_text(report_language, "Nested Model Comparisons", "\u0130\u00e7 \u0130\u00e7e Model Kar\u015f\u0131la\u015ft\u0131rmalar\u0131"),
            table = analysis_results_rv$model_comparison_df
          )))
        }
        write_apa7_report(
          file,
          title = fafa_text(report_language, "Measurement Invariance Report", "\u00d6l\u00e7me De\u011fi\u015fmezli\u011fi Raporu"),
          subtitle = paste0("FAfA ", fafa_package_version(), " - APA 7"),
          sections = sections,
          language = report_language
        )
      }
    )
  })
}
