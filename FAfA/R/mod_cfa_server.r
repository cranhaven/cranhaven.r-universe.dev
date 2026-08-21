#' Confirmatory Factor Analysis (CFA) Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @param factor_dictionary Shared reactive value containing factor-to-item mappings.
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @param language Optional reactive interface language.
#' @details The Dynamic Fit Index workflow called by this module is adapted
#'   from version 1.1.0 of the `dynamic` R package under the GNU Affero General
#'   Public License, version 3 (AGPL-3). The integration was rewritten for
#'   FAfA's existing lavaan analysis flow rather than copied verbatim.
#' @references McNeish, D., & Wolf, M. G. (2023). Dynamic fit index cutoffs
#'   for confirmatory factor analysis models. *Psychological Methods, 28*(1),
#'   61-88. \doi{10.1037/met0000425}
#' @importFrom utils head
#' @export
cfa_server <- function(id, data, factor_dictionary = NULL,
                       error_recorder = NULL, language = NULL) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }
    if (is.null(factor_dictionary)) factor_dictionary <- reactiveVal(list())

    cfa_analysis_results_rv <- reactiveValues(
      lavaan_object = NULL,
      fit_measures_df = NULL,
      factor_loadings_df = NULL,
      modification_indices_df = NULL
    )
    dynamic_fit_result_rv <- reactiveVal(NULL)
    dynamic_fit_status_rv <- reactiveVal(NULL)
    cfa_cache <- new_session_cache()
    dynamic_fit_cache <- new_session_cache()

    dictionary_from_syntax <- function(syntax) {
      observed_factor_dictionary(syntax, names(data()))
    }

    observeEvent(data(), {
      clear_session_cache(cfa_cache)
      cfa_analysis_results_rv$lavaan_object <- NULL
      cfa_analysis_results_rv$fit_measures_df <- NULL
      cfa_analysis_results_rv$factor_loadings_df <- NULL
      cfa_analysis_results_rv$modification_indices_df <- NULL
      dynamic_fit_result_rv(NULL)
      dynamic_fit_status_rv(NULL)
      clear_session_cache(dynamic_fit_cache)
    }, ignoreNULL = TRUE)

    observeEvent(data(), {
      req(data())
      available <- names(data())
      selected <- intersect(
        isolate(input$builder_items %||% character(0)),
        available
      )
      updateSelectizeInput(
        session,
        "builder_items",
        choices = available,
        selected = selected
      )
    }, ignoreNULL = TRUE)

    observe({
      dictionary <- factor_dictionary()
      available_factors <- names(Filter(length, dictionary))
      general_factor <- safe_lavaan_name(
        input$builder_general_factor_name %||% "G"
      )
      observed_names <- if (is.null(data())) character(0) else names(data())

      updateSelectizeInput(
        session,
        "builder_lower_factors",
        choices = available_factors,
        selected = intersect(
          isolate(input$builder_lower_factors %||% character(0)),
          available_factors
        )
      )
      bifactor_choices <- setdiff(available_factors, general_factor)
      updateSelectizeInput(
        session,
        "builder_bifactor_groups",
        choices = bifactor_choices,
        selected = intersect(
          isolate(input$builder_bifactor_groups %||% character(0)),
          bifactor_choices
        )
      )
      covariance_choices <- unique(c(observed_names, available_factors))
      updateSelectizeInput(
        session,
        "builder_cov_items",
        choices = covariance_choices,
        selected = intersect(
          isolate(input$builder_cov_items %||% character(0)),
          covariance_choices
        )
      )
    })

    append_syntax <- function(new_text) {
      current <- input$cfa_model_syntax_input
      if (is.null(current) || current == "") {
        updateTextAreaInput(session, "cfa_model_syntax_input", value = new_text)
      } else {
        updateTextAreaInput(session, "cfa_model_syntax_input", value = paste(current, new_text, sep = "\n"))
      }
    }

    observeEvent(input$btn_add_to_model, {
      f_name <- trimws(input$builder_factor_name %||% "")
      if (nchar(f_name) == 0) {
        showNotification(
          "Factor name is required. Type a name (e.g. F1) before adding to syntax.",
          type = "warning", duration = 5
        )
        return()
      }
      safe_factor <- safe_lavaan_name(f_name)
      if (!identical(f_name, safe_factor)) {
        showNotification(paste0("Factor name changed to analysis-safe form: ", safe_factor), type = "message")
        f_name <- safe_factor
      }
      if (is.null(input$builder_items) || length(input$builder_items) == 0) {
        showNotification("Select at least one indicator variable.", type = "warning")
        return()
      }
      new_syntax <- set_lavaan_measurement(
        input$cfa_model_syntax_input %||% "",
        f_name,
        input$builder_items
      )
      updateTextAreaInput(
        session,
        "cfa_model_syntax_input",
        value = new_syntax
      )
      factor_dictionary(dictionary_from_syntax(new_syntax))
      updateTextInput(session,     "builder_factor_name", value = "")
      updateSelectizeInput(session, "builder_items",      selected = character(0))
    })

    observeEvent(input$btn_add_second_order, {
      tryCatch({
        higher_factor <- safe_lavaan_name(
          input$builder_higher_factor_name %||% ""
        )
        lower_factors <- input$builder_lower_factors %||% character(0)
        missing_factors <- setdiff(lower_factors, names(factor_dictionary()))
        if (length(missing_factors)) {
          stop(
            "First-order factors are not defined: ",
            paste(missing_factors, collapse = ", ")
          )
        }

        new_syntax <- build_second_order_syntax(
          input$cfa_model_syntax_input %||% "",
          higher_factor,
          lower_factors
        )
        updateTextAreaInput(
          session,
          "cfa_model_syntax_input",
          value = new_syntax
        )
        factor_dictionary(dictionary_from_syntax(new_syntax))
        showNotification(
          paste0("Second-order factor added: ", higher_factor),
          type = "message"
        )
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
      })
    })

    observeEvent(input$btn_build_bifactor, {
      tryCatch({
        general_factor <- safe_lavaan_name(
          input$builder_general_factor_name %||% ""
        )
        new_syntax <- build_bifactor_syntax(
          input$cfa_model_syntax_input %||% "",
          factor_dictionary(),
          general_factor,
          input$builder_bifactor_groups %||% character(0),
          orthogonal = isTRUE(input$builder_bifactor_orthogonal)
        )
        updateTextAreaInput(
          session,
          "cfa_model_syntax_input",
          value = new_syntax
        )
        factor_dictionary(dictionary_from_syntax(new_syntax))
        showNotification(
          paste0("Bifactor syntax created with general factor ", general_factor, "."),
          type = "message"
        )
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
      })
    })

    observeEvent(input$cfa_model_syntax_input, {
      req(data())
      factor_dictionary(dictionary_from_syntax(input$cfa_model_syntax_input))
    }, ignoreInit = TRUE)

    observeEvent(input$upload_model_syntax, {
      req(input$upload_model_syntax$datapath)
      tryCatch({
        syntax <- paste(readLines(input$upload_model_syntax$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        updateTextAreaInput(session, "cfa_model_syntax_input", value = syntax)
        factor_dictionary(dictionary_from_syntax(syntax))
      }, error = function(e) showNotification(paste("Syntax file error:", e$message), type = "error"))
    })

    output$download_model_syntax <- downloadHandler(
      filename = function() paste0("cfa_model_", Sys.Date(), ".lav"),
      content = function(file) writeLines(input$cfa_model_syntax_input %||% "", file, useBytes = TRUE)
    )

    observeEvent(input$btn_add_cov, {
      req(input$builder_cov_items)
      if(length(input$builder_cov_items) != 2) {
        showNotification("Select exactly 2 variables.", type = "warning")
        return()
      }
      new_line <- paste0(input$builder_cov_items[1], " ~~ ", input$builder_cov_items[2])
      append_syntax(new_line)
      updateSelectizeInput(session, "builder_cov_items", selected = character(0))
    })

    observeEvent(input$cfa_correlation_type_radio, {
      req(input$cfa_correlation_type_radio)
      estimator_choices <- if (input$cfa_correlation_type_radio == "pea") {
        c("Default (MLR)" = "default", "MLR", "ML", "GLS")
      } else {
        c("Default (WLSMV)" = "default", "WLSMV", "ULSMV", "DWLS")
      }
      updateSelectInput(session, "cfa_estimator_select", choices = estimator_choices, selected = "default")
    })

    observeEvent(input$run_cfa_button, {
      validate(
        need(data(), "Upload data."),
        need(input$cfa_model_syntax_input, "Define model.")
      )

      current_data <- data()
      model_syntax <- input$cfa_model_syntax_input
      corr_type <- input$cfa_correlation_type_radio
      est_sel <- input$cfa_estimator_select

      final_estimator <- est_sel
      if (est_sel == "default") {
        final_estimator <- if (corr_type == "poly") "WLSMV" else "MLR"
      }

      cache_key <- session_cache_key(model_syntax, corr_type, final_estimator)
      cached_result <- session_cache_get(cfa_cache, cache_key)
      if (!is.null(cached_result)) {
        cfa_analysis_results_rv$lavaan_object <- cached_result$lavaan_object
        cfa_analysis_results_rv$fit_measures_df <- cached_result$fit_measures_df
        cfa_analysis_results_rv$factor_loadings_df <- cached_result$factor_loadings_df
        cfa_analysis_results_rv$modification_indices_df <- cached_result$modification_indices_df
        dynamic_fit_result_rv(NULL)
        dynamic_fit_status_rv(NULL)
        showNotification("Saved session result was used.", type = "message")
        return()
      }

      progress_id <- showNotification("Running CFA...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        lavaan_parts <- lavaan::lavaanify(model_syntax)
        factor_names <- unique(lavaan_parts$lhs[lavaan_parts$op == "=~"])
        manifest_vars <- setdiff(unique(lavaan_parts$rhs[lavaan_parts$op == "=~"]), factor_names)
        missing_vars <- setdiff(manifest_vars, names(current_data))
        if (length(missing_vars)) stop(paste("Variables not found in the active dataset:", paste(missing_vars, collapse = ", ")))
        ordered_arg <- if (corr_type == "poly") manifest_vars else FALSE

        fit <- lavaan::cfa(
          model = model_syntax,
          data = current_data,
          ordered = ordered_arg,
          estimator = final_estimator,
          missing = "listwise",
          mimic = "Mplus"
        )
        cfa_analysis_results_rv$lavaan_object <- fit
        dynamic_fit_result_rv(NULL)
        dynamic_fit_status_rv(NULL)

        # Fit Measures
        fm_raw <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
        if(!is.null(fm_raw)) {
          chi <- fm_raw["chisq.scaled"] %||% fm_raw["chisq"]
          df  <- fm_raw["df.scaled"] %||% fm_raw["df"]
          p   <- fm_raw["pvalue.scaled"] %||% fm_raw["pvalue"]
          cfi <- fm_raw["cfi.scaled"] %||% fm_raw["cfi.robust"] %||% fm_raw["cfi"]
          tli <- fm_raw["tli.scaled"] %||% fm_raw["tli.robust"] %||% fm_raw["tli"]
          rmsea     <- fm_raw["rmsea.scaled"] %||% fm_raw["rmsea.robust"] %||% fm_raw["rmsea"]
          rmsea_low <- fm_raw["rmsea.ci.lower.scaled"] %||% fm_raw["rmsea.ci.lower.robust"] %||% fm_raw["rmsea.ci.lower"]
          rmsea_upp <- fm_raw["rmsea.ci.upper.scaled"] %||% fm_raw["rmsea.ci.upper.robust"] %||% fm_raw["rmsea.ci.upper"]
          srmr <- fm_raw["srmr"] %||% fm_raw["srmr_bentler"]

          cfa_analysis_results_rv$fit_measures_df <- data.frame(
            Measure = c("Chi-Square", "Degrees of Freedom (df)", "Chi-Square / df", "p-value",
                        "CFI", "TLI (NNFI)", "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
            Value = round(c(chi, df, if (!is.na(df) && df > 0) chi / df else NA_real_, p,
                            cfi, tli, rmsea, rmsea_low, rmsea_upp, srmr), 3)
          )
          if(!is.na(p)) cfa_analysis_results_rv$fit_measures_df$Value[4] <- format.pval(p, digits=3, eps=0.001)
        }

        # Loadings
        std_sol <- lavaan::standardizedSolution(fit)
        loadings <- std_sol[std_sol$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
        colnames(loadings) <- c("Factor", "Item", "Std. Estimate", "SE", "p-value")
        loadings[,3:4] <- round(loadings[,3:4], 3)
        loadings[,5]   <- format.pval(loadings[,5], digits=3, eps=0.001)
        cfa_analysis_results_rv$factor_loadings_df <- loadings

        # Mod Indices
        mod_ind <- lavaan::modificationIndices(fit, sort. = TRUE, minimum.value = 3.84)
        cfa_analysis_results_rv$modification_indices_df <- head(mod_ind[, c("lhs","op","rhs","mi","epc")], 20)

        session_cache_set(cfa_cache, cache_key, list(
          lavaan_object = cfa_analysis_results_rv$lavaan_object,
          fit_measures_df = cfa_analysis_results_rv$fit_measures_df,
          factor_loadings_df = cfa_analysis_results_rv$factor_loadings_df,
          modification_indices_df = cfa_analysis_results_rv$modification_indices_df
        ))

        showNotification("CFA Analysis Complete!", type = "message")

      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("CFA", "Analysis error")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        cfa_analysis_results_rv$lavaan_object <- NULL
      })
    })

    output$cfa_path_diagram_output <- renderPlot({
      req(cfa_analysis_results_rv$lavaan_object)

      selected_layout <- input$plot_layout %||% "tree"
      rotation_val    <- input$plot_rotation %||% 2
      box_width       <- input$plot_man_size %||% 10
      label_cex       <- input$plot_edge_label_cex %||% 0.8

      if(selected_layout == "tree2") {
        selected_layout <- "tree"
        rotation_val <- 2
      }

      what_labels <- if(!is.null(input$plot_show_labels) && input$plot_show_labels) "std" else "hide"

      is_poly <- (input$cfa_correlation_type_radio == "poly")
      custom_edge_width <- if(is_poly) 0.5 else 1.2
      # Residual labels make ordinal diagrams unreadable and add little beside
      # the standardized loadings.
      show_residuals <- !is_poly

      semPlot::semPaths(
        object = cfa_analysis_results_rv$lavaan_object,
        what = "std",
        whatLabels = what_labels,
        layout = selected_layout,
        rotation = rotation_val,

        shapeMan      = "rectangle",
        sizeMan       = box_width,
        sizeMan2      = box_width / 2,
        sizeLat       = box_width,
        sizeLat2      = box_width / 2,

        label.cex     = 1.2,
        edge.label.cex = label_cex,
        edge.width    = custom_edge_width,
        edge.color    = "black",
        style         = "lisrel",

        intercepts    = FALSE,
        thresholds    = FALSE,
        residuals     = show_residuals,
        residScale    = 15,

        reorder          = FALSE,
        optimizeLatRes   = TRUE,
        curve            = 2.5,
        mar              = c(5, 5, 5, 5),
        nCharNodes       = 0,
        theme            = "gray"
      )
    })

    output$cfa_fit_measures_table <- renderTable({ cfa_analysis_results_rv$fit_measures_df }, striped = TRUE, bordered = TRUE)
    output$cfa_factor_loadings_table <- renderTable({ cfa_analysis_results_rv$factor_loadings_df }, striped = TRUE)
    output$cfa_modification_indices_table <- renderTable({ cfa_analysis_results_rv$modification_indices_df }, striped = TRUE)
    output$conventional_fit_table <- renderTable({
      req(cfa_analysis_results_rv$fit_measures_df)
      conventional_fit_summary(
        cfa_analysis_results_rv$fit_measures_df,
        fafa_language(language)
      )
    }, striped = TRUE, bordered = TRUE)
    output$dynamic_fit_status <- renderText({
      dynamic_fit_status_rv() %||% local_text(
        "Run CFA before requesting model-specific dynamic cutoffs.",
        "Modele \u00f6zg\u00fc dinamik kesme de\u011ferlerini hesaplamadan \u00f6nce DFA'y\u0131 \u00e7al\u0131\u015ft\u0131r\u0131n."
      )
    })
    output$dynamic_fit_cutoffs_table <- renderTable({
      result <- dynamic_fit_result_rv()
      req(result$cutoffs)
      result$cutoffs
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)

    observeEvent(input$run_dynamic_fit_button, {
      req(cfa_analysis_results_rv$lavaan_object, data())

      selected_scale <- input$dynamic_fit_scale %||% "auto"
      if (identical(selected_scale, "auto")) {
        selected_scale <- if (identical(input$cfa_correlation_type_radio, "poly")) {
          "categorical"
        } else {
          "normal"
        }
      }
      replications <- as.integer(input$dynamic_fit_reps %||% 250L)
      simulation_seed <- as.integer(input$dynamic_fit_seed %||% 2026L)

      tryCatch({
        mad_values <- parse_dynamic_mad(input$dynamic_fit_mad %||% "0.038, 0.05, 0.06")
        cache_key <- session_cache_key(
          input$cfa_model_syntax_input,
          input$cfa_correlation_type_radio,
          input$cfa_estimator_select,
          selected_scale,
          replications,
          mad_values,
          simulation_seed
        )
        cached_dynamic <- session_cache_get(dynamic_fit_cache, cache_key)
        if (!is.null(cached_dynamic)) {
          dynamic_fit_result_rv(cached_dynamic)
          dynamic_fit_status_rv(local_text(
            "Saved Dynamic Fit Index result was used.",
            "Kaydedilmi\u015f Dinamik Uyum \u0130ndeksi sonucu kullan\u0131ld\u0131."
          ))
          return()
        }

        dynamic_fit_status_rv(local_text(
          "Dynamic cutoffs are being simulated. This may take several minutes.",
          "Dinamik kesme de\u011ferleri benzetimle hesaplan\u0131yor. Bu i\u015flem birka\u00e7 dakika s\u00fcrebilir."
        ))
        progress_id <- showNotification(
          local_text(
            "Running Dynamic Fit Index simulations...",
            "Dinamik Uyum \u0130ndeksi benzetimleri \u00e7al\u0131\u015ft\u0131r\u0131l\u0131yor..."
          ),
          duration = NULL,
          type = "message"
        )
        on.exit(removeNotification(progress_id), add = TRUE)

        raw_result <- run_dynamic_fit(
          fit = cfa_analysis_results_rv$lavaan_object,
          data = data(),
          scale = selected_scale,
          reps = replications,
          mad = mad_values,
          model = input$cfa_model_syntax_input,
          seed = simulation_seed
        )
        prepared_result <- list(
          raw = raw_result,
          cutoffs = normalize_dynamic_cutoffs(raw_result, fafa_language(language)),
          scale = selected_scale,
          reps = replications,
          mad = mad_values
        )
        dynamic_fit_result_rv(prepared_result)
        session_cache_set(dynamic_fit_cache, cache_key, prepared_result)
        dynamic_fit_status_rv(paste0(
          local_text(
            "Dynamic cutoffs completed. Replications: ",
            "Dinamik kesme de\u011ferleri tamamland\u0131. Benzetim say\u0131s\u0131: "
          ),
          replications
        ))
        showNotification(
          local_text(
            "Dynamic Fit Index analysis completed.",
            "Dinamik Uyum \u0130ndeksi analizi tamamland\u0131."
          ),
          type = "message"
        )
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("CFA", "Dynamic fit error")
        dynamic_fit_status_rv(conditionMessage(e))
        showNotification(conditionMessage(e), type = "error", duration = 12)
      })
    })

    # Downloads
    output$download_fit_measures_button <- downloadHandler(
      filename = "cfa_fit_measures.csv", content = function(file) {
        req(cfa_analysis_results_rv$fit_measures_df)
        write.csv(cfa_analysis_results_rv$fit_measures_df, file, row.names = FALSE)
      }
    )
    output$download_factor_loadings_button <- downloadHandler(
      filename = "cfa_factor_loadings.csv", content = function(file) {
        req(cfa_analysis_results_rv$factor_loadings_df)
        write.csv(cfa_analysis_results_rv$factor_loadings_df, file, row.names = FALSE)
      }
    )
    output$download_dynamic_fit_csv <- downloadHandler(
      filename = function() paste0("cfa_dynamic_fit_cutoffs_", Sys.Date(), ".csv"),
      content = function(file) {
        result <- dynamic_fit_result_rv()
        req(result$cutoffs)
        utils::write.csv(result$cutoffs, file, row.names = FALSE)
      }
    )
    output$download_cfa_apa7 <- downloadHandler(
      filename = function() paste0("cfa_APA7_report_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        req(
          cfa_analysis_results_rv$fit_measures_df,
          cfa_analysis_results_rv$factor_loadings_df
        )
        report_language <- fafa_language(language)
        dynamic_result <- dynamic_fit_result_rv()
        sections <- list(
          list(
            title = local_text("Model Fit", "Model Uyumu"),
            text = apa7_cfa_narrative(
              cfa_analysis_results_rv$fit_measures_df,
              report_language
            ),
            table = cfa_analysis_results_rv$fit_measures_df,
            table_title = local_text("CFA Fit Indices", "DFA Uyum \u0130ndeksleri")
          ),
          list(
            title = local_text(
              "Conventional Fit Criteria",
              "Geleneksel Uyum \u00d6l\u00e7\u00fctleri"
            ),
            table = conventional_fit_summary(
              cfa_analysis_results_rv$fit_measures_df,
              report_language
            ),
            note = local_text(
              "These conventional cutoffs are general guidelines and should be interpreted with model-specific evidence.",
              "Bu geleneksel kesme de\u011ferleri genel k\u0131lavuzlard\u0131r ve modele \u00f6zg\u00fc kan\u0131tlarla birlikte yorumlanmal\u0131d\u0131r."
            )
          ),
          list(
            title = local_text("Standardized Factor Loadings", "Standartla\u015ft\u0131r\u0131lm\u0131\u015f Fakt\u00f6r Y\u00fckleri"),
            table = cfa_analysis_results_rv$factor_loadings_df
          ),
          list(
            title = local_text("Modification Indices", "D\u00fczeltme \u0130ndeksleri"),
            table = cfa_analysis_results_rv$modification_indices_df,
            note = local_text(
              "Modification indices should be considered only when supported by theory.",
              "D\u00fczeltme indeksleri yaln\u0131zca kuramsal olarak desteklendi\u011finde dikkate al\u0131nmal\u0131d\u0131r."
            )
          )
        )
        if (!is.null(dynamic_result$cutoffs)) {
          sections <- c(sections, list(list(
            title = local_text("Dynamic Fit Index Cutoffs", "Dinamik Uyum \u0130ndeksi Kesme De\u011ferleri"),
            text = paste0(
              local_text("Simulation scale: ", "Benzetim \u00f6l\u00e7e\u011fi: "),
              dynamic_result$scale,
              "; ",
              local_text("replications: ", "benzetim say\u0131s\u0131: "),
              dynamic_result$reps,
              "."
            ),
            table = dynamic_result$cutoffs,
            note = local_text(
              "Model-specific cutoffs were generated through FAfA's adaptation of the Direct Discrepancy Dynamic Fit Index simulation framework.",
              "Modele \u00f6zg\u00fc kesme de\u011ferleri, Do\u011frudan Tutars\u0131zl\u0131k Dinamik Uyum \u0130ndeksi benzetim yakla\u015f\u0131m\u0131n\u0131n FAfA uyarlamas\u0131yla hesaplanm\u0131\u015ft\u0131r."
            )
          )))
        }
        write_apa7_report(
          file,
          title = local_text(
            "Confirmatory Factor Analysis Report",
            "Do\u011frulay\u0131c\u0131 Fakt\u00f6r Analizi Raporu"
          ),
          subtitle = paste0("FAfA ", fafa_package_version(), " - APA 7"),
          sections = sections,
          language = report_language
        )
      }
    )
    output$download_path_diagram_button <- downloadHandler(
      filename = function() {
        syntax <- input$cfa_model_syntax_input %||% ""
        factors <- names(parse_factor_dictionary(syntax))
        higher_order <- length(factors) && any(vapply(parse_factor_dictionary(syntax), function(items) any(items %in% factors), logical(1)))
        paste0(if (higher_order) "ho_" else "", "cfa_path_diagram.", input$plot_download_format %||% "png")
      },
      content = function(file) {
        req(cfa_analysis_results_rv$lavaan_object)
        format <- input$plot_download_format %||% "png"
        if (format == "svg") {
          grDevices::svg(file, width = 12, height = 8)
        } else if (format == "jpg") {
          grDevices::jpeg(file, width = 12, height = 8, units = "in", res = 300, quality = 95)
        } else {
          grDevices::png(file, width = 12, height = 8, units = "in", res = 300)
        }
        on.exit(grDevices::dev.off(), add = TRUE)

        selected_layout <- input$plot_layout %||% "tree"
        rotation_val <- input$plot_rotation %||% 2
        box_width <- input$plot_man_size %||% 10
        if(selected_layout == "tree2") { selected_layout <- "tree"; rotation_val <- 2 }
        is_poly <- (input$cfa_correlation_type_radio == "poly")
        custom_edge_width <- if(is_poly) 0.5 else 1.2

        is_poly_dl <- (input$cfa_correlation_type_radio == "poly")
        semPlot::semPaths(
          object = cfa_analysis_results_rv$lavaan_object,
          what = "std", whatLabels = if (isTRUE(input$plot_show_labels)) "std" else "hide",
          layout = selected_layout, rotation = rotation_val,
          shapeMan = "rectangle", sizeMan = box_width, sizeMan2 = box_width/2,
          sizeLat = box_width, sizeLat2 = box_width/2,
          label.cex = 1.2,
          edge.label.cex = input$plot_edge_label_cex %||% 0.8,
          edge.width    = custom_edge_width,
          thresholds    = FALSE,
          reorder       = FALSE,
          optimizeLatRes = TRUE,
          curve          = 2.5,
          mar            = c(5, 5, 5, 5),
          residScale    = 15,
          residuals     = !is_poly_dl,
          edge.color    = "black",
          style         = "lisrel",
          intercepts    = FALSE,
          nCharNodes    = 0
        )
      }
    )
  })
}
