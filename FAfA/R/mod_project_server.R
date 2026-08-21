#' Project and report server
#'
#' @param id Module namespace ID.
#' @param source_data Shared source-data reactive value.
#' @param active_data Reactive containing the active data.
#' @param app_inputs Reactive snapshot of application inputs.
#' @param state_store Shared module-state object.
#' @param restore_trigger Reactive value used to restore module state.
#' @param factor_dictionary Shared CFA factor dictionary.
#' @param audit_log Shared audit-log reactive value.
#' @param error_log Shared anonymized error-log reactive value.
#' @param source_name Shared source-name reactive value.
#' @noRd
project_server <- function(id, source_data, active_data, app_inputs,
                           state_store, restore_trigger, factor_dictionary,
                           audit_log, error_log, source_name) {
  moduleServer(id, function(input, output, session) {
    project_status_rv <- reactiveVal("No project file has been loaded.")

    current_project <- reactive({
      create_fafa_project(
        source_data = source_data(),
        active_data = active_data(),
        inputs = app_inputs(),
        module_state = reactiveValuesToList(state_store, all.names = TRUE),
        factor_dictionary = factor_dictionary(),
        audit = audit_log(),
        source_name = source_name(),
        include_data = isTRUE(input$include_project_data)
      )
    })

    observeEvent(input$app_language, {
      session$sendCustomMessage(
        "fafa-language",
        list(
          language = input$app_language,
          translations = as.list(fafa_translations(input$app_language))
        )
      )
    }, ignoreInit = FALSE)

    output$project_status <- renderText(project_status_rv())
    output$audit_table <- renderTable({
      audit_log()
    }, rownames = FALSE, striped = TRUE)

    observeEvent(input$clear_audit, {
      audit_log(empty_audit_log())
      project_status_rv("Workflow audit cleared.")
    })

    output$download_project <- downloadHandler(
      filename = function() paste0("FAfA_project_", Sys.Date(), ".fafa"),
      content = function(file) {
        saveRDS(current_project(), file, compress = "gzip", version = 3)
      }
    )

    observeEvent(input$load_project, {
      req(input$project_file$datapath)
      tryCatch({
        file_size <- input$project_file$size %||% 0
        if (isTRUE(is.finite(file_size) && file_size > 250 * 1024^2)) {
          stop("Project files larger than 250 MB are not accepted.")
        }
        project <- readRDS(input$project_file$datapath)
        validate_fafa_project(project)

        if (!is.null(project$source_data)) {
          source_data(project$source_data)
          source_name(project$source_name %||% "FAfA project")
        } else if (is.null(source_data())) {
          stop("This project does not contain data. Upload the matching dataset before loading it.")
        }

        factor_dictionary(project$factor_dictionary)
        audit_log(project$audit %||% empty_audit_log())

        restore_token <- paste0(as.numeric(Sys.time()), "-", sample.int(1e6, 1))
        restore_stage <- function(stage) {
          restore_trigger(list(
            token = paste(restore_token, stage, sep = "-"),
            stage = stage,
            module_state = project$module_state
          ))
        }

        session$onFlushed(function() {
          session$sendCustomMessage("fafa-restore-inputs", project$inputs)
          restore_stage("exclusion")
          session$onFlushed(function() {
            restore_stage("recode")
            session$onFlushed(function() {
              restore_stage("missing")
              session$onFlushed(function() {
                restore_stage("outliers")
              }, once = TRUE)
            }, once = TRUE)
          }, once = TRUE)
        }, once = TRUE)

        project_status_rv(
          paste0("Project loaded (created with FAfA ", project$package_version, ").")
        )
        audit_log(append_audit_entry(
          audit_log(), "Project", "Project loaded",
          paste("FAfA", project$package_version)
        ))
        showNotification("FAfA project loaded.", type = "message")
      }, error = function(e) {
        error_log(append_audit_entry(
          error_log(), "Project", "Load error", "Project file could not be loaded"
        ))
        project_status_rv(conditionMessage(e))
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    output$download_audit <- downloadHandler(
      filename = function() paste0("fafa_workflow_audit_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(audit_log(), file, row.names = FALSE)
    )

    output$download_r_script <- downloadHandler(
      filename = function() paste0("fafa_reproducible_analysis_", Sys.Date(), ".R"),
      content = function(file) writeLines(build_reproducible_script(current_project()), file, useBytes = TRUE)
    )

    output$download_html_report <- downloadHandler(
      filename = function() paste0("fafa_report_", Sys.Date(), ".html"),
      content = function(file) write_fafa_html_report(file, current_project())
    )

    output$download_pdf_report <- downloadHandler(
      filename = function() paste0("fafa_report_", Sys.Date(), ".pdf"),
      content = function(file) write_fafa_pdf_report(file, current_project())
    )

    output$download_apa7_report <- downloadHandler(
      filename = function() paste0("fafa_APA7_report_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        project <- current_project()
        report_language <- fafa_language(input$app_language %||% "en")
        active <- project$active_data %||% project$source_data
        project_summary <- data.frame(
          Field = c(
            fafa_text(report_language, "FAfA version", "FAfA s\u00fcr\u00fcm\u00fc"),
            fafa_text(report_language, "Project date", "Proje tarihi"),
            fafa_text(report_language, "Observations", "G\u00f6zlem say\u0131s\u0131"),
            fafa_text(report_language, "Variables", "De\u011fi\u015fken say\u0131s\u0131")
          ),
          Value = c(
            project$package_version,
            project$created_at,
            if (is.data.frame(active)) nrow(active) else NA,
            if (is.data.frame(active)) ncol(active) else NA
          ),
          check.names = FALSE
        )
        dictionary <- project$factor_dictionary %||% list()
        dictionary_table <- if (length(dictionary)) {
          data.frame(
            Factor = names(dictionary),
            Indicators = vapply(dictionary, paste, collapse = ", ", character(1)),
            check.names = FALSE
          )
        } else {
          NULL
        }
        sections <- list(
          list(
            title = fafa_text(report_language, "Project Summary", "Proje \u00d6zeti"),
            table = project_summary
          ),
          list(
            title = fafa_text(report_language, "Workflow Audit", "\u0130\u015f Ak\u0131\u015f\u0131 Kayd\u0131"),
            table = project$audit
          )
        )
        if (!is.null(dictionary_table)) {
          sections <- c(sections, list(list(
            title = fafa_text(report_language, "Factor Dictionary", "Fakt\u00f6r S\u00f6zl\u00fc\u011f\u00fc"),
            table = dictionary_table
          )))
        }
        write_apa7_report(
          file,
          title = fafa_text(report_language, "FAfA Analysis Report", "FAfA Analiz Raporu"),
          subtitle = "APA 7",
          sections = sections,
          language = report_language
        )
      }
    )

    output$download_diagnostic <- downloadHandler(
      filename = function() paste0("fafa_diagnostic_", Sys.Date(), ".txt"),
      content = function(file) write_fafa_diagnostic_report(file, error_log())
    )
  })
}
