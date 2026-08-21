#' The application server-side
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  source_data_rv <- reactiveVal(NULL)
  source_name_rv <- reactiveVal("")
  module_state <- reactiveValues()
  restore_trigger <- reactiveVal(NULL)
  factor_dictionary_rv <- reactiveVal(list())
  audit_log_rv <- reactiveVal(empty_audit_log())
  error_log_rv <- reactiveVal(empty_audit_log())
  interface_language <- reactive(input[["project-app_language"]] %||% "en")

  record_audit <- function(module, action, details = "") {
    audit_log_rv(append_audit_entry(
      isolate(audit_log_rv()), module, action, details
    ))
  }

  record_error <- function(module, category) {
    error_log_rv(append_audit_entry(
      isolate(error_log_rv()), module, category, ""
    ))
  }

  # The file is read only after the user confirms the upload.
  observeEvent(input[["data_selection-analyze_data"]], {
    inFile <- input[["data_selection-file1"]]
    user_has_header <- input[["data_selection-has_header_checkbox"]] %||% TRUE

    if (is.null(inFile)) {
      showNotification("Please select a data file first.", type = "warning")
      return()
    }

    tryCatch({
      ext <- tools::file_ext(tolower(inFile$name))
      df <- switch(ext,
                   "csv" = utils::read.csv(inFile$datapath, header = user_has_header, na.strings = c("NA", "", " ", ".", "na", "NaN")),
                   "xlsx" = readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("NA", "", " ")),
                   "xls" = readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("NA", "", " ")),
                   "sav" = haven::read_sav(inFile$datapath),
                   "txt" = utils::read.table(inFile$datapath, header = user_has_header, na.strings = c("NA", "", " ")),
                   "dat" = utils::read.table(inFile$datapath, header = user_has_header, na.strings = c("NA", "", " ")),
                   stop(paste0("Unsupported file type: .", ext))
      )

      original_names <- names(df)
      df <- normalize_variable_names(as.data.frame(df))
      if (!identical(original_names, names(df))) {
        changed <- original_names != names(df)
        showNotification(
          paste0("Variable names were made analysis-safe: ",
                 paste(paste0(original_names[changed], " -> ", names(df)[changed]), collapse = ", ")),
          type = "message", duration = 10
        )
      }

      # Missing values stay in place until the user chooses a handling method.
      cleaned_result <- clean_missing_data(df, remove_na = FALSE)

      if (is.null(cleaned_result$cleaned_data)) {
        stop("The selected file did not contain usable data.")
      }
      source_data_rv(cleaned_result$cleaned_data)
      source_name_rv(inFile$name %||% "uploaded data")
      record_audit(
        "Data", "Data loaded",
        paste(nrow(cleaned_result$cleaned_data), "rows and",
              ncol(cleaned_result$cleaned_data), "variables")
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      record_error("Data", "Import error")
    })
  }, ignoreInit = TRUE)

  shared_data_reactive <- reactive(source_data_rv())

  # Data preparation follows the order shown in the navigation menu.
  data_selection_server(
    "data_selection",
    data = shared_data_reactive,
    language = interface_language
  )

  data_after_exclusion_reactive <- wrangling_server_ex_var(
    "wrangling_ex_var", data = shared_data_reactive,
    project_state = module_state, restore_state = restore_trigger,
    error_recorder = record_error
  )
  data_after_recode_reactive <- wrangling_server_recode(
    "wrangling_recode", data = data_after_exclusion_reactive,
    project_state = module_state, restore_state = restore_trigger,
    error_recorder = record_error, language = interface_language
  )

  processed_missing_data <- mod_missing_server(
    "missing_val", data = data_after_recode_reactive,
    project_state = module_state, restore_state = restore_trigger,
    error_recorder = record_error, language = interface_language
  )

  final_wrangled_data_reactive <- wrangling_server_outliers(
    "wrangling_outliers", data = processed_missing_data,
    project_state = module_state, restore_state = restore_trigger,
    error_recorder = record_error
  )

  numeric_analysis_data <- reactive({
    req(final_wrangled_data_reactive())
    current <- final_wrangled_data_reactive()
    current[, vapply(current, is.numeric, logical(1)), drop = FALSE]
  })

  wrangling_server_split(
    "wrangling_split", data = final_wrangled_data_reactive,
    error_recorder = record_error
  )

  # Analyses that do not need a grouping variable receive numeric columns only.
  assumptions_server(
    "assumptions", data = numeric_analysis_data,
    error_recorder = record_error,
    language = interface_language
  )
  mod_itemrest_server(
    "item_rest", data = numeric_analysis_data,
    error_recorder = record_error, language = interface_language
  )
  efa_server_fac_ret(
    "efa_fac_ret", data = numeric_analysis_data,
    error_recorder = record_error
  )

  efa_settings_reactive <- moduleServer("efa_analysis", function(input, output, session) {
    reactive({
      list(number_factor = input$number_factor, rotating_method = input$rotating_method, fact_method = input$fact_method, cor_kind = input$cor_kind)
    })
  })

  returned_efa_object_reactive <- efa_server_analysis(
    "efa_analysis", data = numeric_analysis_data,
    error_recorder = record_error
  )
  efa_server_report(
    "efa_report", data = numeric_analysis_data,
    efa_output_reactive = returned_efa_object_reactive,
    efa_settings_reactive = efa_settings_reactive,
    language = interface_language
  )

  ega_server(
    "ega",
    data = numeric_analysis_data,
    error_recorder = record_error,
    language = interface_language
  )
  cfa_server(
    "cfa", data = numeric_analysis_data,
    factor_dictionary = factor_dictionary_rv,
    error_recorder = record_error,
    language = interface_language
  )
  inv_server(
    "inv", data = final_wrangled_data_reactive,
    error_recorder = record_error,
    language = interface_language
  )
  reliability_server(
    "reliability", data = numeric_analysis_data,
    factor_dictionary = factor_dictionary_rv,
    error_recorder = record_error
  )
  item_weighting_server(
    "item_weighting", data = numeric_analysis_data,
    error_recorder = record_error
  )
  about_server("about")

  project_server(
    "project",
    source_data = source_data_rv,
    active_data = final_wrangled_data_reactive,
    app_inputs = reactive(reactiveValuesToList(input, all.names = TRUE)),
    state_store = module_state,
    restore_trigger = restore_trigger,
    factor_dictionary = factor_dictionary_rv,
    audit_log = audit_log_rv,
    error_log = error_log_rv,
    source_name = source_name_rv
  )

  audit_actions <- list(
    c("wrangling_ex_var-exclude_button", "Data", "Variables excluded"),
    c("wrangling_ex_var-recover_button", "Data", "Variables recovered"),
    c("wrangling_ex_var-reset_button", "Data", "Variable exclusions reset"),
    c("wrangling_recode-apply_reverse_scoring", "Data", "Reverse scoring applied"),
    c("wrangling_recode-reset_reverse_scoring", "Data", "Reverse scoring reset"),
    c("missing_val-apply_imputation", "Missing values", "Imputation requested"),
    c("missing_val-run_mcar_button", "Missing values", "MCAR test requested"),
    c("wrangling_split-split_data_button", "Data", "Dataset split"),
    c("wrangling_outliers-check_outliers_button", "Outliers", "Outlier check requested"),
    c("wrangling_outliers-remove_outliers_button", "Outliers", "Outlier removal requested"),
    c("assumptions-run_descriptives_button", "Assumptions", "Descriptives requested"),
    c("assumptions-run_collinearity_button", "Assumptions", "Collinearity check requested"),
    c("assumptions-run_normality_tests_button", "Assumptions", "Normality tests requested"),
    c("efa_fac_ret-run_factor_ret", "EFA", "Factor retention requested"),
    c("efa_analysis-run_efa", "EFA", "EFA requested"),
    c("item_rest-run_itemrest", "EFA", "Item dropout requested"),
    c("ega-run_ega_button", "EGA", "EGA requested"),
    c("ega-run_bootega_button", "EGA", "Bootstrap EGA requested"),
    c("cfa-run_cfa_button", "CFA", "CFA requested"),
    c("inv-run_invariance_button", "Invariance", "Measurement invariance requested"),
    c("reliability-run_reliability_button", "Reliability", "Reliability requested"),
    c("item_weighting-calculate_weighted_scores_button", "Item weighting", "Scores requested")
  )

  lapply(audit_actions, function(specification) {
    observeEvent(input[[specification[[1]]]], {
      record_audit(specification[[2]], specification[[3]])
    }, ignoreInit = TRUE)
  })
}
