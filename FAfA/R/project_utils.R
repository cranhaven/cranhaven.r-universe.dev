# Project files contain configuration and, when requested, a copy of the data.
empty_audit_log <- function() {
  data.frame(
    Time = character(0),
    Module = character(0),
    Action = character(0),
    Details = character(0),
    stringsAsFactors = FALSE
  )
}

append_audit_entry <- function(log, module, action, details = "") {
  if (!is.data.frame(log)) log <- empty_audit_log()
  rbind(
    log,
    data.frame(
      Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Module = as.character(module),
      Action = as.character(action),
      Details = paste(as.character(details), collapse = "; "),
      stringsAsFactors = FALSE
    )
  )
}

new_session_cache <- function() {
  new.env(parent = emptyenv(), hash = TRUE)
}

session_cache_key <- function(...) {
  bytes <- as.integer(serialize(list(...), NULL, ascii = FALSE))
  first_hash <- 0
  second_hash <- 0
  for (byte in bytes) {
    first_hash <- (first_hash * 131 + byte + 1) %% 2147483629
    second_hash <- (second_hash * 65599 + byte + 1) %% 2147483587
  }
  paste(length(bytes), format(first_hash, scientific = FALSE),
        format(second_hash, scientific = FALSE), sep = "-")
}

session_cache_get <- function(cache, key) {
  if (exists(key, envir = cache, inherits = FALSE)) {
    get(key, envir = cache, inherits = FALSE)
  } else {
    NULL
  }
}

session_cache_set <- function(cache, key, value) {
  assign(key, value, envir = cache)
  invisible(value)
}

clear_session_cache <- function(cache) {
  keys <- ls(envir = cache, all.names = TRUE)
  if (length(keys)) rm(list = keys, envir = cache)
  invisible(cache)
}

fafa_package_version <- function() {
  tryCatch(
    as.character(getNamespaceVersion("FAfA")),
    error = function(e) {
      tryCatch(
        as.character(utils::packageVersion("FAfA")),
        error = function(e) "1.2"
      )
    }
  )
}

safe_project_inputs <- function(values) {
  if (!is.list(values) || is.null(names(values))) return(list())
  action_pattern <- paste0(
    "(^|[-_])(btn_|run_|apply_|reset_|recover_|exclude_|check_|remove_|",
    "split_|analyze_|download_|load_project|clear_audit)"
  )
  keep <- vapply(values, function(value) {
    is.atomic(value) && length(value) <= 500
  }, logical(1))
  keep <- keep & !grepl(action_pattern, names(values), ignore.case = TRUE)
  values[keep]
}

strip_project_data <- function(module_state) {
  if (is.data.frame(module_state)) return(module_state)
  if (!is.list(module_state)) return(module_state)
  if ("outliers" %in% names(module_state) && is.list(module_state$outliers)) {
    module_state$outliers$table <- NULL
  }
  data_fields <- c("data", "recoded_data", "processed_data", "data_clean")
  for (field in intersect(names(module_state), data_fields)) {
    module_state[[field]] <- NULL
  }
  lapply(module_state, strip_project_data)
}

create_fafa_project <- function(source_data, active_data, inputs,
                                module_state, factor_dictionary, audit,
                                source_name = NULL, include_data = TRUE) {
  if (!is.list(module_state)) module_state <- list()
  if (!isTRUE(include_data)) module_state <- strip_project_data(module_state)

  list(
    signature = "FAfA-project",
    format_version = 1L,
    package_version = fafa_package_version(),
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    source_name = if (isTRUE(include_data)) as.character(source_name %||% "") else "",
    source_data = if (isTRUE(include_data)) source_data else NULL,
    active_data = if (isTRUE(include_data)) active_data else NULL,
    inputs = safe_project_inputs(inputs),
    module_state = module_state,
    factor_dictionary = factor_dictionary %||% list(),
    audit = if (is.data.frame(audit)) audit else empty_audit_log()
  )
}

validate_fafa_project <- function(project) {
  if (!is.list(project) || !identical(project$signature, "FAfA-project")) {
    stop("The selected file is not a FAfA project file.")
  }
  if (!identical(as.integer(project$format_version), 1L)) {
    stop("This FAfA project format is not supported by the installed version.")
  }
  if (!is.null(project$source_data) && !is.data.frame(project$source_data)) {
    stop("The project data is not in a valid data-frame format.")
  }
  if (!is.null(project$active_data) && !is.data.frame(project$active_data)) {
    stop("The active project data is not in a valid data-frame format.")
  }
  if (!is.list(project$inputs) || !is.list(project$module_state)) {
    stop("The project settings are incomplete.")
  }
  if (!is.list(project$factor_dictionary)) {
    stop("The saved factor dictionary is invalid.")
  }
  invisible(project)
}

r_literal <- function(value) {
  paste(deparse(value, width.cutoff = 500L), collapse = "")
}

project_input <- function(project, id, default = NULL) {
  project$inputs[[id]] %||% default
}

build_reproducible_script <- function(project) {
  validate_fafa_project(project)
  state <- project$module_state
  exclusion <- state$exclusion$excluded %||% character(0)
  recode_rules <- state$recode$rules
  missing_method <- state$missing$method %||%
    project_input(project, "missing_val-imputation_method", "none")
  cfa_syntax <- project_input(project, "cfa-cfa_model_syntax_input", "")
  cfa_corr <- project_input(project, "cfa-cfa_correlation_type_radio", "pea")
  cfa_est <- project_input(project, "cfa-cfa_estimator_select", "default")
  efa_factors <- project_input(project, "efa_analysis-number_factor", 1)
  efa_rotation <- project_input(project, "efa_analysis-rotating_method", "oblimin")
  efa_method <- project_input(project, "efa_analysis-fact_method", "minres")
  efa_corr <- project_input(project, "efa_analysis-cor_kind", "poly")

  lines <- c(
    "# Reproducible FAfA analysis script",
    paste0("# Generated with FAfA ", project$package_version),
    paste0("# Project created: ", project$created_at),
    "",
    "# Replace this path with the original data file.",
    "data <- read.csv(\"replace_with_data_path.csv\", check.names = FALSE)",
    ""
  )

  if (length(exclusion)) {
    lines <- c(
      lines,
      "# Excluded variables",
      paste0("excluded_variables <- ", r_literal(exclusion)),
      "data <- data[, setdiff(names(data), excluded_variables), drop = FALSE]",
      ""
    )
  }

  if (is.data.frame(recode_rules) && nrow(recode_rules)) {
    lines <- c(lines, "# Reverse-scored variables")
    for (i in seq_len(nrow(recode_rules))) {
      variable <- recode_rules$Variable[[i]]
      constant <- recode_rules$Minimum[[i]] + recode_rules$Maximum[[i]]
      lines <- c(
        lines,
        paste0("data[[", r_literal(variable), "]] <- ", constant,
               " - data[[", r_literal(variable), "]]")
      )
    }
    lines <- c(lines, "")
  }

  if (!identical(missing_method, "none")) {
    missing_code <- switch(
      missing_method,
      listwise = "data <- stats::na.omit(data)",
      mean = "data[] <- lapply(data, function(x) { if (is.numeric(x)) x[is.na(x)] <- mean(x, na.rm = TRUE); x })",
      median = "data[] <- lapply(data, function(x) { if (is.numeric(x)) x[is.na(x)] <- stats::median(x, na.rm = TRUE); x })",
      amelia = "data <- Amelia::amelia(data, m = 1, p2s = 0)$imputations[[1]]",
      mice = "data <- mice::complete(mice::mice(data, m = 1, printFlag = FALSE), 1)",
      missForest_cont = "data <- missForest::missForest(data, verbose = FALSE)$ximp",
      missForest_cat = "data <- missForest::missForest(data.frame(lapply(data, as.factor)), verbose = FALSE)$ximp",
      "# No imputation code was generated for the selected method."
    )
    lines <- c(lines, "# Missing-value handling", missing_code, "")
  }

  lines <- c(
    lines,
    "# Analyses below use numeric variables, as in the FAfA interface.",
    "analysis_data <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]",
    ""
  )

  lines <- c(
    lines,
    "# Exploratory factor analysis",
    paste0("efa_result <- psych::fa(analysis_data, nfactors = ", as.integer(efa_factors),
           ", rotate = ", r_literal(efa_rotation),
           ", fm = ", r_literal(efa_method),
           ", cor = ", r_literal(if (identical(efa_corr, "pea")) "cor" else "poly"), ")"),
    "print(efa_result)",
    ""
  )

  if (nzchar(trimws(cfa_syntax))) {
    estimator <- if (identical(cfa_est, "default")) {
      if (identical(cfa_corr, "poly")) "WLSMV" else "MLR"
    } else cfa_est
    lines <- c(
      lines,
      "# Confirmatory factor analysis",
      paste0("cfa_model <- ", r_literal(cfa_syntax)),
      paste0("cfa_result <- lavaan::cfa(cfa_model, data = analysis_data, estimator = ",
             r_literal(estimator), ")"),
      "summary(cfa_result, fit.measures = TRUE, standardized = TRUE)",
      ""
    )
  }

  paste(lines, collapse = "\n")
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

data_frame_html <- function(x) {
  if (!is.data.frame(x) || !nrow(x)) return("<p>No recorded operations.</p>")
  header <- paste0("<th>", html_escape(names(x)), "</th>", collapse = "")
  rows <- apply(x, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table><thead><tr>", header, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}

write_fafa_html_report <- function(file, project) {
  validate_fafa_project(project)
  active <- project$active_data %||% project$source_data
  data_summary <- if (is.data.frame(active)) {
    paste0(nrow(active), " rows and ", ncol(active), " variables")
  } else {
    "Data was not embedded in the project."
  }
  script <- build_reproducible_script(project)
  dictionary <- project$factor_dictionary
  dictionary_text <- if (length(dictionary)) {
    paste(vapply(names(dictionary), function(name) {
      paste0(name, " = ", paste(dictionary[[name]], collapse = ", "))
    }, character(1)), collapse = "\n")
  } else "No factor dictionary was saved."

  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<title>FAfA Reproducible Report</title>",
    "<style>body{font-family:'Segoe UI',Arial,sans-serif;max-width:1100px;margin:36px auto;color:#1e293b;line-height:1.5}",
    "h1,h2{color:#1d4ed8}table{border-collapse:collapse;width:100%;font-size:13px}",
    "th,td{border:1px solid #cbd5e1;padding:7px;text-align:left}th{background:#e2e8f0}",
    "pre{background:#f1f5f9;padding:16px;border-radius:8px;overflow:auto}</style></head><body>",
    "<h1>FAfA Reproducible Analysis Report</h1>",
    "<p><strong>FAfA version:</strong> ", html_escape(project$package_version), "<br>",
    "<strong>Project date:</strong> ", html_escape(project$created_at), "<br>",
    "<strong>Active data:</strong> ", html_escape(data_summary), "</p>",
    "<h2>Workflow audit</h2>", data_frame_html(project$audit),
    "<h2>Factor dictionary</h2><pre>", html_escape(dictionary_text), "</pre>",
    "<h2>Reproducible R code</h2><pre>", html_escape(script), "</pre>",
    "</body></html>"
  )
  writeLines(html, file, useBytes = TRUE)
}

project_report_lines <- function(project) {
  active <- project$active_data %||% project$source_data
  dimensions <- if (is.data.frame(active)) {
    paste(nrow(active), "rows x", ncol(active), "variables")
  } else "Data not embedded"
  audit_lines <- if (is.data.frame(project$audit) && nrow(project$audit)) {
    apply(project$audit, 1, function(row) paste(row, collapse = " | "))
  } else "No recorded operations."
  c(
    "FAfA Reproducible Analysis Report",
    paste("FAfA version:", project$package_version),
    paste("Project date:", project$created_at),
    paste("Active data:", dimensions),
    "",
    "Workflow audit",
    audit_lines,
    "",
    "Reproducible R code",
    strsplit(build_reproducible_script(project), "\n", fixed = TRUE)[[1]]
  )
}

write_fafa_pdf_report <- function(file, project) {
  validate_fafa_project(project)
  raw_lines <- project_report_lines(project)
  wrapped <- unlist(lapply(raw_lines, function(line) {
    if (!nzchar(line)) "" else strwrap(line, width = 92)
  }), use.names = FALSE)
  pages <- split(wrapped, ceiling(seq_along(wrapped) / 52))
  grDevices::pdf(file, width = 8.27, height = 11.69, family = "Helvetica")
  on.exit(grDevices::dev.off(), add = TRUE)
  for (page in pages) {
    graphics::par(family = "sans", mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::text(
      x = 0.05,
      y = 0.96,
      labels = paste(page, collapse = "\n"),
      adj = c(0, 1),
      family = "mono",
      cex = 0.72
    )
  }
  invisible(file)
}

diagnostic_package_versions <- function() {
  packages <- c(
    "FAfA", "R", "shiny", "bslib", "bsicons", "psych", "lavaan",
    "semPlot", "EFAtools", "EFA.MRFA", "EGAnet", "ggplot2"
  )
  versions <- vapply(packages, function(package) {
    if (identical(package, "R")) return(as.character(getRversion()))
    tryCatch(as.character(utils::packageVersion(package)), error = function(e) "not installed")
  }, character(1))
  data.frame(Package = packages, Version = versions, stringsAsFactors = FALSE)
}

write_fafa_diagnostic_report <- function(file, error_log = NULL) {
  errors <- if (is.data.frame(error_log) && nrow(error_log)) {
    paste(error_log$Time, error_log$Module, error_log$Action, sep = " | ")
  } else {
    "No application errors were recorded in this session."
  }
  version_lines <- apply(diagnostic_package_versions(), 1, paste, collapse = ": ")
  lines <- c(
    "FAfA anonymized diagnostic report",
    paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste("R platform:", R.version$platform),
    paste("Operating system:", Sys.info()[["sysname"]], Sys.info()[["release"]]),
    paste("Locale:", Sys.getlocale("LC_CTYPE")),
    "",
    "Package versions",
    version_lines,
    "",
    "Recorded error categories",
    errors,
    "",
    "This report does not contain uploaded data, variable names, model syntax,",
    "file paths, user names, or workflow details."
  )
  writeLines(lines, file, useBytes = TRUE)
}
