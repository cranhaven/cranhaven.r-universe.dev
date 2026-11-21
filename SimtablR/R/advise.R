# POST-COMPUTE EDUCATOR RULE FRAMEWORK
# internal engine for the SimtablR educator guidance system used by the package to provide advice on methodological issues in computed results.
# provides methodological guidance, best-practice warnings, and diagnostics
# Should be didactic and never completly block an action, always be able to be turned off, and not annoying.

# Internal in-memory storage environment for registered educational rules.
.simtab_rule_registry <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.simtab_ruleset_version <- function() {
  "downscale-2026-09-23"
}

#' Register an educator rule
#'
#' A rule is a tested, non-blocking advisory contract evaluated against a
#' computed result. `applies()` decides whether the rule is relevant; `check()`
#' returns `NULL` or an advice entry. The example functions are executable
#' contract cases used by the test suite.
#'
#' @param id Stable rule id.
#' @param applies Function taking a `simtab_result` and returning `TRUE`/`FALSE`.
#' @param check Function taking a `simtab_result` and returning `NULL`, one
#'   advice entry, or a list of advice entries.
#' @param severity Integer rung from 0 to 4. No rung blocks computation.
#' @param citation Character citation/rationale.
#' @param fix One-line suggested fix.
#' @param version Rule version.
#' @param example_fires Function returning a result where the rule must fire.
#' @param example_silent Function returning a result where the rule must stay silent.
#' @param checklist Optional named character vector mapping guideline families
#'   to item numbers, for example `c(strobe = "14")`.
#' @return Invisibly, the registered rule.
#' @keywords internal
#' @noRd
register_rule <- function(id, applies, check, severity, citation, fix, version,
                          example_fires, example_silent, checklist = NULL) {
  key <- .normalise_registry_name(id, "id")
  # Validate that validation predicates and test fixtures are executable functions
  for (arg in c("applies", "check", "example_fires", "example_silent")) {
    if (!is.function(get(arg))) {
      simtab_abort_input("{.arg {arg}} must be a function.")
    }
  }
  for (arg in c("citation", "fix", "version")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1 || !nzchar(value)) {
      simtab_abort_input("{.arg {arg}} must be a non-empty string.")
    }
  }
  # Validate severity range: 0 (audit/info) to 4 (critical methodological hazard)
  if (!is.numeric(severity) || length(severity) != 1 || is.na(severity) ||
      severity < 0 || severity > 4) {
    simtab_abort_input(c(
      "{.arg severity} must be a single numeric rung from 0 to 4.",
      "i" = "Received: {.val {severity}}.",
      "v" = "Rung 0 is informational; rung 4 is the strongest methodological concern."
    ))
  }
  checklist <- .normalise_rule_checklist(checklist)


  # Build the rule object with S3 class 'simtab_rule'
  rule <- structure(
    list(
      id = key,
      applies = applies,
      check = check,
      severity = as.integer(severity),
      citation = citation,
      fix = fix,
      version = version,
      example_fires = example_fires,
      example_silent = example_silent,
      checklist = checklist,
      origin = if (startsWith(version, .simtab_ruleset_version())) "builtin" else "user" # Built-in rules match package version; custom user rules are marked 'user'
    ),
    class = "simtab_rule"
  )
  # Store inside registry environment
  assign(key, rule, envir = .simtab_rule_registry)
  invisible(rule)
}


#' List registered educator rules
#'
#' @return A data.frame with rule ids and metadata.
#' @keywords internal
#' @noRd
list_rules <- function() {
  rules <- .registered_rules()
  if (length(rules) == 0) {
    return(data.frame(
      id = character(),
      severity = integer(),
      citation = character(),
      fix = character(),
      version = character(),
      origin = character(),
      checklist = character()
    ))
  }
  out <- do.call(rbind, lapply(rules, function(rule) {
    data.frame(
      id = rule$id,
      severity = rule$severity,
      citation = rule$citation,
      fix = rule$fix,
      version = rule$version,
      origin = rule$origin,
      checklist = .format_rule_checklist(rule$checklist %||% character())
    )
  }))
  rownames(out) <- NULL
  out[order(out$id), , drop = FALSE]
}


#' Normalizes the raw output of a rule's `check()` function into standardized advice entries
#' @keywords internal
#' @noRd
.normalise_rule_checklist <- function(checklist) {
  if (is.null(checklist)) {
    return(character())
  }
  if (!is.character(checklist) || anyNA(checklist)) {
    simtab_abort_input(c(
      "{.arg checklist} must be a named character vector.",
      "i" = "Received an object of class {.cls {class(checklist)[[1]]}}.",
      "v" = "Pass {.code c(strobe = \"12a\")} mapping checklist to item."
    ))
  }
  nms <- names(checklist)
  if (is.null(nms) || any(!nzchar(nms)) || any(!nzchar(checklist))) {
    simtab_abort_input(c(
      "{.arg checklist} must be a named character vector.",
      "i" = "Every element needs a non-empty name and a non-empty value.",
      "v" = "Pass {.code c(strobe = \"12a\")} mapping checklist to item."
    ))
  }
  names(checklist) <- tolower(nms)
  checklist
}

#' @keywords internal
#' @noRd
.format_rule_checklist <- function(checklist) {
  if (length(checklist) == 0) {
    return("")
  }
  paste(sprintf("%s:%s", names(checklist), unname(checklist)), collapse = ", ")
}

#' @keywords internal
#' @noRd
.registered_rules <- function() {
  keys <- sort(ls(envir = .simtab_rule_registry))
  lapply(keys, get, envir = .simtab_rule_registry, inherits = FALSE)
}

#' @keywords internal
#' @noRd
.normalise_advice_entries <- function(value, rule) {
  if (is.null(value)) {
    return(list())
  }

  if (is.list(value) && !is.null(value$message)) {
    value <- list(value)
  }
  if (!is.list(value)) {
    return(list())
  }

  entries <- list()
  for (entry in value) {
    if (!is.list(entry)) {
      next
    }
    msg <- entry$message %||% rule$message %||% rule$id
    if (!is.character(msg) || length(msg) != 1 || !nzchar(msg)) {
      next
    }
    entries[[length(entries) + 1]] <- list(
      id = .normalise_registry_name(entry$id %||% rule$id, "id"),
      severity = as.integer(entry$severity %||% rule$severity),
      message = msg,
      citation = entry$citation %||% rule$citation,
      fix = entry$fix %||% rule$fix,
      version = entry$version %||% rule$version,
      why = entry$why %||% NULL
    )
  }
  entries
}
#' Deduplicates multiple advice items sharing the same rule ID
#' @keywords internal
#' @noRd
.dedupe_advice <- function(advice) {
  if (length(advice) == 0) {
    return(list())
  }
  ids <- vapply(advice, `[[`, character(1), "id")
  advice[!duplicated(ids)]
}

#########
# ADVICE RUNNER AND EVALUATION DISPATCH

#' Run educator advice rules
#'
#' Evaluates all applicable rules against a computed result. Rule failures are
#' caught and skipped so advice can never break analysis.
#'
#' @param result A computed `simtab_result` or `simtab_report`.
#' @param audit Logical. `FALSE` (default) returns the fired advice entries.
#'   `TRUE` runs every applicable rule regardless of the guidance dial and
#'   returns a `simtab_audit` report of checked, fired, and silent rules
#'   (single results only).
#' @return A list of advice entries deduplicated by rule id, or a
#'   `simtab_audit` object when `audit = TRUE`.
#' @seealso [simtablr_references] for the full references behind the
#'   citations shown in advice, and [simtablr_guidance()] to control display.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' advise(res)
#' advise(res, audit = TRUE)
#' @export
advise <- function(result, audit = FALSE) {
  UseMethod("advise")
}

#' Evaluates rules against a single computed result
#' @noRd
#' @export
advise.simtab_result <- function(result, audit = FALSE) {
  result <- validate_simtab_result(result)
  if (isTRUE(audit)) {
    return(.audit_result(result))
  }
  .dedupe_advice(.run_advice_rules(result))
}

#' @export
advise.simtab_report <- function(result, audit = FALSE) {
  result <- validate_simtab_report(result)
  if (isTRUE(audit)) {
    simtab_abort_input(c(
      "{.code audit = TRUE} needs a single computed result.",
      "i" = "A report bundles several results.",
      "v" = "Audit each item, e.g. {.code advise(report$items[[1]], audit = TRUE)}."
    ))
  }
  advice <- result$advice %||% list()
  advice <- c(advice, .report_advice_from_items(result$items))
  advice <- c(advice, .run_advice_rules(result))
  .dedupe_advice(advice)
}

#' Evaluate every registered rule against a result/report; rule failures are
#' caught and skipped so advice can never break analysis.
#' @keywords internal
#' @noRd
.run_advice_rules <- function(result) {
  advice <- list()
  for (rule in .registered_rules()) {
    applies <- tryCatch(isTRUE(rule$applies(result)), error = function(e) FALSE)
    if (!applies) {
      next
    }
    # Run advice check safely
    entries <- tryCatch(
      .normalise_advice_entries(rule$check(result), rule),
      error = function(e) list()
    )
    advice <- c(advice, entries)
  }
  advice
}

#' Automatically computes and attaches advice entries to a result object metadata
#' @keywords internal
#' @noRd
.attach_advice <- function(result) {
  result <- validate_simtab_result(result)
  result$meta$ruleset_version <- .simtab_ruleset_version()
  result$advice <- tryCatch(advise(result), error = function(e) list())
  result
}

##########
#DISPLAY & GUIDANCE SETTINGS

#' Set or query SimtablR educator guidance
#'
#' Guidance changes only which stored advice is displayed. `"default"` shows
#' severity 3-4 advice and counts hidden severity 1-2 notes; `"important"`
#' and its compatibility alias `"quiet"` show severity 3-4 advice only;
#' `"teaching"` shows severity 1-4 advice with external citations and recorded
#' rationale; `"strict"` shows severity 1-4 advice in compact prose; and
#' `"off"` hides automatic advice. Severity 0 remains audit-only.
#' When a result has only one non-audit advice entry, that entry is shown under
#' every profile except `"off"` instead of being reported as hidden.
#'
#' Guidance is a session-level display setting read when a stored result is
#' printed. Call `simtablr_guidance()` separately; do not place it inside the
#' `...` of `table1()` or `tb()`. Changing guidance does not recompute results.
#'
#' @param level One of `"default"`, `"important"`, `"quiet"`, `"teaching"`,
#'   `"strict"`, or `"off"`. If omitted, returns the current level.
#' @return The active guidance level, invisibly when setting.
#' @seealso [simtablr_references] for the full references behind the
#'   citations shown in advice.
#' @examples
#' previous <- simtablr_guidance()
#' result <- table1(epitabl, "sex")
#' simtablr_guidance("teaching")
#' print(result)
#' simtablr_guidance(previous)
#' suppressMessages(print(result))
#' @export
simtablr_guidance <- function(level = NULL) {
  allowed <- c("default", "important", "quiet", "teaching", "strict", "off")
  if (is.null(level)) {
    current <- getOption("simtablr.guidance", "default")
    if (!current %in% allowed) {
      current <- "default"
    }
    return(current)
  }
  if (isFALSE(level)) {
    level <- "off"
  }
  if (!is.character(level) || length(level) != 1 || !tolower(level) %in% allowed) {
    simtab_abort_input(c(
      "{.arg level} must be one of {.val {allowed}}.",
      "i" = "Received: {.val {level}}.",
      "v" = "Use {.code simtablr_guidance(\"quiet\")} to reduce advice output."
    ))
  }
  level <- tolower(level)
  options(simtablr.guidance = level)
  invisible(level)
}

#' Maps guidance level name to filtering rules; minimum severity, details, hidden counts
#' @keywords internal
#' @noRd
.guidance_profile <- function(level = simtablr_guidance()) {
  switch(
    level,
    default = list(min_severity = 3L, show_hidden_count = TRUE, detailed = FALSE),
    important = list(min_severity = 3L, show_hidden_count = FALSE, detailed = FALSE),
    quiet = list(min_severity = 3L, show_hidden_count = FALSE, detailed = FALSE),
    teaching = list(min_severity = 1L, show_hidden_count = FALSE, detailed = TRUE),
    strict = list(min_severity = 1L, show_hidden_count = FALSE, detailed = FALSE),
    off = list(min_severity = 5L, show_hidden_count = FALSE, detailed = FALSE),
    list(min_severity = 3L, show_hidden_count = TRUE, detailed = FALSE)
  )
}

#' Sorts advice items descending by severity rung (highest concern first)
#' @keywords internal
#' @noRd
.order_advice_by_severity <- function(advice) {
  if (length(advice) < 2L) {
    return(advice)
  }
  severity <- vapply(
    advice,
    function(entry) as.integer(entry$severity %||% 0L),
    integer(1)
  )
  advice[order(-severity, seq_along(advice))]
}

#' Filters stored advice according to the active profile's minimum severity threshold
#' @keywords internal
#' @noRd
.filter_advice_for_guidance <- function(advice, level = simtablr_guidance()) {
  if (length(advice) == 0 || identical(level, "off")) {
    return(list())
  }
  profile <- .guidance_profile(level)
  advice <- .dedupe_advice(advice)
  out <- advice[vapply(advice, function(entry) {
    sev <- entry$severity %||% 0L
    sev > 0L && sev >= profile$min_severity
  }, logical(1))]
  .order_advice_by_severity(out)
}

#' Formats a single advice entry into a readable line, including fix and citation if enabled
#' @keywords internal
#' @noRd
.format_advice_line <- function(entry, level = simtablr_guidance()) {
  main <- entry$message
  detailed <- isTRUE(.guidance_profile(level)$detailed)
  if (detailed) {
    citation <- entry$citation %||% ""
    if (nzchar(citation) && !startsWith(citation, "SimtablR")) {
      main <- paste0(main, " (", citation, ")")
    }
  }
  if (!is.null(entry$fix) && nzchar(entry$fix)) {
    main <- paste(main, entry$fix)
  }
  if (detailed) {
    if (!is.null(entry$why) && nzchar(entry$why)) {
      main <- paste(main, entry$why)
    }
  }
  main
}

#' Distinguishes between warning >=3 and general guidance, and emits a callout with the advice lines
#' @keywords internal
#' @noRd
.emit_advice_callout <- function(lines, warning = FALSE) {
  if (length(lines) == 0L) {
    return(invisible(NULL))
  }
  header <- if (warning) {
    "{.strong Methodological warning}"
  } else {
    "{.strong Methodological guidance}"
  }
  message <- header
  names(message) <- if (warning) "!" else "i"
  body <- sprintf("{lines[[%d]]}", seq_along(lines))
  names(body) <- rep(" ", length(body))
  cli::cli_inform(c(message, body), .envir = environment())
}

#' Main print interceptor for advice attached to simtab result objects
#' @keywords internal
#' @noRd
.print_advice <- function(x) {
  advice <- .order_advice_by_severity(.dedupe_advice(x$advice %||% list()))
  if (length(advice) == 0) {
    return(invisible(NULL))
  }
  level <- simtablr_guidance()
  profile <- .guidance_profile(level)

  # Non-audit items (severity > 0)
  non_audit <- advice[vapply(advice, function(entry) {
    (entry$severity %||% 0L) > 0L
  }, logical(1))]

  # Special rule: If exactly one non-audit note exists display it
  show_singleton <- !identical(level, "off") && length(non_audit) == 1L
  shown <- if (show_singleton) {
    non_audit
  } else {
    .filter_advice_for_guidance(advice, level)
  }
  # Calculate number of hidden severity 1-2 notes under default mode
  hidden_count <- 0L
  if (isTRUE(profile$show_hidden_count) && !show_singleton) {
    hidden_count <- sum(vapply(advice, function(entry) {
      severity <- entry$severity %||% 0L
      severity >= 1L && severity <= 2L
    }, logical(1)))
  }
  if (length(shown) == 0 && hidden_count == 0L) {
    return(invisible(NULL))
  }

  lines <- vapply(shown, .format_advice_line, character(1), level = level)
  if (hidden_count > 0L) {
    noun <- if (hidden_count == 1L) "note" else "notes"
    lines <- c(
      lines,
      sprintf(
        paste0(
          "%d additional methodological %s hidden. Run ",
          "simtablr_guidance(\"teaching\") separately before printing ",
          "to show all advice."
        ),
        hidden_count,
        noun
      )
    )
  } else {
    lines <- c(
      lines,
      'Run simtablr_guidance("off") separately before printing to hide advice.'
    )
  }
  has_warning <- any(vapply(shown, function(entry) {
    (entry$severity %||% 0L) >= 3L
  }, logical(1)))
  .emit_advice_callout(lines, warning = has_warning)
  invisible(NULL)
}

#########
#AUDIT FACILITY
# transparent inspection of which rules fired, passed silently or were evaluated against the dataset

#' Audit a computed result: run all applicable rules, ignoring the guidance
#' dial, and return a structured report of fired and silent checks.
#' @keywords internal
#' @noRd
.audit_result <- function(result) {
  result <- validate_simtab_result(result)
  rows_checked <- list()
  fired <- list()
  silent <- list()

  for (rule in .registered_rules()) {
    applies <- tryCatch(isTRUE(rule$applies(result)), error = function(e) FALSE)
    if (!applies) {
      next
    }
    rows_checked[[length(rows_checked) + 1]] <- rule
    entries <- tryCatch(
      .normalise_advice_entries(rule$check(result), rule),
      error = function(e) list()
    )
    entries <- .dedupe_advice(entries)
    if (length(entries) > 0) {
      fired <- c(fired, entries)
    } else {
      silent[[length(silent) + 1]] <- rule
    }
  }

  fired_df <- .advice_df(.dedupe_advice(fired))
  silent_df <- .rules_df(silent)
  checked_df <- .rules_df(rows_checked)
  structure(
    list(
      result_class = class(result),
      ruleset_version = result$meta$ruleset_version %||% .simtab_ruleset_version(),
      checked = checked_df,
      fired = fired_df,
      silent = silent_df,
      summary = list(
        n_checked = nrow(checked_df),
        n_fired = nrow(fired_df),
        n_silent = nrow(silent_df)
      )
    ),
    class = "simtab_audit"
  )
}

#' Converts a list of advice entries into a structured data.frame
#' @keywords internal
#' @noRd
.advice_df <- function(advice) {
  if (length(advice) == 0) {
    return(data.frame(
      id = character(), severity = integer(), message = character(),
      citation = character(), fix = character(), version = character()
    ))
  }
  advice <- .order_advice_by_severity(.dedupe_advice(advice))
  out <- do.call(rbind, lapply(advice, function(entry) {
    data.frame(
      id = entry$id,
      severity = entry$severity,
      message = entry$message,
      citation = entry$citation %||% "",
      fix = entry$fix %||% "",
      version = entry$version %||% ""
    )
  }))
  rownames(out) <- NULL
  out
}

#' Converts a list of rule definitions into a structured data.frame
#' @keywords internal
#' @noRd
.rules_df <- function(rules) {
  if (length(rules) == 0) {
    return(data.frame(
      id = character(), severity = integer(), citation = character(),
      fix = character(), version = character()
    ))
  }
  out <- do.call(rbind, lapply(rules, function(rule) {
    data.frame(
      id = rule$id,
      severity = rule$severity,
      citation = rule$citation,
      fix = rule$fix,
      version = rule$version
    )
  }))
  rownames(out) <- NULL
  out
}

#' Coerce a SimtablR audit to a data.frame
#'
#' Returns the checked-rules table (`$checked`) with a `fired` logical column
#' joined on, so the fired/silent distinction shown by `print()` is available
#' as a single rectangular table.
#'
#' @param x A `simtab_audit`.
#' @param row.names,optional,... Ignored; accepted for S3 consistency.
#' @return A data.frame with one row per checked rule.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' as.data.frame(advise(res, audit = TRUE))
#' @export
as.data.frame.simtab_audit <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- x$checked
  out$fired <- out$id %in% x$fired$id
  out
}

#' Helper to wrap terminal text with uniform indentation
#' @keywords internal
#' @noRd
.wrap_labelled_field <- function(label, value, width, indent = 4L) {
  prefix <- sprintf("%s%s: ", strrep(" ", indent), label)
  if (is.null(value) || !nzchar(value)) {
    cat(prefix, "\n", sep = "")
    return(invisible(NULL))
  }
  cont_prefix <- strrep(" ", nchar(prefix))
  avail <- max(width - nchar(prefix), 20L)
  wrapped <- strwrap(value, width = avail)
  if (length(wrapped) == 0) {
    wrapped <- ""
  }
  cat(prefix, wrapped[[1]], "\n", sep = "")
  if (length(wrapped) > 1) {
    for (ln in wrapped[-1]) {
      cat(cont_prefix, ln, "\n", sep = "")
    }
  }
  invisible(NULL)
}


#' Custom console printer for simtab_audit objects
#' @noRd
#' @export
print.simtab_audit <- function(x, ...) {
  width <- getOption("width", 80L)
  cat("SimtablR audit\n")
  cat("Ruleset version: ", x$ruleset_version, "\n", sep = "")
  cat("Checked rules: ", x$summary$n_checked, "\n", sep = "")
  cat("Fired: ", x$summary$n_fired, " | Silent: ", x$summary$n_silent, "\n", sep = "")
  if (nrow(x$fired) > 0) {
    cat("\nFired rules:\n")
    for (i in seq_len(nrow(x$fired))) {
      cat("  ID: ", x$fired$id[i], "\n", sep = "")
      cat("    Severity: ", x$fired$severity[i], "\n", sep = "")
      .wrap_labelled_field("Message", x$fired$message[i], width)
      .wrap_labelled_field("Citation", x$fired$citation[i], width)
      .wrap_labelled_field("Fix", x$fired$fix[i], width)
    }
  }
  if (nrow(x$silent) > 0) {
    cat("\nSilent rules:\n")
    writeLines(strwrap(paste(x$silent$id, collapse = ", "), width = width, indent = 2, exdent = 2))
  }
  invisible(x)
}

#########
#DATA INSPECTION, STATISTICAL SENTINELS
# for methodological issues (sparse data, non-convergence, missingness, etc.)

#' Checks if the result object is a table representation like table 1 or contingency tb
#' @keywords internal
#' @noRd
.is_table_result <- function(result) {
  inherits(result, "simtab_table1") || inherits(result, "simtab_tb")
}

#' Computes event/outcome prevalence proportion from contingency table or baseline table
#' @keywords internal
#' @noRd
.outcome_prevalence <- function(result) {
  if (inherits(result, "simtab_tb") && !is.null(result$data$frequencies) &&
      length(dim(result$data$frequencies)) == 2) {
    tab <- result$data$frequencies
    return(sum(tab[, ncol(tab)], na.rm = TRUE) / sum(tab, na.rm = TRUE))
  }
  if (inherits(result, "simtab_table1") && !is.null(result$meta$strat_var) &&
      !is.null(result$meta$event_level)) {
    n_event <- result$meta$group_n[[result$meta$event_level]]
    n_total <- result$meta$n_total %||% sum(result$meta$group_n)
    if (!is.null(n_event) && n_total > 0) {
      return(as.numeric(n_event) / n_total)
    }
  }
  NA_real_
}

#' Checks whether a specific effect measure (e.g. "OR", "RR", "RD") was requested/computed
#' @keywords internal
#' @noRd
.result_uses_measure <- function(result, measure) {
  measure <- toupper(measure)
  if (!is.null(result$meta$effect) && toupper(result$meta$effect) %in% measure) {
    return(TRUE)
  }
  if (!is.null(result$spec$effect$measure) &&
      toupper(result$spec$effect$measure) %in% measure) {
    return(TRUE)
  }
  ratios <- result$data$ratios
  if (is.data.frame(ratios) && "type" %in% names(ratios)) {
    return(any(toupper(ratios$type) %in% measure, na.rm = TRUE))
  }
  FALSE
}

#' Checks if any hypothesis test was executed on continuous variables
#' @keywords internal
#' @noRd
.any_continuous_test <- function(result) {
  if (inherits(result, "simtab_tb")) {
    return(isTRUE(result$meta$is_continuous) && !is.null(result$meta$stats))
  }
  if (inherits(result, "simtab_table1")) {
    for (rec in result$data) {
      if (identical(rec$type, "continuous") && !is.null(rec$test)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Generates stratified synthetic 2x2 data for testing Mantel-Haenszel rules
#' @keywords internal
#' @noRd
.mh_rule_example_data <- function(strata) {
  do.call(rbind, lapply(names(strata), function(stratum_name) {
    tab <- strata[[stratum_name]]
    data.frame(
      exposure = factor(
        c(rep("index", sum(tab[1, ])), rep("ref", sum(tab[2, ]))),
        levels = c("ref", "index")
      ),
      outcome = factor(
        c(rep("Yes", tab[1, 1]), rep("No", tab[1, 2]),
          rep("Yes", tab[2, 1]), rep("No", tab[2, 2])),
        levels = c("No", "Yes")
      ),
      stratum = factor(stratum_name, levels = names(strata))
    )
  }))
}

#' Extracts all available p-values across table tests
#' @keywords internal
#' @noRd
.result_test_p_values <- function(result) {
  if (inherits(result, "simtab_table1")) {
    vals <- vapply(result$data, function(rec) {
      if (is.null(rec$test)) {
        return(NA_real_)
      }
      rec$test$p.value
    }, numeric(1))
    return(vals[!is.na(vals)])
  }
  if (inherits(result, "simtab_tb")) {
    if (is.data.frame(result$data$tests) && "p_value" %in% names(result$data$tests)) {
      return(result$data$tests$p_value[!is.na(result$data$tests$p_value)])
    }
    if (!is.null(result$meta$stats) && !is.na(result$meta$stats$p.value)) {
      return(result$meta$stats$p.value)
    }
  }
  numeric(0)
}

#' Extracts Standardized Mean Difference (SMD) values from baseline tables
#' @keywords internal
#' @noRd
.result_smd_values <- function(result) {
  if (!inherits(result, "simtab_table1")) {
    return(numeric(0))
  }
  vals <- vapply(result$data, function(rec) {
    smd <- rec$smd %||% NA_real_
    if (length(smd) != 1) NA_real_ else as.numeric(smd)
  }, numeric(1))
  vals[!is.na(vals)]
}

#' Detects the fraction of missing values when missingness display was suppressed
#' @keywords internal
#' @noRd
.missing_fraction_unreported <- function(result) {
  if (inherits(result, "simtab_table1")) {
    if (.result_missing_display(result)) {
      return(0)
    }
    totals <- vapply(result$data, function(rec) sum(rec$n_missing, na.rm = TRUE), numeric(1))
    denom <- result$meta$n_total %||% result$used$nrow %||% NA_real_
    if (is.na(denom) || denom <= 0) {
      return(0)
    }
    return(max(totals / denom, na.rm = TRUE))
  }
  if (inherits(result, "simtab_tb")) {
    if (isTRUE(result$meta$flags$missing)) {
      return(0)
    }
    data <- result$used$ref$data
    vars <- c(result$meta$row_var_name, result$meta$col_var_name)
    vars <- vars[!is.na(vars) & vars %in% names(data)]
    if (length(vars) == 0 || nrow(data) == 0) {
      return(0)
    }
    dropped <- !stats::complete.cases(data[, vars, drop = FALSE])
    return(mean(dropped))
  }
  0
}

#' Checks if missing count rows/columns are explicitly rendered in the output
#' impotant for stad and the like.
#' @keywords internal
#' @noRd
.result_missing_display <- function(result) {
  missing <- result$meta$missing
  if (is.list(missing)) {
    return(isTRUE(missing$display))
  }
  isTRUE(missing)
}

#' Retrieves the descriptive sample size available before model filtering
#' @keywords internal
#' @noRd
.result_descriptive_available_n <- function(result) {
  if (inherits(result, "simtab_table1")) {
    return(as.integer(result$meta$n_available %||% result$used$nrow %||% result$meta$n_total))
  }
  if (inherits(result, "simtab_regtab")) {
    return(as.integer(result$used$nrow %||% nrow(result$used$ref$data)))
  }
  NA_integer_
}

#' Retrieves sample size used by fitted regression models
#' @keywords internal
#' @noRd
.result_model_n <- function(result) {
  n <- result$meta$model_n %||% NULL
  if (is.null(n)) {
    return(integer(0))
  }
  n <- as.integer(n)
  n[!is.na(n)]
}

#' Flags variables where a log-binomial model failed and fell back to modified Poisson
#' @keywords internal
#' @noRd
.logbinomial_fallback_vars <- function(result) {
  flags <- result$meta$logbinomial_fallback %||% NULL
  if (!is.null(flags)) {
    flag_names <- names(flags)
    flags <- as.logical(flags)
    names(flags) <- flag_names
    vars <- names(flags)[!is.na(flags) & flags]
    return(vars[nzchar(vars)])
  }

  vars <- names(result$data)
  vars[vapply(result$data, function(rec) isTRUE(rec$logbinomial_fallback), logical(1))]
}

#' Distinguishes whether log-binomial fitting failed to start vs failed to converge#'
#' Prevents misleading the user about why a fallback estimator was chosen.
#'
#' @param result A computed table result.
#' @param vars Variables whose adjusted estimate fell back to modified Poisson.
#' @return A single sentence fragment naming the reason.
#' @keywords internal
#' @noRd
.logbinomial_reason_phrase <- function(result, vars) {
  statuses <- result$meta$logbinomial_status %||% NULL
  statuses <- if (is.null(statuses)) {
    vapply(
      result$data[vars],
      function(rec) as.character(rec$logbinomial_status %||% NA_character_),
      character(1)
    )
  } else {
    as.character(statuses[vars])
  }
  statuses <- unique(statuses[!is.na(statuses)])

  fitted_failed <- "the log-binomial model could not be fitted"
  not_converged <- "the log-binomial model did not converge"

  if (length(statuses) == 0) {
    return(not_converged)
  }
  if (setequal(statuses, "failed_to_fit")) {
    return(fitted_failed)
  }
  if (setequal(statuses, "not_converged")) {
    return(not_converged)
  }
  paste(not_converged, "or could not be fitted")
}

#' Checks for discrepancy between descriptive sample size and model complete-case sample size
#' @keywords internal
#' @noRd
.n_mismatch_complete_case <- function(result, threshold = 0.05) {
  available_n <- .result_descriptive_available_n(result)
  model_n <- .result_model_n(result)
  if (length(model_n) == 0 || is.na(available_n) || available_n <= 0) {
    return(NULL)
  }
  min_model_n <- min(model_n)
  dropped <- available_n - min_model_n
  frac <- dropped / available_n
  if (is.na(frac) || frac <= threshold) {
    return(NULL)
  }
  list(
    severity = if (frac > 0.10) 3L else 2L,
    message = sprintf(
      "Adjusted/model-based estimates use N=%d complete cases, smaller than the descriptive available N=%d (%.1f%% dropped).",
      min_model_n,
      available_n,
      frac * 100
    ),
    why = "Listwise model fitting can silently change the analysed cohort relative to the descriptive table."
  )
}

#' Identifies if a regression table is running a logistic regression model
#' @keywords internal
#' @noRd
.regtab_is_logistic <- function(result) {
  inherits(result, "simtab_regtab") &&
    identical(result$meta$family, "binomial") &&
    identical(result$meta$link, "logit")
}

#' Helper to extract the minority event count for binary/categorical outcomes
#' @keywords internal
#' @noRd
.outcome_event_count <- function(y) {
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    fy <- droplevels(factor(y))
    if (nlevels(fy) < 2) {
      return(0)
    }
    return(min(table(fy)))
  }
  vals <- y[!is.na(y)]
  if (length(vals) == 0) {
    return(0)
  }
  if (all(vals %in% c(0, 1))) {
    return(min(sum(vals == 1), sum(vals == 0)))
  }
  sum(vals != 0)
}


#' Calculates Events Per Variable (EPV) for logistic regression models
#' uses EPV >= 10 to prevent model overfitting.
#' @keywords internal
#' @noRd
.regtab_epv <- function(result) {
  data <- result$used$ref$data
  outcomes <- result$meta$outcomes %||% character()
  params <- length(result$meta$term_order) + 1L
  if (params <= 0 || length(outcomes) == 0) {
    return(numeric())
  }
  stats::setNames(
    vapply(outcomes, function(outcome) {
      if (!outcome %in% names(data)) {
        return(NA_real_)
      }
      .outcome_event_count(data[[outcome]]) / params
    }, numeric(1)),
    outcomes
  )
}

#' Extracts unique Variance Inflation Factor (VIF) rows to identify multicollinearity
#' @keywords internal
#' @noRd
.regtab_vif_rows <- function(result) {
  if (!inherits(result, "simtab_regtab") || !"vif" %in% names(result$data)) {
    return(data.frame())
  }
  rows <- result$data[!is.na(result$data$vif), c("outcome", "vif_term", "gvif", "vif_df", "gvif_adjusted", "vif"), drop = FALSE]
  if (nrow(rows) == 0) {
    return(rows)
  }
  rows[!duplicated(rows[c("outcome", "vif_term")]), , drop = FALSE]
}

#' Extracts minimum class size in ROC analyses
#' @keywords internal
#' @noRd
.roc_min_class_n <- function(result) {
  flag <- result$meta$rule_flags$min_class_n %||% NA_real_
  if (!is.na(flag)) {
    return(as.numeric(flag))
  }
  auc <- result$data$auc
  if (!is.data.frame(auc) || !all(c("n_pos", "n_neg") %in% names(auc))) {
    return(NA_real_)
  }
  min(auc$n_pos, auc$n_neg, na.rm = TRUE)
}

#' Synthetic data generator for ROC analysis test fixtures
#' @keywords internal
#' @noRd
.roc_example_data <- function(n_pos = 40, n_neg = 40, markers = 1) {
  n <- n_pos + n_neg
  outcome <- factor(c(rep("No", n_neg), rep("Yes", n_pos)), levels = c("No", "Yes"))
  out <- data.frame(outcome = outcome)
  for (i in seq_len(markers)) {
    shift <- 0.55 + (i * 0.15)
    out[[paste0("marker", i)]] <- c(
      stats::qnorm((seq_len(n_neg) - 0.5) / n_neg),
      stats::qnorm((seq_len(n_pos) - 0.5) / n_pos) + shift
    )
  }
  out
}

#' Synthetic data generator for testing log-binomial convergence and fallback rules
#' @keywords internal
#' @noRd
.logbinomial_rule_example_data <- function(kind = c("hard", "clean")) {
  kind <- match.arg(kind)
  set.seed(1)
  n <- 160
  exposure <- factor(rep(c("ref", "exp"), each = n / 2), levels = c("ref", "exp"))
  if (identical(kind, "hard")) {
    x <- rep(seq(-3, 3, length.out = n / 2), 2)
    eta <- 0.3 + 0.8 * (exposure == "exp") + 1.0 * x
  } else {
    x <- rep(seq(-2, 2, length.out = n / 2), 2)
    eta <- -1.4 + 0.25 * (exposure == "exp") + 0.12 * x
  }
  y <- stats::rbinom(n, size = 1, prob = stats::plogis(eta))
  data.frame(
    y = factor(ifelse(y == 1, "Yes", "No"), levels = c("No", "Yes")),
    exposure = exposure,
    x = x
  )
}

#' Identifies variables whose adjusted models failed numerical convergence
#' @keywords internal
#' @noRd
.effect_nonconverged_vars <- function(result) {
  flags <- result$meta$effect_converged %||% NULL
  if (!is.null(flags)) {
    flag_names <- names(flags)
    flags <- as.logical(flags)
    names(flags) <- flag_names
    vars <- names(flags)[!is.na(flags) & !flags]
    return(vars[nzchar(vars)])
  }

  vars <- names(result$data)
  vars[vapply(
    result$data,
    function(rec) isFALSE(rec$effect_converged) && !is.null(rec$adjusted),
    logical(1)
  )]
}

#' Deterministic separated for the convergence rule.
#' @keywords internal
#' @noRd
.nonconvergence_rule_example_data <- function(kind = c("separated", "clean")) {
  kind <- match.arg(kind)
  n <- 120
  exposure <- factor(rep(c("ref", "exp"), each = n / 2), levels = c("ref", "exp"))
  y <- if (identical(kind, "separated")) {
    # Exposure predicts the outcome perfectly: maximum likelihood diverges.
    rep(c("No", "Yes"), each = n / 2)
  } else {
    rep(c("No", "Yes", "Yes", "No"), length.out = n)
  }
  data.frame(
    y = factor(y, levels = c("No", "Yes")),
    exposure = exposure,
    x = rep(seq(-2, 2, length.out = n / 4), 4)
  )
}

#' contingency table fixtures with sparse cells to test Campbell rule requirements
#' @keywords internal
#' @noRd
.campbell_rule_example_data <- function(kind = c("low", "small", "ok")) {
  kind <- match.arg(kind)
  tab <- switch(
    kind,
    low = matrix(c(0, 3, 3, 4), nrow = 2, byrow = TRUE),
    small = matrix(c(0, 49, 10, 41), nrow = 2, byrow = TRUE),
    ok = matrix(c(0, 10, 50, 40), nrow = 2, byrow = TRUE)
  )
  data.frame(
    exp = factor(rep(rep(c("A", "B"), each = 2), as.vector(t(tab))), levels = c("A", "B")),
    out = factor(rep(rep(c("no", "yes"), 2), as.vector(t(tab))), levels = c("no", "yes"))
  )
}

#' Extracts internal test footnotes/annotations by ID
#' @keywords internal
#' @noRd
.result_test_notes <- function(result, id = NULL) {
  notes <- result$meta$test_notes %||% list()
  if (length(notes) == 0) {
    return(list())
  }
  if (is.null(id)) {
    return(notes)
  }
  notes[vapply(notes, function(note) identical(note$id, id), logical(1))]
}

#' Retrieves automated summary choices recorded in metadata
#' @keywords internal
#' @noRd
.result_summary_auto <- function(result) {
  result$meta$summary_auto %||% list()
}

#' Extracts list of variable names described in table1 or tb objects
#' @keywords internal
#' @noRd
.result_described_vars <- function(result) {
  if (inherits(result, "simtab_table1")) {
    return(names(result$data))
  }
  if (inherits(result, "simtab_tb")) {
    v <- result$meta$row_var_name
    if (is.null(v) || is.na(v) || !nzchar(v)) {
      return(character())
    }
    return(v)
  }
  character()
}

#' Identifies variable record type ('continuous' or 'categorical')
#' @keywords internal
#' @noRd
.result_var_record_type <- function(result, v) {
  if (inherits(result, "simtab_table1")) {
    return(result$data[[v]]$type %||% NA_character_)
  }
  if (inherits(result, "simtab_tb") && identical(v, result$meta$row_var_name)) {
    return(if (isTRUE(result$meta$is_continuous)) "continuous" else "categorical")
  }
  NA_character_
}

#' Extracts user-requested summary statistic
#' @keywords internal
#' @noRd
.result_var_stat_requested <- function(result, v) {
  if (inherits(result, "simtab_table1")) {
    return(result$data[[v]]$stat_requested %||% NA_character_)
  }
  if (inherits(result, "simtab_tb") && identical(v, result$meta$row_var_name)) {
    return(result$meta$stat.cont_requested %||% NA_character_)
  }
  NA_character_
}

#' Fetches raw column vector from the original dataset stored in result reference
#' @keywords internal
#' @noRd
.result_raw_column <- function(result, v) {
  data <- result$used$ref$data
  if (is.null(data) || !v %in% names(data)) {
    return(NULL)
  }
  data[[v]]
}

#' Sentinel: Flags columns that look like unique ID numbers or subject identifiers (N > 20 and 100% unique values)
#' @keywords internal
#' @noRd
.id_like_flagged_vars <- function(result) {
  Filter(function(v) {
    x <- .result_raw_column(result, v)
    if (is.null(x)) {
      return(FALSE)
    }
    xc <- x[!is.na(x)]
    n <- length(xc)
    n > 20 && length(unique(xc)) == n
  }, .result_described_vars(result))
}

#' Sentinel: Flags columns with zero variance (all non-missing values are identical)
#' @keywords internal
#' @noRd
.constant_flagged_vars <- function(result) {
  Filter(function(v) {
    x <- .result_raw_column(result, v)
    if (is.null(x)) {
      return(FALSE)
    }
    xc <- x[!is.na(x)]
    length(xc) > 0 && length(unique(xc)) == 1
  }, .result_described_vars(result))
}

#' Sentinel: Flags categorical variables with excessive cardinality (> 15 categories)
#' @keywords internal
#' @noRd
.high_cardinality_flagged_vars <- function(result) {
  Filter(function(v) {
    if (!identical(.result_var_record_type(result, v), "categorical")) {
      return(FALSE)
    }
    x <- .result_raw_column(result, v)
    if (is.null(x)) {
      return(FALSE)
    }
    length(unique(x[!is.na(x)])) > 15
  }, .result_described_vars(result))
}

#' Sentinel: Flags discrete/ordinal variables (<= 7 integer values) summarized as continuous
#' @keywords internal
#' @noRd
.ordinal_as_continuous_flagged_vars <- function(result) {
  Filter(function(v) {
    if (!identical(.result_var_record_type(result, v), "continuous")) {
      return(FALSE)
    }
    x <- .result_raw_column(result, v)
    if (is.null(x) || !is.numeric(x)) {
      return(FALSE)
    }
    xc <- x[!is.na(x)]
    # A tiny sample trivially has few unique values regardless of scale; require
    # enough observations that low cardinality reflects the variable's nature.
    if (length(xc) < 20 || any(xc != round(xc))) {
      return(FALSE)
    }
    length(unique(xc)) <= 7
  }, .result_described_vars(result))
}

#' Sentinel: Flags Date/POSIXt columns included in descriptive tables
#' @keywords internal
#' @noRd
.date_column_flagged_vars <- function(result) {
  Filter(function(v) {
    x <- .result_raw_column(result, v)
    !is.null(x) && (inherits(x, "Date") || inherits(x, "POSIXt"))
  }, .result_described_vars(result))
}

#' Sentinel: Flags repeated subject identifiers indicating longitudinal/clustered structure
#' @keywords internal
#' @noRd
.repeated_id_columns <- function(data) {
  if (!is.data.frame(data)) {
    return(character())
  }
  nms <- names(data)
  candidates <- nms[grepl("^(id|subject|patient|record)", nms, ignore.case = TRUE)]
  Filter(function(v) {
    x <- data[[v]]
    x <- x[!is.na(x)]
    length(x) > 0 && anyDuplicated(x) > 0
  }, candidates)
}

#' Checks if an unpaired statistical test was performed when paired data might exist
#' @keywords internal
#' @noRd
.result_has_unpaired_test_or_effect <- function(result) {
  if (isTRUE(result$spec$comparison$paired)) {
    return(FALSE)
  }
  has_test <- isTRUE(result$meta$has_test) || !is.null(result$meta$stats)
  has_effect <- !is.null(result$spec$effect$measure)
  has_test || has_effect
}

#' Sentinel: Detects overdispersed Poisson models (> 1.5) that omitted robust standard errors
#' @keywords internal
#' @noRd
.regtab_overdispersed_rows <- function(result) {
  info <- result$meta$model_info
  if (!is.data.frame(info) || !all(c("dispersion", "family", "robust", "failed") %in% names(info))) {
    return(info[0, , drop = FALSE])
  }
  info[
    !info$failed & !is.na(info$dispersion) &
      info$family == "poisson" & info$dispersion > 1.5 & !info$robust,
    ,
    drop = FALSE
  ]
}

#' Sentinel: Flags HC0 robust standard errors fitted with small samples (N < 100)
#' @keywords internal
#' @noRd
.regtab_hc0_small_n_rows <- function(result) {
  info <- result$meta$model_info
  if (!is.data.frame(info) || !all(c("vcov", "n", "failed") %in% names(info))) {
    return(info[0, , drop = FALSE])
  }
  info[!info$failed & info$vcov == "HC0" & !is.na(info$n) & info$n < 100, , drop = FALSE]
}

#' Sentinel: Flags continuous variables forced to report mean/SD when data is skewed
#' @keywords internal
#' @noRd
.forced_mean_skewed_flagged_vars <- function(result) {
  Filter(function(v) {
    if (!identical(.result_var_record_type(result, v), "continuous")) {
      return(FALSE)
    }
    if (!identical(.result_var_stat_requested(result, v), "mean")) {
      return(FALSE)
    }
    x <- .result_raw_column(result, v)
    if (is.null(x) || !is.numeric(x)) {
      return(FALSE)
    }
    identical(.auto_summary_decision(x)$decision, "median")
  }, .result_described_vars(result))
}

#' Extracts column group denominators for percentage calculations
#' @keywords internal
#' @noRd
.result_pct_denominators <- function(result) {
  if (inherits(result, "simtab_table1")) {
    denoms <- unlist(lapply(result$data, function(rec) {
      if (!identical(rec$type, "categorical") || is.null(rec$freq)) {
        return(numeric(0))
      }
      unname(colSums(rec$freq))
    }))
    return(denoms[!is.na(denoms)])
  }
  if (inherits(result, "simtab_tb") && !isTRUE(result$meta$is_continuous)) {
    tab <- result$data$frequencies
    if (is.null(tab)) {
      return(numeric(0))
    }
    if (length(dim(tab)) == 2) {
      return(unname(colSums(tab)))
    }
    return(sum(tab))
  }
  numeric(0)
}

#' Sentinel: Flags percentages computed over tiny sub-sample denominators (0 < N < 10)
#' @keywords internal
#' @noRd
.tiny_denominator_flag <- function(result) {
  d <- .result_pct_denominators(result)
  tiny <- d[d > 0 & d < 10]
  if (length(tiny) == 0 || !any(d >= 20)) {
    return(NA_real_)
  }
  min(tiny)
}

#' Initializes and registers default built-in educator rules into the environment
#' @keywords internal
#' @noRd
.seed_builtin_rules <- function() {
  version <- .simtab_ruleset_version()
  .register_core_advice_rules(version)
  invisible(TRUE)
}


#' Minimal dataset fixture for testing sensitivity analysis rules
#' @keywords internal
#' @noRd
.sensitivity_rule_example_data <- function() {
  data.frame(
    exposure = factor(rep(c("No", "Yes"), each = 40), levels = c("No", "Yes")),
    outcome = factor(
      c(rep("No", 28), rep("Yes", 12), rep("No", 18), rep("Yes", 22)),
      levels = c("No", "Yes")
    ),
    age = seq_len(80)
  )
}

