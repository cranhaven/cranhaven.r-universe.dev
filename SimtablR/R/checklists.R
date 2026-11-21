# REPORTING GUIDELINE CHECKLISTS (STROBE AND STARD)
# Cross-references computed SimtablR results, metadata, participant flow, and advisory
# notices against international reporting guidelines (STROBE for observational studies; STARD for diagnostic accuracy).

#########
# STROBE OBSERVATIONAL STUDY CHECKLIST
# Evaluates STROBE items against recorded study design, sample sizes, and methods.

#' Build a STROBE reporting checklist
#'
#' @param x A SimtablR result, report, or spec.
#' @param ... Ignored.
#' @return A `simtab_checklist` object.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' strobe(res)
#' @export
strobe <- function(x, ...) {
  UseMethod("strobe")
}

#' @export
strobe.default <- function(x, ...) {
  simtab_abort_spec(c(
    "{.fn strobe} requires a SimtablR result, report, or spec.",
    "i" = "STROBE coverage is derived from recorded SimtablR analysis metadata.",
    "v" = "Pass an object returned by {.fn table1}, {.fn tb}, {.fn regtab}, or {.fn simtablr}."
  ))
}

#' @export
strobe.simtab_spec <- function(x, ...) {
  strobe(evaluate(x), ...)
}

#' @export
strobe.simtab_result <- function(x, ...) {
  .strobe_from_items(list(result = validate_simtab_result(x)))
}

#' @export
strobe.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  .strobe_from_items(x$items, report = x)
}

#' Map recorded report item metadata to STROBE guideline items
#' @keywords internal
#' @noRd
.strobe_from_items <- function(items, report = NULL) {
  df <- .strobe_items()
  object <- report %||% items[[1]]
  has_design <- any(vapply(items, function(item) !is.null(.result_design(item)), logical(1)))
  has_data <- !is.null(object$used %||% items[[1]]$used)
  has_methods <- nzchar(as_methods(object))
  has_flow <- tryCatch(nrow(as.data.frame(flow(object))) > 2, error = function(e) FALSE)
  has_result <- length(items) > 0

  if (has_design) {
    df <- .checklist_cover(df, "4", "Recorded study design metadata.")
  }
  if (has_data) {
    df <- .checklist_cover(df, "6", "Captured SimtablR source-data row count and variables.")
    df <- .checklist_cover(df, "7", "See codebook(x) for recorded variables.")
    df <- .checklist_cover(df, "10", "Source and rendered Ns are recorded by SimtablR.")
    df <- .checklist_cover(df, "14", "See codebook(x) and table output for descriptive data and missingness.")
  }
  if (has_methods) {
    df <- .checklist_cover(df, "12", "See as_methods(x).")
  }
  if (has_flow) {
    df <- .checklist_cover(df, "13", "See as.data.frame(flow(x)).")
  }
  if (has_result) {
    df <- .checklist_cover(df, "16", "See rendered SimtablR estimates and confidence intervals.")
  }
  if (inherits(object, "simtab_sensitivity")) {
    df <- .checklist_cover(df, "17", "Sensitivity-analysis output is recorded in this report.")
  }

  .checklist_apply_advice(df, object, family = "strobe")
}

#########
# STARD DIAGNOSTIC ACCURACY CHECKLIST
# Evaluates STARD items against index tests, reference standards, and cutpoints.

#' Build a STARD reporting checklist
#'
#' @param x A SimtablR diagnostic or ROC result, report, or spec.
#' @param ... Ignored.
#' @return A `simtab_checklist` object.
#' @examples
#' d <- diag_test(epitabl, poc_hstn_positive, adjudicated_acs,
#'                positive = "Yes", test_positive = "Positive")
#' stard(d)
#' @export
stard <- function(x, ...) {
  UseMethod("stard")
}

#' @export
stard.default <- function(x, ...) {
  simtab_abort_spec(c(
    "{.fn stard} requires a SimtablR diagnostic or ROC result.",
    "i" = "STARD coverage is derived from recorded diagnostic-analysis metadata.",
    "v" = "Pass an object returned by {.fn diag_test} or {.fn roc}."
  ))
}

#' @export
stard.simtab_spec <- function(x, ...) {
  stard(evaluate(x), ...)
}

#' @export
stard.simtab_result <- function(x, ...) {
  .stard_from_items(list(result = validate_simtab_result(x)))
}

#' @export
stard.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  .stard_from_items(x$items, report = x)
}

#' Map recorded diagnostic metadata to STARD guideline items
#' @keywords internal
#' @noRd
.stard_from_items <- function(items, report = NULL) {
  df <- .stard_items()
  object <- report %||% items[[1]]
  has_diag <- any(vapply(items, function(item) inherits(item, "simtab_diag"), logical(1)))
  has_roc <- any(vapply(items, function(item) inherits(item, "simtab_roc"), logical(1)))
  has_flow <- tryCatch(nrow(as.data.frame(flow(object))) > 2, error = function(e) FALSE)

  if (has_diag || has_roc) {
    df <- .checklist_cover(df, "10", "Index test variables and analysis settings are recorded by SimtablR.")
    df <- .checklist_cover(df, "12", "Positive-test definitions and cutpoints are recorded in result metadata.")
    df <- .checklist_cover(df, "24", "Diagnostic accuracy estimates and confidence intervals are rendered by SimtablR.")
  }
  if (has_diag) {
    df <- .checklist_cover(df, "11", "Reference-standard variable and positive level are recorded in result metadata.")
  }
  if (has_flow) {
    df <- .checklist_cover(df, "19", "See as.data.frame(flow(x)).")
  }

  .checklist_apply_advice(df, object, family = "stard")
}

#########
# CHECKLIST RECORD DATA STRUCTURE AND S3 METHODS
# Constructors, formatting, and flextable output for reporting checklists.

#' Construct a simtab_checklist container object
#' @keywords internal
#' @noRd
.new_simtab_checklist <- function(df, guideline) {
  structure(
    list(guideline = guideline, data = df),
    class = c("simtab_checklist", "simtab")
  )
}

#' @export
as.data.frame.simtab_checklist <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$data
}

#' @export
print.simtab_checklist <- function(x, ...) {
  cat("<simtab_checklist>\n")
  cat("  guideline: ", x$guideline, "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
  invisible(x)
}

#' @rdname strobe
#' @param x A `simtab_checklist` object.
#' @exportS3Method flextable::as_flextable
as_flextable.simtab_checklist <- function(x, ...) {
  .require_pkg("flextable")
  simtab_theme(flextable::flextable(as.data.frame(x), ...))
}

#########
# CHECKLIST ITEM DEFINITIONS AND ADVICE INTEGRATION
# Defines standard checklist requirement rosters and integrates active advisory notices.

#' Mark a guideline checklist item as covered by computed output
#' @keywords internal
#' @noRd
.checklist_cover <- function(df, item, pointer) {
  idx <- df$item == item
  df$status[idx] <- "covered-by-output"
  df$pointer[idx] <- pointer
  df
}

#' Attach advisory pointer to a guideline checklist item
#' @keywords internal
#' @noRd
.checklist_advise <- function(df, item, pointer) {
  idx <- df$item == item & df$status != "covered-by-output"
  df$status[idx] <- "advised"
  df$pointer[idx] <- pointer
  df
}

#' Cross-reference active analysis advice against guideline checklist items
#' @keywords internal
#' @noRd
.checklist_apply_advice <- function(df, object, family) {
  advice <- object$advice %||% list()
  if (length(advice) == 0) {
    return(.new_simtab_checklist(df, toupper(family)))
  }
  rules <- .registered_rules()
  rule_map <- stats::setNames(rules, vapply(rules, `[[`, character(1), "id"))
  for (entry in advice) {
    rule <- rule_map[[entry$id %||% ""]] %||% NULL
    checklist <- rule$checklist %||% character()
    if (length(checklist) == 0 || !family %in% names(checklist)) {
      next
    }
    item <- unname(checklist[[family]])
    df <- .checklist_advise(df, item, paste("Advice:", entry$id))
  }
  .new_simtab_checklist(df, toupper(family))
}

#' Return the 22-item STROBE statement checklist structure
#' @keywords internal
#' @noRd
.strobe_items <- function() {
  data <- data.frame(
    item = as.character(1:22),
    requirement = c(
      "Title and abstract",
      "Background and rationale",
      "Objectives",
      "Study design",
      "Setting",
      "Participants",
      "Variables",
      "Data sources and measurement",
      "Bias",
      "Study size",
      "Quantitative variables",
      "Statistical methods",
      "Participants and flow",
      "Descriptive data",
      "Outcome data",
      "Main results",
      "Other analyses",
      "Key results",
      "Limitations",
      "Interpretation",
      "Generalisability",
      "Funding"
    ),
    status = "not-assessed",
    pointer = ""
  )
  data
}

#' Return the 30-item STARD statement checklist structure
#' @keywords internal
#' @noRd
.stard_items <- function() {
  data.frame(
    item = as.character(1:30),
    requirement = c(
      "Identification as diagnostic accuracy study",
      "Structured summary",
      "Scientific and clinical background",
      "Study objectives",
      "Study design",
      "Eligibility criteria",
      "Participant selection",
      "Participant recruitment",
      "Study setting",
      "Index test methods",
      "Reference standard",
      "Test positivity definitions",
      "Clinical information available",
      "Methods for estimating accuracy",
      "Handling indeterminate results",
      "Handling missing data",
      "Sample size",
      "Flow of participants",
      "Participant flow diagram",
      "Baseline demographic and clinical characteristics",
      "Distribution of disease severity",
      "Time interval between tests",
      "Cross tabulation of results",
      "Diagnostic accuracy estimates",
      "Adverse events",
      "Study limitations",
      "Implications for practice",
      "Registration number",
      "Protocol access",
      "Funding sources"
    ),
    status = "not-assessed",
    pointer = ""
  )
}
