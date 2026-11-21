# REPORT ORCHESTRATION AND MULTI-TABLE COMPOSITION
# Composes descriptive Table 1 and association Table 2 into a cohesive simtab_report
# while managing tidyselect variable binding, methods synthesis, and export dispatch.

#########
# REPORT ORCHESTRATION ENTRY POINT
# Coordinates variable selection and builds Table 1 and Table 2 compositions.

#' Build a manuscript-ready SimtablR report in one call
#'
#' `simtablr()` composes existing presets into a transparent `simtab_report`.
#' It creates a cohort-description Table 1 and an exposure-to-outcome Table 2
#' using the same `table1()` effect path as direct users. No new estimation is
#' performed by the orchestrator.
#'
#' @param data A data.frame.
#' @param outcome Bare column name for the outcome.
#' @param exposure Bare column name for the exposure.
#' @param vars Optional tidyselect expression of Table 1 variables. Defaults to
#'   all columns except `outcome` and `exposure`.
#' @param adjust Optional tidyselect expression of adjustment covariates for
#'   Table 2.
#' @param design Optional per-call study design used by the existing
#'   design-aware measure resolver.
#' @param test Logical or test name passed to `table1()`.
#' @param style Display style passed to `table1()`.
#' @param ... Additional `table1()` arguments. Effect-specific arguments such
#'   as `measure` are applied to Table 2 only.
#' @return A `simtab_report` with named `table1` and `table2` results.
#' @examples
#' \dontrun{
#' data(epitabl)
#' simtablr(epitabl, outcome = adjudicated_acs, exposure = smoking,
#'          vars = c(age, sex, smoking), adjust = c(age, sex),
#'          design = "cross_sectional")
#' }
#' @export
simtablr <- function(
  data,
  outcome,
  exposure,
  vars = NULL,
  adjust = NULL,
  design = NULL,
  test = TRUE,
  style = "default",
  ...
) {
  .check_data_frame(data)

  outcome_quo <- rlang::enquo(outcome)
  exposure_quo <- rlang::enquo(exposure)
  vars_quo <- rlang::enquo(vars)
  adjust_quo <- rlang::enquo(adjust)

  outcome_name <- .report_select_one(data, outcome_quo, "outcome")
  exposure_name <- .report_select_one(data, exposure_quo, "exposure")
  if (identical(outcome_name, exposure_name)) {
    simtab_abort_binding(c(
      "{.arg outcome} and {.arg exposure} must select different columns.",
      "i" = "Both resolved to {.val {outcome_name}}.",
      "v" = "Name the outcome and the exposure separately."
    ))
  }

  vars_names <- if (missing(vars) || .report_quo_is_null(vars_quo)) {
    setdiff(names(data), c(outcome_name, exposure_name))
  } else {
    .report_select_many(data, vars_quo, "vars")
  }
  if (length(vars_names) == 0) {
    simtab_abort_binding(c(
      "{.arg vars} must select at least one Table 1 column.",
      "i" = "After removing the outcome and exposure, no columns were left.",
      "v" = "Name the baseline characteristics explicitly."
    ))
  }

  adjust_names <- if (missing(adjust) || .report_quo_is_null(adjust_quo)) {
    NULL
  } else {
    .report_select_many(data, adjust_quo, "adjust")
  }

  dots <- list(...)
  has_requested_measure <- !is.null(dots$measure)
  has_available_design <- !is.null(design) || !is.null(.data_design(data))
  if (!is.null(adjust_names) && !has_requested_measure && !has_available_design) {
    simtab_abort_spec(c(
      "{.arg adjust} requires an effect measure.",
      "i" = "No effect measure can be resolved without a study design or explicit measure.",
      "v" = "Add {.code design =} or {.code measure =} when using {.code adjust =}."
    ))
  }
  table1_dots <- dots[!names(dots) %in% c("measure", "adjust", "design", "ref")]

  table1_args <- c(
    list(
      data = data,
      vars = vars_names,
      by = exposure_name,
      test = test,
      style = style
    ),
    table1_dots
  )
  cohort <- do.call(table1, table1_args)

  table2_args <- c(
    list(
      data = data,
      vars = exposure_name,
      by = outcome_name,
      test = test,
      style = style,
      design = design
    ),
    dots
  )
  if (!is.null(adjust_names)) {
    table2_args$adjust <- adjust_names
  }
  association <- do.call(table1, table2_args)

  used <- cohort$used
  items <- .share_report_data_ref(
    list(table1 = cohort, table2 = association),
    used
  )

  report <- new_simtab_report(
    items = items,
    methods = .report_methods(items),
    used = used
  )
  if (is.null(association$spec$effect$measure)) {
    report$advice <- c(report$advice, list(list(
      id = "report_no_effect_measure",
      severity = 2L,
      message = "No effect measure was included in Table 2.",
      why = "The outcome and exposure were tabulated, but SimtablR could not infer a design-appropriate association measure.",
      citation = "SimtablR analysis-plan guidance",
      fix = "Set design = or measure = to include an effect estimate."
    )))
  }
  report
}

#' Test whether a quosure evaluates to NULL
#' @keywords internal
#' @noRd
.report_quo_is_null <- function(quo) {
  identical(rlang::quo_get_expr(quo), NULL)
}

#' Evaluate tidyselect expression ensuring exactly one column is selected
#' @keywords internal
#' @noRd
.report_select_one <- function(data, quo, arg) {
  selected <- .report_select_many(data, quo, arg)
  if (length(selected) != 1) {
    simtab_abort_binding(c(
      "{.arg {arg}} must select exactly one column.",
      "i" = "Selected {length(selected)} column(s): {.val {selected}}.",
      "v" = "Name a single column."
    ))
  }
  selected[[1]]
}

#' Evaluate tidyselect expression ensuring at least one column is selected
#' @keywords internal
#' @noRd
.report_select_many <- function(data, quo, arg) {
  if (rlang::quo_is_missing(quo) || .report_quo_is_null(quo)) {
    simtab_abort_binding(c(
      "{.arg {arg}} must select at least one column.",
      "i" = "No selection was supplied.",
      "v" = "Name the column(s) to use."
    ))
  }
  expr <- rlang::quo_get_expr(quo)
  env <- rlang::quo_get_env(quo)
  selected <- names(tidyselect::eval_select(expr, data = data, env = env))
  if (length(selected) == 0) {
    simtab_abort_binding(c(
      "{.arg {arg}} must select at least one column.",
      "i" = "The selection matched no columns in the data.",
      "v" = "Check the spelling, or widen the selection."
    ))
  }
  selected
}

#########
# REPORT COERCION AND RENDERER DISPATCH
# S3 methods for data frame coercion, gt tables, flextable lists, and Excel sheet naming.

#' Coerce a SimtablR report to a data.frame
#'
#' A `simtab_report` bundles items that can have different shapes (for
#' example `simtablr()`'s descriptive Table 1 and association Table 2), so
#' there is no single natural rectangular table for a report in general. This
#' default method instead returns a named list of `as.data.frame()` results,
#' one per item, mirroring [as_gt.simtab_report()] and
#' [as_flextable.simtab_report()]. Subclasses whose items always share one
#' shape, such as [sensitivity()], provide their own
#' rectangular `as.data.frame()` method instead of using this default.
#'
#' @param x A `simtab_report`.
#' @param row.names,optional,... Passed to each item's `as.data.frame()`.
#' @return A named list of data.frames, one per report item.
#' @examples
#' rep <- simtablr(epitabl, outcome = mace_event, exposure = renal_impairment,
#'                 vars = c(age, sex), design = "cohort")
#' as.data.frame(rep)
#' @export
as.data.frame.simtab_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_simtab_report(x)
  stats::setNames(
    lapply(x$items, as.data.frame, row.names = row.names, optional = optional, ...),
    names(x$items)
  )
}

#' @rdname as_gt
#' @export
as_gt.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  stats::setNames(
    lapply(x$items, as_gt, ...),
    names(x$items)
  )
}

#' @rdname as_flextable.simtab_result
#' @export
as_flextable.simtab_report <- function(x, ...) {
  .require_pkg("flextable")
  x <- validate_simtab_report(x)
  stats::setNames(
    lapply(x$items, flextable::as_flextable, ...),
    names(x$items)
  )
}

#' Sanitize and truncate item name to a valid unique Excel sheet name
#' @keywords internal
#' @noRd
.report_sheet_name <- function(name, used = character()) {
  name <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", name)
  name <- substr(name, 1, 31)
  if (!nzchar(name)) {
    name <- "sheet"
  }
  base <- name
  i <- 1L
  while (name %in% used) {
    suffix <- paste0("_", i)
    name <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
    i <- i + 1L
  }
  name
}

#' Row-bind tabular frames from multiple report items with item indicator
#' @keywords internal
#' @noRd
.report_bind_item_frames <- function(items, frame_fun) {
  frames <- Map(function(item, nm) {
    df <- frame_fun(item)
    attr(df, "row_type") <- NULL
    attr(df, "row_var") <- NULL
    attr(df, "row_level") <- NULL
    attr(df, "confusion_matrix") <- NULL
    data.frame(item = nm, df, check.names = FALSE)
  }, items, names(items))
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}
