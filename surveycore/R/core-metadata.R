# R/core-metadata.R
#
# Metadata extraction and setting functions for survey design objects.
#
# Dispatch note: UseMethod() does not work for S7 objects when the package is
# loaded under its own namespace, because S7 uses namespaced class names (e.g.,
# "surveycore::survey_base") that cannot be S3 method suffixes. All functions
# here are implemented as plain functions that validate their first argument
# with S7::S7_inherits(). NSE arguments are captured via rlang::enquo() at the
# function level before any branching.
#
# Functions defined here:
#   Extractors (getters):
#     extract_var_label()        — variable labels
#     extract_val_labels()       — value labels
#     extract_question_preface() — question preface text
#     extract_var_note()         — analyst notes
#     extract_universe()         — universe (eligibility) descriptions
#     extract_missing_codes()    — missing value sentinel codes
#     extract_metadata()         — all metadata fields for one or more variables
#     extract_sata()             — SATA (select-all-that-apply) flags
#     extract_higher_is()        — direction-of-improvement attributes
#     extract_reverse_coded()    — reverse-coded flags
#   Unified setters (conventions 1/2/3, survey objects + data frames):
#     set_var_label()            — set label for one or more variables
#     set_val_labels()           — set value labels for one or more variables
#     set_question_preface()     — set question preface for one or more
#                                  variables
#     set_var_note()             — set note for one or more variables
#     set_universe()             — set universe description for one or more vars
#     set_missing_codes()        — set missing codes for one or more variables
#     set_sata()                 — set SATA flag for one or more variables
#     set_higher_is()            — set direction-of-improvement attribute
#     set_reverse_coded()        — set reverse-coded flag
#   Internal helpers:
#     .validate_val_labels()     — check label completeness
#     .extract_haven_metadata()  — read haven-style attrs from a data.frame

# ── PR 2 internal helper infrastructure ──────────────────────────────────────

# .check_is_survey_or_df(x, call)
# Type guard accepting survey objects and plain data frames; replaces
# .check_is_survey() for all functions that gain data frame support.
.check_is_survey_or_df <- function(x, call = rlang::caller_env()) {
  if (!S7::S7_inherits(x, survey_base) && !is.data.frame(x)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a survey design object or a data frame,",
          " not {.cls {class(x)[[1L]]}}."
        ),
        "v" = paste0(
          "Create a survey object with {.fn as_survey},",
          " {.fn as_survey_replicate}, or {.fn as_survey_twophase}."
        )
      ),
      class = "surveycore_error_not_survey_or_df",
      call = call
    )
  }
  invisible(NULL)
}

# .get_data_cols(x)
# Returns the names of all data columns: names(x) for data frames,
# names(x@data) for survey objects.
.get_data_cols <- function(x) {
  if (is.data.frame(x)) names(x) else names(x@data)
}

# .get_metadata(x)
# Returns x@metadata for survey objects, NULL for data frames.
.get_metadata <- function(x) {
  if (S7::S7_inherits(x, survey_base)) x@metadata else NULL
}

# .parse_setter_input(dots, variable, content, content_arg_name,
#                     content_type, fn_name, call)
# Shared convention detection for all unified setters. Receives `dots` as an
# evaluated list (rlang::list2(...) at the caller). Returns a named list
# mapping variable names to content values; NULL values signal deletion.
#
# content_type:
#   "scalar" — Convention 2 expects a named character vector in ...
#   "vector" — Convention 2 expects a named list in ...
.parse_setter_input <- function(
  dots,
  variable,
  content,
  content_arg_name,
  content_type = c("scalar", "vector"),
  fn_name,
  call = rlang::caller_env()
) {
  content_type <- match.arg(content_type)
  dots_len <- length(dots)
  var_provided <- !is.null(variable)

  # Ambiguity: both ... and variable provided
  if (dots_len > 0L && var_provided) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        ),
        "i" = paste0(
          "Use named {.arg ...} args, a named vector in {.arg ...}, or",
          " {.arg variable} + {.arg {content_arg_name}} \u2014 not a mix."
        )
      ),
      class = "surveycore_error_setter_ambiguous",
      call = call
    )
  }

  # Empty: neither provided
  if (dots_len == 0L && !var_provided) {
    cli::cli_abort(
      c(
        "x" = "{.fn {fn_name}} requires at least one variable-label pair.",
        "v" = paste0(
          "Use named {.arg ...} args: ",
          "{.code {fn_name}(x, age = 'Age in years')}."
        )
      ),
      class = "surveycore_error_setter_empty",
      call = call
    )
  }

  # Convention 3: explicit variable + content arg
  if (var_provided && dots_len == 0L) {
    if (length(variable) == 0L) {
      cli::cli_warn(
        c(
          "!" = "{.fn {fn_name}} was called with {.arg variable} of length 0.",
          "i" = paste0(
            "No metadata was set. Did you accidentally filter ",
            "all variable names out?"
          )
        ),
        class = "surveycore_warning_setter_empty_variables",
        call = call
      )
      return(list())
    }
    if (!is.null(content) && length(variable) != length(content)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg variable} has {length(variable)} element{?s} but",
            " {.arg {content_arg_name}} has {length(content)} element{?s}."
          ),
          "i" = paste0(
            "They must be the same length ",
            "(one content value per variable name)."
          )
        ),
        class = "surveycore_error_setter_mismatched_lengths",
        call = call
      )
    }
    if (is.null(content)) {
      return(stats::setNames(vector("list", length(variable)), variable))
    }
    return(stats::setNames(as.list(content), variable))
  }

  # From here: dots_len > 0 and variable is NULL
  dot_names <- names(dots)
  has_unnamed <- is.null(dot_names) || any(!nzchar(dot_names))

  if (has_unnamed) {
    # Check Convention 2: single unnamed element that is itself a named
    # character vector (scalar) or named list (vector)
    if (dots_len == 1L) {
      elem <- dots[[1L]]
      is_named_char_vec <- (is.character(elem) &&
        !is.null(names(elem)) &&
        length(elem) > 0L &&
        all(nzchar(names(elem))))
      is_named_list <- (is.list(elem) &&
        !is.null(names(elem)) &&
        length(elem) > 0L &&
        all(nzchar(names(elem))))
      if (content_type == "scalar" && is_named_char_vec) {
        return(as.list(elem))
      }
      if (content_type == "vector" && is_named_list) {
        return(elem)
      }
    }
    # Not Convention 2 — mixed or fully unnamed ... elements
    n_named <- if (!is.null(dot_names)) sum(nzchar(dot_names)) else 0L
    n_unnamed <- dots_len - n_named
    cli::cli_abort(
      c(
        "x" = "All {.arg ...} arguments must be named when using Convention 1.",
        "i" = "Got {n_named} named and {n_unnamed} unnamed element{?s}.",
        "v" = paste0(
          "Use {.code {fn_name}(x, age = 'Age', income = 'Annual income')}",
          " or a fully named vector."
        )
      ),
      class = "surveycore_error_setter_mixed_dots",
      call = call
    )
  }

  # Convention 1: all elements are named
  dots
}

# .format_scalar_result(result_list, format, col_name, empty_value)
# Converts a named list of character scalars to the requested output format.
# empty_value = NULL omits NULL entries; empty_value = NA_character_ replaces
# NULL entries with NA.
.format_scalar_result <- function(
  result_list,
  format,
  col_name,
  empty_value
) {
  if (is.null(empty_value)) {
    result_list <- result_list[!vapply(result_list, is.null, logical(1L))]
  } else {
    result_list <- lapply(
      result_list,
      function(v) if (is.null(v)) empty_value else v
    )
  }

  var_names <- names(result_list)

  switch(
    format,
    "named_vector" = {
      if (length(var_names) == 0L) {
        return(character(0L))
      }
      values <- unlist(result_list, use.names = FALSE)
      stats::setNames(values, var_names)
    },
    "list" = result_list,
    "data_frame" = {
      values <- unname(vapply(
        result_list,
        function(v) if (is.null(v)) NA_character_ else v,
        character(1L)
      ))
      result <- tibble::tibble(variable = var_names)
      result[[col_name]] <- values
      result
    }
  )
}

# .format_list_result(result_list, format, fn_name)
# Converts a named list of vectors to "list" or "data_frame" format.
# "named_vector" is rejected with surveycore_error_format_invalid since vector
# content cannot be collapsed into a flat named vector.
.format_list_result <- function(
  result_list,
  format,
  fn_name
) {
  valid_formats <- c("list", "data_frame")
  if (!format %in% valid_formats) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn {fn_name}} received an invalid {.arg format} ",
          "value {.val {format}}."
        ),
        "i" = "{.arg format} must be one of {.val {valid_formats}}."
      ),
      class = "surveycore_error_format_invalid"
    )
  }

  if (format == "list") {
    return(result_list)
  }

  # data_frame: one row per (variable, code) pair
  empty_out <- tibble::tibble(
    variable = character(0L),
    label = character(0L),
    value = character(0L)
  )

  if (length(result_list) == 0L) {
    return(empty_out)
  }

  rows <- lapply(names(result_list), function(var_name) {
    vec <- result_list[[var_name]]
    if (is.null(vec) || length(vec) == 0L) {
      return(NULL)
    }
    lbl <- if (!is.null(names(vec))) {
      names(vec)
    } else {
      rep(NA_character_, length(vec))
    }
    tibble::tibble(
      variable = var_name,
      label = lbl,
      value = as.character(vec)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]

  if (length(rows) == 0L) {
    return(empty_out)
  }

  dplyr::bind_rows(rows)
}

# .format_logical_result(result, format)
# Converts a named logical vector into the format requested by extract_sata().
# - "named_vector": returns the input vector as-is (empty: logical(0)).
# - "list": as.list(result) (empty: list()).
# - "data_frame": tibble with columns `variable` and `sata`
#   (empty: zero-row tibble with both columns typed).
.format_logical_result <- function(result, format) {
  switch(
    format,
    "named_vector" = result,
    "list" = as.list(result),
    "data_frame" = tibble::tibble(
      variable = names(result),
      sata = unname(result)
    )
  )
}


# ── Input check helper ────────────────────────────────────────────────────────

# Used by all exported functions to produce a consistent error when x is not
# a survey design object.
.check_is_survey <- function(x, call = rlang::caller_env()) {
  if (!S7::S7_inherits(x, survey_base)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a survey design object, not ",
          "{.cls {class(x)[[1L]]}}."
        ),
        "v" = paste0(
          "Create a survey object with {.fn as_survey}, ",
          "{.fn as_survey_replicate}, or {.fn as_survey_twophase}."
        )
      ),
      class = "surveycore_error_not_survey",
      call = call
    )
  }
  invisible(NULL)
}


# ── Extractors ────────────────────────────────────────────────────────────────

# Shared fill validator for individual extractors
# (accepts NULL or NA_character_).
.check_extractor_fill <- function(fill, fn_name, call) {
  if (!is.null(fill) && !identical(fill, NA_character_)) {
    cli::cli_abort(
      c(
        "x" = "{.fn {fn_name}} does not accept {.code fill = {.val {fill}}}.",
        "i" = paste0(
          "Valid values for {.fn {fn_name}}: ",
          "{.val NULL} (omit) or {.code NA_character_} (include with NA)."
        )
      ),
      class = "surveycore_error_fill_invalid",
      call = call
    )
  }
  invisible(NULL)
}

# Shared format validator. valid_formats must match the function's contract.
.check_extractor_format <- function(format, fn_name, valid_formats, call) {
  if (!format %in% valid_formats) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn {fn_name}} received an invalid {.arg format} value ",
          "{.val {format}}."
        ),
        "i" = "{.arg format} must be one of {.val {valid_formats}}."
      ),
      class = "surveycore_error_format_invalid",
      call = call
    )
  }
  invisible(NULL)
}


#' Extract Variable Labels
#'
#' Returns variable labels for one or more variables in a survey design object
#' or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param format `character(1)`. Output format: `"named_vector"` (default),
#'   `"list"`, or `"data_frame"`.
#' @param fill Scalar or `NULL`. How to handle variables with no label:
#'   `NULL` (default) omits them; `NA_character_` includes them with `NA`.
#'
#' @return
#' - `"named_vector"` (default): named character vector. Empty: `character(0)`.
#' - `"list"`: named list of character scalars. Empty: `list()`.
#' - `"data_frame"`: tibble with columns `variable` and `label`. Empty:
#'   zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' extract_var_label(d)
#' extract_var_label(d, riagendr, ridageyr)
#' extract_var_label(d, format = "data_frame")
#' extract_var_label(d, fill = NA_character_)
#' @seealso [set_var_label()] to set a variable label
#' @family metadata
#' @export
extract_var_label <- function(x, ..., format = "named_vector", fill = NULL) {
  call <- rlang::caller_env()
  fn_name <- "extract_var_label"
  .check_extractor_fill(fill, fn_name, call)
  .check_extractor_format(
    format,
    fn_name,
    c("named_vector", "list", "data_frame"),
    call
  )
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result_list <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@variable_labels[[v]]
      } else {
        attr(x[[v]], "label", exact = TRUE)
      }
    }),
    var_names
  )
  .format_scalar_result(result_list, format, "label", fill)
}


#' Extract Value Labels
#'
#' Returns value labels for one or more variables in a survey design object
#' or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param format `character(1)`. Output format: `"list"` (default) or
#'   `"data_frame"`. `"named_vector"` is not valid for this function.
#' @param fill Scalar or `NULL`. How to handle variables with no labels:
#'   `NULL` (default) omits them; `NA_character_` includes them as `NULL`
#'   entries in `"list"` format. In `"data_frame"` format, variables with
#'   no labels are always excluded regardless of `fill`.
#'
#' @return
#' - `"list"` (default): named list of named vectors. Empty: `list()`.
#' - `"data_frame"`: long-format tibble with columns `variable`, `label`,
#'   `value` (codes coerced to character). Empty: zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' extract_val_labels(d, riagendr)
#' extract_val_labels(d, riagendr, format = "data_frame")
#' @seealso [set_val_labels()] to set value labels
#' @family metadata
#' @export
extract_val_labels <- function(x, ..., format = "list", fill = NULL) {
  call <- rlang::caller_env()
  fn_name <- "extract_val_labels"
  .check_extractor_fill(fill, fn_name, call)
  .check_extractor_format(format, fn_name, c("list", "data_frame"), call)
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result_list <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@value_labels[[v]]
      } else {
        attr(x[[v]], "labels", exact = TRUE)
      }
    }),
    var_names
  )
  if (is.null(fill)) {
    result_list <- result_list[!vapply(result_list, is.null, logical(1L))]
  }
  .format_list_result(result_list, format, fn_name)
}


#' Extract Question Prefaces
#'
#' Returns question preface text for one or more variables in a survey design
#' object or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param format `character(1)`. Output format: `"named_vector"` (default),
#'   `"list"`, or `"data_frame"`.
#' @param fill Scalar or `NULL`. How to handle variables with no preface:
#'   `NULL` (default) omits them; `NA_character_` includes them with `NA`.
#'
#' @return
#' - `"named_vector"` (default): named character vector. Empty: `character(0)`.
#' - `"list"`: named list of character scalars. Empty: `list()`.
#' - `"data_frame"`: tibble with columns `variable` and `preface`. Empty:
#'   zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d <- set_question_preface(d, happy = "Taken all together...")
#' extract_question_preface(d, happy)
#' @seealso [set_question_preface()] to set a question preface
#' @family metadata
#' @export
extract_question_preface <- function(
  x,
  ...,
  format = "named_vector",
  fill = NULL
) {
  call <- rlang::caller_env()
  fn_name <- "extract_question_preface"
  .check_extractor_fill(fill, fn_name, call)
  .check_extractor_format(
    format,
    fn_name,
    c("named_vector", "list", "data_frame"),
    call
  )
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result_list <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@question_prefaces[[v]]
      } else {
        attr(x[[v]], "question_preface", exact = TRUE)
      }
    }),
    var_names
  )
  .format_scalar_result(result_list, format, "preface", fill)
}


#' Extract Analyst Notes
#'
#' Returns analyst notes for one or more variables in a survey design object
#' or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param format `character(1)`. Output format: `"named_vector"` (default),
#'   `"list"`, or `"data_frame"`.
#' @param fill Scalar or `NULL`. How to handle variables with no note:
#'   `NULL` (default) omits them; `NA_character_` includes them with `NA`.
#'
#' @return
#' - `"named_vector"` (default): named character vector. Empty: `character(0)`.
#' - `"list"`: named list of character scalars. Empty: `list()`.
#' - `"data_frame"`: tibble with columns `variable` and `note`. Empty:
#'   zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d <- set_var_note(d, age = "Top-coded at 89")
#' extract_var_note(d, age)
#' @seealso [set_var_note()] to set a note
#' @family metadata
#' @export
extract_var_note <- function(x, ..., format = "named_vector", fill = NULL) {
  call <- rlang::caller_env()
  fn_name <- "extract_var_note"
  .check_extractor_fill(fill, fn_name, call)
  .check_extractor_format(
    format,
    fn_name,
    c("named_vector", "list", "data_frame"),
    call
  )
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result_list <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@notes[[v]]
      } else {
        attr(x[[v]], "note", exact = TRUE)
      }
    }),
    var_names
  )
  .format_scalar_result(result_list, format, "note", fill)
}


#' Extract Universe Descriptions
#'
#' Returns universe (eligibility) descriptions for one or more variables in a
#' survey design object or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param format `character(1)`. Output format: `"named_vector"` (default),
#'   `"list"`, or `"data_frame"`.
#' @param fill Scalar or `NULL`. How to handle variables with no universe:
#'   `NULL` (default) omits them; `NA_character_` includes them with `NA`.
#'
#' @return
#' - `"named_vector"` (default): named character vector. Empty: `character(0)`.
#' - `"list"`: named list of character scalars. Empty: `list()`.
#' - `"data_frame"`: tibble with columns `variable` and `universe`. Empty:
#'   zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_universe(d, ridageyr = "All participants 0+")
#' extract_universe(d)
#' extract_universe(d, ridageyr, format = "data_frame")
#' @seealso [set_universe()] to set a universe description
#' @family metadata
#' @export
extract_universe <- function(x, ..., format = "named_vector", fill = NULL) {
  call <- rlang::caller_env()
  fn_name <- "extract_universe"
  .check_extractor_fill(fill, fn_name, call)
  .check_extractor_format(
    format,
    fn_name,
    c("named_vector", "list", "data_frame"),
    call
  )
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result_list <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@universe[[v]]
      } else {
        attr(x[[v]], "universe", exact = TRUE)
      }
    }),
    var_names
  )
  .format_scalar_result(result_list, format, "universe", fill)
}


#' Extract Missing Value Codes
#'
#' Returns missing value sentinel codes for one or more variables in a survey
#' design object or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param format `character(1)`. Output format: `"list"` (default) or
#'   `"data_frame"`. `"named_vector"` is not valid for this function.
#' @param fill Scalar or `NULL`. How to handle variables with no codes:
#'   `NULL` (default) omits them; `NA_character_` includes them as `NULL`
#'   entries in `"list"` format. In `"data_frame"` format, variables with
#'   no codes are always excluded regardless of `fill`.
#'
#' @return
#' - `"list"` (default): named list of atomic vectors. Empty: `list()`.
#' - `"data_frame"`: long-format tibble with columns `variable`, `description`
#'   (`NA` if codes vector is unnamed), `code` (coerced to character). Empty:
#'   zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_missing_codes(d, ridageyr = c("Not applicable" = 999L))
#' extract_missing_codes(d, ridageyr)
#' extract_missing_codes(d, ridageyr, format = "data_frame")
#' @seealso [set_missing_codes()] to set missing value codes
#' @family metadata
#' @export
extract_missing_codes <- function(x, ..., format = "list", fill = NULL) {
  call <- rlang::caller_env()
  fn_name <- "extract_missing_codes"
  .check_extractor_fill(fill, fn_name, call)
  .check_extractor_format(format, fn_name, c("list", "data_frame"), call)
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result_list <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@missing_codes[[v]]
      } else {
        attr(x[[v]], "missing_codes", exact = TRUE)
      }
    }),
    var_names
  )
  if (is.null(fill)) {
    result_list <- result_list[!vapply(result_list, is.null, logical(1L))]
  }
  if (format == "list") {
    return(result_list)
  }
  # data_frame: long format with variable / description / code columns
  # (different column names from extract_val_labels data_frame)
  result_list_nonnull <- result_list[
    !vapply(result_list, is.null, logical(1L))
  ]
  empty_out <- tibble::tibble(
    variable = character(0L),
    description = character(0L),
    code = character(0L)
  )
  if (length(result_list_nonnull) == 0L) {
    return(empty_out)
  }
  rows <- lapply(names(result_list_nonnull), function(var_name) {
    vec <- result_list_nonnull[[var_name]]
    if (is.null(vec) || length(vec) == 0L) {
      return(NULL)
    }
    desc <- if (!is.null(names(vec))) {
      names(vec)
    } else {
      rep(NA_character_, length(vec))
    }
    tibble::tibble(
      variable = var_name,
      description = desc,
      code = as.character(vec)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(empty_out)
  }
  dplyr::bind_rows(rows)
}


#' Extract All Metadata for Variables
#'
#' Returns a summary of all metadata fields for one or more variables in a
#' survey design object or data frame. Useful for auditing metadata state or
#' building codebooks.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], [tidyselect::matches()],
#'   etc. If empty, returns metadata for all variables. Use
#'   [tidyselect::any_of()] to silently skip missing variable names.
#' @param fill `NULL` (default) or `"include"`. `NULL` omits variables that
#'   have no metadata in any field; `"include"` returns all variables
#'   regardless.
#'
#' @return A named list. Each entry is a named list with keys:
#'   `variable_label`, `value_labels`, `question_preface`, `note`,
#'   `universe`, `missing_codes`, `transformations`.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_universe(d, ridageyr = "All participants 0+")
#' extract_metadata(d, ridageyr)
#' extract_metadata(d, fill = "include")
#' @family metadata
#' @export
extract_metadata <- function(x, ..., fill = NULL) {
  call <- rlang::caller_env()
  fn_name <- "extract_metadata"
  if (!is.null(fill) && !identical(fill, "include")) {
    cli::cli_abort(
      c(
        "x" = "{.fn {fn_name}} does not accept {.code fill = {.val {fill}}}.",
        "i" = paste0(
          "Valid values for {.fn {fn_name}}: ",
          "{.val NULL} (omit empty) or {.val \"include\"} (return all)."
        )
      ),
      class = "surveycore_error_fill_invalid",
      call = call
    )
  }
  .check_is_survey_or_df(x, call = call)
  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }
  result <- stats::setNames(
    lapply(var_names, function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        t <- x@metadata@transformations[[v]]
        list(
          variable_label = x@metadata@variable_labels[[v]],
          value_labels = x@metadata@value_labels[[v]],
          question_preface = x@metadata@question_prefaces[[v]],
          note = x@metadata@notes[[v]],
          universe = x@metadata@universe[[v]],
          missing_codes = x@metadata@missing_codes[[v]],
          transformations = if (is.null(t)) list() else t
        )
      } else {
        list(
          variable_label = attr(x[[v]], "label", exact = TRUE),
          value_labels = attr(x[[v]], "labels", exact = TRUE),
          question_preface = attr(x[[v]], "question_preface", exact = TRUE),
          note = attr(x[[v]], "note", exact = TRUE),
          universe = attr(x[[v]], "universe", exact = TRUE),
          missing_codes = attr(x[[v]], "missing_codes", exact = TRUE),
          transformations = list()
        )
      }
    }),
    var_names
  )
  if (is.null(fill)) {
    content_fields <- c(
      "variable_label",
      "value_labels",
      "question_preface",
      "note",
      "universe",
      "missing_codes"
    )
    has_meta <- vapply(
      result,
      function(entry) {
        any(
          !vapply(content_fields, function(f) is.null(entry[[f]]), logical(1L))
        ) ||
          length(entry$transformations) > 0L
      },
      logical(1L)
    )
    result <- result[has_meta]
    if (length(result) == 0L) return(list())
  }
  result
}


# ── Unified setters ───────────────────────────────────────────────────────────

# Internal helper: shared scalar validation for set_var_label(),
# set_question_preface(), set_var_note(), and set_universe().
# Errors with surveycore_error_label_not_scalar when content is non-NULL and
# is not a character scalar.
.validate_scalar_content <- function(content, var_name, call) {
  if (!is.null(content)) {
    if (!is.character(content) || length(content) != 1L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Label content for {.field {var_name}} must be a character",
            " scalar, not {.cls {class(content)[[1L]]}} of length",
            " {length(content)}."
          ),
          "v" = paste0(
            "Pass a single character string, e.g.",
            " {.code set_var_label(x, {var_name} = 'My label')}."
          )
        ),
        class = "surveycore_error_label_not_scalar",
        call = call
      )
    }
  }
  invisible(NULL)
}


#' Set Variable Label(s)
#'
#' Sets variable labels using one of three conventions.
#'
#' **Convention 1 (named `...`)** — recommended for interactive use:
#' ```r
#' set_var_label(x, age = "Age in years", income = "Annual income")
#' set_var_label(x, !!!labels_list)   # list splicing
#' ```
#'
#' **Convention 2 (named vector in `...`)** — useful for programmatic use:
#' ```r
#' set_var_label(x, c(age = "Age in years", income = "Annual income"))
#' ```
#'
#' **Convention 3 (`variable` + `label` arguments)** — for vector input:
#' ```r
#' vars <- c("age", "income")
#' lbls <- c("Age in years", "Annual income")
#' set_var_label(x, variable = vars, label = lbls)
#' ```
#'
#' @param x A survey design object or a data frame.
#' @param ... Named arguments where the name is the variable and the value is
#'   the label string. Supports `!!!` list splicing.
#' @param variable A character vector of variable names. Use with `label`.
#' @param label A character vector of label strings, one per element of
#'   `variable`.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_var_label(d, indfmpir = "Income-to-poverty ratio")
#'
#' # Multiple variables
#' d <- set_var_label(
#'   d,
#'   bpxsy1 = "Systolic BP (1st reading)",
#'   bpxdi1 = "Diastolic BP (1st reading)"
#' )
#' @seealso [extract_var_label()] to retrieve a label
#' @family metadata
#' @export
set_var_label <- function(x, ..., variable = NULL, label = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  # Detect old positional form: set_var_label(x, bare_symbol, "string")
  dot_quos <- rlang::enquos(...)
  nms <- names(dot_quos)
  if (
    length(dot_quos) == 2L &&
      (is.null(nms) ||
        (!nzchar(if (!is.null(nms[[1L]])) nms[[1L]] else "") &&
          !nzchar(if (!is.null(nms[[2L]])) nms[[2L]] else ""))) &&
      rlang::quo_is_symbol(dot_quos[[1L]])
  ) {
    val2 <- tryCatch(rlang::eval_tidy(dot_quos[[2L]]), error = function(e) NULL)
    if (is.character(val2) && length(val2) == 1L) {
      var_nm2 <- rlang::as_name(dot_quos[[1L]])
      cli::cli_abort(
        c(
          "x" = paste0(
            "The old positional calling form",
            " {.code set_var_label(x, var, content)} is no longer supported."
          ),
          "i" = "The new unified setter uses named arguments.",
          "v" = paste0(
            "Use {.code set_var_label(x, {var_nm2} = {.val {val2}})} ",
            "instead."
          )
        ),
        class = "surveycore_error_old_positional_setter",
        call = call
      )
    }
  }

  dots <- rlang::list2(...)
  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = label,
    content_arg_name = "label",
    content_type = "scalar",
    fn_name = "set_var_label",
    call = call
  )

  all_cols <- .get_data_cols(x)
  for (var_name in names(pairs)) {
    content <- pairs[[var_name]]
    if (!var_name %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Variable {.field {var_name}} not found in ",
            "{.arg x} and was skipped."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    .validate_scalar_content(content, var_name, call)
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@variable_labels[[var_name]] <- content
    } else {
      attr(x[[var_name]], "label") <- content
    }
  }
  invisible(x)
}


#' Set Value Labels
#'
#' Sets value labels for one or more variables using one of three conventions.
#'
#' **Convention 1 (named `...`)** — recommended:
#' ```r
#' set_val_labels(x, sex = c(Male = 1L, Female = 2L))
#' ```
#'
#' **Convention 2 (single named list in `...`)**:
#' ```r
#' set_val_labels(x, list(sex = c(Male = 1L, Female = 2L)))
#' ```
#'
#' **Convention 3 (`variable` + `labels`)**:
#' ```r
#' set_val_labels(x, variable = "sex", labels = c(Male = 1L, Female = 2L))
#' ```
#'
#' @param x A survey design object or a data frame.
#' @param ... Named arguments where the name is the variable and the value is
#'   a fully named vector of value labels. Supports `!!!` list splicing.
#' @param variable A character vector of variable names.
#' @param labels A list of named vectors, one per element of `variable`. When
#'   `variable` has length 1, a bare named vector is also accepted.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_val_labels(d, riagendr = c(Male = 1L, Female = 2L))
#' @seealso [extract_val_labels()] to retrieve value labels
#' @family metadata
#' @export
set_val_labels <- function(x, ..., variable = NULL, labels = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  # Convention 3 bare-vector exception: wrap scalar atomic vector in a list
  # when length(variable) == 1 and labels is not already a list.
  if (
    !is.null(variable) &&
      length(variable) == 1L &&
      !is.null(labels) &&
      !is.list(labels) &&
      is.atomic(labels)
  ) {
    labels <- list(labels)
  }

  dots <- rlang::list2(...)
  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = labels,
    content_arg_name = "labels",
    content_type = "vector",
    fn_name = "set_val_labels",
    call = call
  )

  all_cols <- .get_data_cols(x)
  for (var_name in names(pairs)) {
    content <- pairs[[var_name]]
    if (!var_name %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Variable {.field {var_name}} not found in ",
            "{.arg x} and was skipped."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    if (!is.null(content)) {
      if (is.null(names(content)) || any(!nzchar(names(content)))) {
        cli::cli_abort(
          c(
            "x" = "{.arg labels} must be a fully named vector.",
            "i" = "All elements must have names."
          ),
          class = "surveycore_error_labels_unnamed",
          call = call
        )
      }
      var_data <- if (S7::S7_inherits(x, survey_base)) {
        x@data[[var_name]]
      } else {
        x[[var_name]]
      }
      .validate_val_labels(var_data, content, var_name = var_name)
    }
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@value_labels[[var_name]] <- content
    } else {
      attr(x[[var_name]], "labels") <- content
    }
  }
  invisible(x)
}


#' Set Question Preface(s)
#'
#' Sets the question preface string for one or more variables. Question prefaces
#' are the shared introductory text for a battery of related questions.
#'
#' Supports Conventions 1, 2, and 3 — see [set_var_label()] for details.
#'
#' @param x A survey design object or a data frame.
#' @param ... Named arguments where the name is the variable and the value is
#'   the preface string. Supports `!!!` list splicing.
#' @param variable A character vector of variable names. Use with `preface`.
#' @param preface A character vector of preface strings, one per element of
#'   `variable`.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d <- set_question_preface(d, happy = "Taken all together...")
#' extract_question_preface(d, happy)
#' @seealso [extract_question_preface()] to retrieve a preface
#' @family metadata
#' @export
set_question_preface <- function(x, ..., variable = NULL, preface = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots <- rlang::list2(...)
  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = preface,
    content_arg_name = "preface",
    content_type = "scalar",
    fn_name = "set_question_preface",
    call = call
  )

  all_cols <- .get_data_cols(x)
  for (var_name in names(pairs)) {
    content <- pairs[[var_name]]
    if (!var_name %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Variable {.field {var_name}} not found in ",
            "{.arg x} and was skipped."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    .validate_scalar_content(content, var_name, call)
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@question_prefaces[[var_name]] <- content
    } else {
      attr(x[[var_name]], "question_preface") <- content
    }
  }
  invisible(x)
}


#' Set Analyst Note(s)
#'
#' Sets an analyst note for one or more variables. Notes are free-text
#' annotations for documenting processing decisions, data quality concerns,
#' or other context.
#'
#' Supports Conventions 1, 2, and 3 — see [set_var_label()] for details.
#'
#' @param x A survey design object or a data frame.
#' @param ... Named arguments where the name is the variable and the value is
#'   the note string. Supports `!!!` list splicing.
#' @param variable A character vector of variable names. Use with `note`.
#' @param note A character vector of note strings, one per element of
#'   `variable`.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d <- set_var_note(d, age = "Top-coded at 89")
#' extract_var_note(d, age)
#' @seealso [extract_var_note()] to retrieve a note
#' @family metadata
#' @export
set_var_note <- function(x, ..., variable = NULL, note = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots <- rlang::list2(...)
  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = note,
    content_arg_name = "note",
    content_type = "scalar",
    fn_name = "set_var_note",
    call = call
  )

  all_cols <- .get_data_cols(x)
  for (var_name in names(pairs)) {
    content <- pairs[[var_name]]
    if (!var_name %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Variable {.field {var_name}} not found in ",
            "{.arg x} and was skipped."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    .validate_scalar_content(content, var_name, call)
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@notes[[var_name]] <- content
    } else {
      attr(x[[var_name]], "note") <- content
    }
  }
  invisible(x)
}


#' Set Universe Description(s)
#'
#' Sets the universe description for one or more variables. The universe
#' describes the population to which a variable applies
#' (e.g., `"Adults 18+"`).
#'
#' Supports Conventions 1, 2, and 3 — see [set_var_label()] for details.
#'
#' @param x A survey design object or a data frame.
#' @param ... Named arguments where the name is the variable and the value is
#'   the universe description string. Supports `!!!` list splicing.
#' @param variable A character vector of variable names. Use with `universe`.
#' @param universe A character vector of universe description strings, one per
#'   element of `variable`.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d <- set_universe(d, age = "All respondents 18+")
#' extract_metadata(d, age)
#' @seealso [extract_universe()] to retrieve universe descriptions
#' @family metadata
#' @export
set_universe <- function(x, ..., variable = NULL, universe = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots <- rlang::list2(...)
  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = universe,
    content_arg_name = "universe",
    content_type = "scalar",
    fn_name = "set_universe",
    call = call
  )

  all_cols <- .get_data_cols(x)
  for (var_name in names(pairs)) {
    content <- pairs[[var_name]]
    if (!var_name %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Variable {.field {var_name}} not found in ",
            "{.arg x} and was skipped."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    .validate_scalar_content(content, var_name, call)
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@universe[[var_name]] <- content
    } else {
      attr(x[[var_name]], "universe") <- content
    }
  }
  invisible(x)
}


#' Set Missing Code(s)
#'
#' Sets missing-value codes for one or more variables. Missing codes are atomic
#' vectors documenting which data values represent missing data
#' (e.g., `c(Refused = -2L, DontKnow = -1L)`).
#'
#' Supports Conventions 1, 2, and 3 — see [set_var_label()] for details on
#' the calling conventions. For Convention 3 with a single variable, a bare
#' named atomic vector is accepted in addition to a list.
#'
#' @param x A survey design object or a data frame.
#' @param ... Named arguments where the name is the variable and the value is
#'   a named atomic vector of missing codes. Supports `!!!` list splicing.
#' @param variable A character vector of variable names. Use with `codes`.
#' @param codes A list of named atomic vectors, one per element of `variable`.
#'   When `variable` has length 1, a bare named atomic vector is also accepted.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d <- set_missing_codes(d, happy = c(Refused = -1L, DK = -2L))
#' extract_missing_codes(d, happy)
#' @seealso [extract_missing_codes()] to retrieve missing value codes
#' @family metadata
#' @export
set_missing_codes <- function(x, ..., variable = NULL, codes = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  # Convention 3 bare-vector exception: wrap atomic vector in a list
  # when length(variable) == 1 and codes is not already a list.
  if (
    !is.null(variable) &&
      length(variable) == 1L &&
      !is.null(codes) &&
      !is.list(codes) &&
      is.atomic(codes)
  ) {
    codes <- list(codes)
  }

  dots <- rlang::list2(...)
  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = codes,
    content_arg_name = "codes",
    content_type = "vector",
    fn_name = "set_missing_codes",
    call = call
  )

  all_cols <- .get_data_cols(x)
  for (var_name in names(pairs)) {
    content <- pairs[[var_name]]
    if (!var_name %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Variable {.field {var_name}} not found in ",
            "{.arg x} and was skipped."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    if (!is.null(content) && is.list(content)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Missing codes for {.field {var_name}} must be an atomic",
            " vector, not a list."
          ),
          "i" = paste0(
            "Got class {.cls {class(content)[[1L]]}}."
          ),
          "v" = paste0(
            "Use {.code set_missing_codes(x, {var_name} =",
            " c(Missing = -1L))} instead."
          )
        ),
        class = "surveycore_error_missing_codes_not_vector",
        call = call
      )
    }
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@missing_codes[[var_name]] <- content
    } else {
      attr(x[[var_name]], "missing_codes") <- content
    }
  }
  invisible(x)
}


# ── SATA (Select-All-That-Apply) setter/getter ────────────────────────────────

#' Set SATA (Select-All-That-Apply) Flag
#'
#' Marks one or more variables as select-all-that-apply (SATA) in a survey
#' design object or a data frame. Unlike the other unified setters (which map
#' variable names to heterogeneous content), `set_sata()` applies a single
#' logical flag to all listed variables, so it uses a simplified two-convention
#' pattern.
#'
#' **Convention A (tidy-select `...`)** — recommended:
#' ```r
#' design |> set_sata(news_tv, news_online, news_radio)
#' design |> set_sata(starts_with("news_"))
#' ```
#'
#' **Convention B (`variable` = character vector)** — programmatic:
#' ```r
#' sata_vars <- c("news_tv", "news_online", "news_radio")
#' design |> set_sata(variable = sata_vars)
#' ```
#'
#' Setting `sata = FALSE` unmarks the listed variables.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to mark.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], etc. Cannot be combined
#'   with `variable`.
#' @param variable `character`. Alternative programmatic interface: character
#'   vector of variable names. Cannot be combined with `...`.
#' @param sata `logical(1)`. `TRUE` (default) marks variables as SATA; `FALSE`
#'   removes the SATA flag. `NA` is not accepted.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_sata(d, riagendr, ridageyr)
#' d <- set_sata(d, riagendr, sata = FALSE)
#' @seealso [extract_sata()] to retrieve SATA flags
#' @family metadata
#' @export
set_sata <- function(x, ..., variable = NULL, sata = TRUE) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  if (!is.logical(sata) || length(sata) != 1L || is.na(sata)) {
    cli::cli_abort(
      c("x" = "{.arg sata} must be {.code TRUE} or {.code FALSE}."),
      class = "surveycore_error_sata_not_logical",
      call = call
    )
  }

  dots_used <- ...length() > 0L
  var_used <- !is.null(variable)

  if (dots_used && var_used) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        )
      ),
      class = "surveycore_error_sata_ambiguous_input",
      call = call
    )
  }

  if (!dots_used && (!var_used || length(variable) == 0L)) {
    cli::cli_abort(
      c("x" = "{.fn set_sata} requires at least one variable name."),
      class = "surveycore_error_sata_no_vars",
      call = call
    )
  }

  all_cols <- .get_data_cols(x)

  if (dots_used) {
    var_names <- names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  } else {
    missing <- setdiff(variable, all_cols)
    if (length(missing) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{length(missing)} variable{?s} not found in {.arg x}",
            " and {?was/were} skipped: {.field {missing}}."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
    }
    var_names <- intersect(variable, all_cols)
  }

  for (v in var_names) {
    if (S7::S7_inherits(x, survey_base)) {
      if (isTRUE(sata)) {
        x@metadata@sata[[v]] <- TRUE
      } else {
        x@metadata@sata[[v]] <- NULL
      }
    } else {
      attr(x[[v]], "sata") <- if (isTRUE(sata)) TRUE else NULL
    }
  }

  invisible(x)
}


#' Extract SATA (Select-All-That-Apply) Flags
#'
#' Returns the SATA status for one or more variables in a survey design object
#' or a data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], etc. If empty, returns
#'   SATA status for all columns of `x`.
#' @param format `character(1)`. Output format: `"named_vector"` (default),
#'   `"list"`, or `"data_frame"`.
#' @param fill `FALSE` (default) or `NULL`. Controls how unmarked variables
#'   are reported. `FALSE` includes them in the result with value `FALSE`
#'   (dense view); `NULL` omits them (sparse view). `TRUE` and other values
#'   are rejected.
#'
#' @return
#' - `"named_vector"` (default): named logical vector. Empty: `logical(0)`.
#' - `"list"`: named list of logical scalars. Empty: `list()`.
#' - `"data_frame"`: tibble with columns `variable` (character) and `sata`
#'   (logical). Empty: zero-row tibble.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_sata(d, riagendr)
#' extract_sata(d, riagendr)
#' extract_sata(d, fill = NULL)
#' @seealso [set_sata()] to set SATA flags
#' @family metadata
#' @export
extract_sata <- function(x, ..., format = "named_vector", fill = FALSE) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  if (!is.null(fill) && !identical(fill, FALSE)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg fill} must be {.code FALSE} or {.code NULL}."
        )
      ),
      class = "surveycore_error_fill_not_logical",
      call = call
    )
  }

  .check_extractor_format(
    format,
    "extract_sata",
    c("named_vector", "list", "data_frame"),
    call
  )

  var_names <- if (...length() == 0L) {
    .get_data_cols(x)
  } else {
    names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  }

  result <- stats::setNames(
    vapply(
      var_names,
      function(v) {
        raw <- if (S7::S7_inherits(x, survey_base)) {
          x@metadata@sata[[v]]
        } else {
          attr(x[[v]], "sata", exact = TRUE)
        }
        isTRUE(raw)
      },
      logical(1L)
    ),
    var_names
  )

  if (is.null(fill)) {
    result <- result[result]
  }

  .format_logical_result(result, format)
}


# ── set_higher_is() / extract_higher_is() ─────────────────────────────────────

#' Set Direction-of-Improvement Attribute
#'
#' Records whether a higher value is better (`"better"`) or worse (`"worse"`)
#' for one or more variables in a survey design object or data frame. This
#' metadata is used by [get_diffs()] when `show_favorability = TRUE`.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... Named arguments where the name is the variable and the value is
#'   the direction (`"better"` or `"worse"`). Supports Convention 1 (named
#'   args: `bpxsy1 = "worse"`) and Convention 2 (named character vector:
#'   `c(bpxsy1 = "worse", lbxtc = "better")`). Mutually exclusive with
#'   `variable`.
#' @param variable `character`. Variable name(s) — Convention 3 alternative
#'   to `...`. Mutually exclusive with `...`.
#' @param direction `character`. Direction value(s) for Convention 3. Must be
#'   `"better"`, `"worse"`, or `NULL` (to remove the attribute). Same length
#'   as `variable`, or `NULL` to remove.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_higher_is(d, bpxsy1 = "worse")
#' extract_higher_is(d, bpxsy1)
#' @seealso [extract_higher_is()] to retrieve direction attributes
#' @family metadata
#' @export
set_higher_is <- function(x, ..., variable = NULL, direction = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots <- rlang::list2(...)
  dots_used <- length(dots) > 0L
  var_used <- !is.null(variable)

  if (dots_used && var_used) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        )
      ),
      class = "surveycore_error_higher_is_ambiguous_input",
      call = call
    )
  }

  if (!dots_used && !var_used) {
    cli::cli_abort(
      c("x" = "{.fn set_higher_is} requires at least one variable name."),
      class = "surveycore_error_higher_is_no_vars",
      call = call
    )
  }

  # Convention 3 pre-check: scalar direction validation before parse
  if (
    var_used &&
      length(direction) == 1L &&
      !is.null(direction) &&
      !direction %in% c("better", "worse")
  ) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg direction} must be {.val \"better\"} or {.val \"worse\"} ",
          "(or {.code NULL} to remove). Got {.val {direction}}."
        )
      ),
      class = "surveycore_error_direction_invalid",
      call = call
    )
  }

  pairs <- .parse_setter_input(
    dots = dots,
    variable = variable,
    content = direction,
    content_arg_name = "direction",
    content_type = "scalar",
    fn_name = "set_higher_is",
    call = call
  )

  all_cols <- .get_data_cols(x)

  for (v in names(pairs)) {
    if (!v %in% all_cols) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "1 variable not found in {.arg x} and was skipped: {.field {v}}."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
      next
    }
    val <- pairs[[v]]
    if (!is.null(val) && !val %in% c("better", "worse")) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg direction} must be {.val \"better\"} or {.val \"worse\"} ",
            "(or {.code NULL} to remove). Got {.val {val}}."
          )
        ),
        class = "surveycore_error_direction_invalid",
        call = call
      )
    }
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@higher_is[[v]] <- val
    } else {
      attr(x[[v]], "higher_is") <- val
    }
  }

  invisible(x)
}


#' Extract Direction-of-Improvement Attributes
#'
#' Returns the direction-of-improvement (`"better"` or `"worse"`) for one or
#' more variables in a survey design object or data frame. Variables with no
#' direction set return `NA_character_`.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   If empty, returns direction for all columns of `x`. Mutually exclusive
#'   with `variable`.
#' @param variable `character`. Variable name(s) — alternative to `...`.
#'   Mutually exclusive with `...`.
#'
#' @return A named character vector. Unset variables return `NA_character_`.
#'   Returns `character(0)` (named, zero-length) when all specified variables
#'   are missing from `x`.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_higher_is(d, bpxsy1 = "worse")
#' extract_higher_is(d, bpxsy1)
#' extract_higher_is(d)
#' @seealso [set_higher_is()] to set direction attributes
#' @family metadata
#' @export
extract_higher_is <- function(x, ..., variable = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots_used <- ...length() > 0L
  var_used <- !is.null(variable)

  if (dots_used && var_used) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        )
      ),
      class = "surveycore_error_higher_is_ambiguous_input",
      call = call
    )
  }

  all_cols <- .get_data_cols(x)

  if (dots_used) {
    var_names <- names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  } else if (var_used) {
    missing <- setdiff(variable, all_cols)
    if (length(missing) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{length(missing)} variable{?s} not found in {.arg x}",
            " and {?was/were} skipped: {.field {missing}}."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
    }
    var_names <- intersect(variable, all_cols)
  } else {
    var_names <- all_cols
  }

  out <- vapply(
    var_names,
    function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        x@metadata@higher_is[[v]] %||% NA_character_
      } else {
        attr(x[[v]], "higher_is", exact = TRUE) %||% NA_character_
      }
    },
    character(1L)
  )

  if (length(out) == 0L) {
    out <- unname(out)
  }
  out
}


# ── set_reverse_coded() / extract_reverse_coded() ─────────────────────────────

#' Set Reverse-Coded Flag
#'
#' Marks one or more variables as reverse-coded in a survey design object or
#' data frame. Uses the same two-convention pattern as [set_sata()].
#'
#' **Convention A (tidy-select `...`)** — recommended:
#' ```r
#' design |> set_reverse_coded(anxiety, worry)
#' ```
#'
#' **Convention B (`variable` = character vector)** — programmatic:
#' ```r
#' vars <- c("anxiety", "worry")
#' design |> set_reverse_coded(variable = vars)
#' ```
#'
#' Setting `reverse_coded = FALSE` removes the flag.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to mark.
#'   Cannot be combined with `variable`.
#' @param variable `character`. Alternative programmatic interface: character
#'   vector of variable names. Cannot be combined with `...`.
#' @param reverse_coded `logical(1)`. `TRUE` (default) marks variables as
#'   reverse-coded; `FALSE` removes the flag. `NA` is not accepted.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_reverse_coded(d, bpxsy1, ridageyr)
#' d <- set_reverse_coded(d, bpxsy1, reverse_coded = FALSE)
#' @seealso [extract_reverse_coded()] to retrieve reverse-coded flags
#' @family metadata
#' @export
set_reverse_coded <- function(x, ..., variable = NULL, reverse_coded = TRUE) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  if (
    !is.logical(reverse_coded) ||
      length(reverse_coded) != 1L ||
      is.na(reverse_coded)
  ) {
    cli::cli_abort(
      c("x" = "{.arg reverse_coded} must be {.code TRUE} or {.code FALSE}."),
      class = "surveycore_error_reverse_coded_not_logical",
      call = call
    )
  }

  dots_used <- ...length() > 0L
  var_used <- !is.null(variable)

  if (dots_used && var_used) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        )
      ),
      class = "surveycore_error_reverse_coded_ambiguous_input",
      call = call
    )
  }

  if (!dots_used && (!var_used || length(variable) == 0L)) {
    cli::cli_abort(
      c("x" = "{.fn set_reverse_coded} requires at least one variable name."),
      class = "surveycore_error_reverse_coded_no_vars",
      call = call
    )
  }

  all_cols <- .get_data_cols(x)

  if (dots_used) {
    var_names <- names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  } else {
    missing <- setdiff(variable, all_cols)
    if (length(missing) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{length(missing)} variable{?s} not found in {.arg x}",
            " and {?was/were} skipped: {.field {missing}}."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
    }
    var_names <- intersect(variable, all_cols)
  }

  for (v in var_names) {
    if (S7::S7_inherits(x, survey_base)) {
      x@metadata@reverse_coded[[v]] <- if (isTRUE(reverse_coded)) TRUE else NULL
    } else {
      attr(x[[v]], "reverse_coded") <- if (isTRUE(reverse_coded)) TRUE else NULL
    }
  }

  invisible(x)
}


#' Extract Reverse-Coded Flags
#'
#' Returns the reverse-coded status for one or more variables in a survey
#' design object or data frame.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to query.
#'   If empty, returns reverse-coded status for all columns of `x`.
#'   Cannot be combined with `variable`.
#' @param variable `character`. Alternative programmatic interface: character
#'   vector of variable names. Cannot be combined with `...`.
#'
#' @return A named logical vector. Variables not marked as reverse-coded return
#'   `FALSE`. When all specified variables are missing, returns `logical(0)`.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_reverse_coded(d, bpxsy1)
#' extract_reverse_coded(d, bpxsy1)
#' extract_reverse_coded(d)
#' @seealso [set_reverse_coded()] to set reverse-coded flags
#' @family metadata
#' @export
extract_reverse_coded <- function(x, ..., variable = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots_used <- ...length() > 0L
  var_used <- !is.null(variable)

  if (dots_used && var_used) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        )
      ),
      class = "surveycore_error_reverse_coded_ambiguous_input",
      call = call
    )
  }

  all_cols <- .get_data_cols(x)

  if (dots_used) {
    var_names <- names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  } else if (var_used) {
    missing <- setdiff(variable, all_cols)
    if (length(missing) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{length(missing)} variable{?s} not found in {.arg x}",
            " and {?was/were} skipped: {.field {missing}}."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
    }
    var_names <- intersect(variable, all_cols)
  } else {
    var_names <- all_cols
  }

  out <- vapply(
    var_names,
    function(v) {
      if (S7::S7_inherits(x, survey_base)) {
        isTRUE(x@metadata@reverse_coded[[v]])
      } else {
        isTRUE(attr(x[[v]], "reverse_coded", exact = TRUE))
      }
    },
    logical(1L)
  )

  if (length(out) == 0L) {
    out <- unname(out)
  }
  out
}


# ── classify_question_type() — variable type classifier ──────────────────────

#' Classify Variable Question Types
#'
#' Groups variables by their shared `question_preface` metadata and classifies
#' each group as one of `"single"`, `"sata"`, or `"battery"`. This is the single
#' source of truth used by downstream export functions to decide how to render
#' each question.
#'
#' The classification rules, applied per requested variable:
#'
#' 1. If the variable has no `question_preface`, or is the only requested
#'    variable sharing its preface, `type = "single"`.
#' 2. If a `question_preface` is shared by 2+ requested variables and at least
#'    one is flagged via [set_sata()], all variables in that group get
#'    `type = "sata"`.
#' 3. Otherwise (shared preface, no SATA flag), all variables in the group
#'    get `type = "battery"`.
#'
#' Group numbers are assigned sequentially by first appearance in the input.
#'
#' @param x A survey design object or `data.frame`.
#' @param ... <[`tidy-select`][tidyselect::language]> Variables to classify.
#'   Supports selection helpers: [tidyselect::starts_with()],
#'   [tidyselect::all_of()], [tidyselect::any_of()], etc. Cannot be combined
#'   with `variable`.
#' @param variable `character`. Alternative programmatic interface: character
#'   vector of variable names. Cannot be combined with `...`.
#'
#' @return A tibble with columns:
#' - `variable` (character) — variable name
#' - `question_preface` (character) — the preface, or `NA` if none
#' - `type` (character) — one of `"single"`, `"sata"`, or `"battery"`
#' - `group` (integer) — group id; variables with the same non-NA preface
#'   share a group
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' d <- set_question_preface(
#'   d,
#'   riagendr = "Demographics",
#'   ridageyr = "Demographics"
#' )
#' d <- set_sata(d, riagendr, ridageyr)
#' classify_question_type(d, riagendr, ridageyr, bpxsy1)
#' @seealso [set_sata()], [extract_sata()], [set_question_preface()]
#' @family metadata
#' @export
classify_question_type <- function(x, ..., variable = NULL) {
  call <- rlang::caller_env()
  .check_is_survey_or_df(x, call = call)

  dots_used <- ...length() > 0L
  var_used <- !is.null(variable)

  if (dots_used && var_used) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Provide variable names via {.arg ...} or via ",
          "{.arg variable}, not both."
        )
      ),
      class = "surveycore_error_detect_ambiguous_input",
      call = call
    )
  }

  if (!dots_used && (!var_used || length(variable) == 0L)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn classify_question_type} requires at least one variable name."
        )
      ),
      class = "surveycore_error_detect_no_vars",
      call = call
    )
  }

  all_cols <- .get_data_cols(x)

  if (dots_used) {
    var_names <- names(tidyselect::eval_select(
      rlang::expr(c(...)),
      data = .get_data_for_select(x)
    ))
  } else {
    missing <- setdiff(variable, all_cols)
    if (length(missing) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{length(missing)} variable{?s} not found in {.arg x}",
            " and {?was/were} skipped: {.field {missing}}."
          )
        ),
        class = "surveycore_warning_var_not_found",
        call = call
      )
    }
    var_names <- intersect(variable, all_cols)
  }

  empty_out <- tibble::tibble(
    variable = character(0L),
    question_preface = character(0L),
    type = character(0L),
    group = integer(0L)
  )
  if (length(var_names) == 0L) {
    return(empty_out)
  }

  # Extract preface + sata for each requested variable
  is_survey <- S7::S7_inherits(x, survey_base)
  prefaces <- vapply(
    var_names,
    function(v) {
      raw <- if (is_survey) {
        x@metadata@question_prefaces[[v]]
      } else {
        attr(x[[v]], "question_preface", exact = TRUE)
      }
      if (is.null(raw) || !is.character(raw) || length(raw) == 0L) {
        NA_character_
      } else {
        raw[[1L]]
      }
    },
    character(1L)
  )
  sata_flags <- vapply(
    var_names,
    function(v) {
      raw <- if (is_survey) {
        x@metadata@sata[[v]]
      } else {
        attr(x[[v]], "sata", exact = TRUE)
      }
      isTRUE(raw)
    },
    logical(1L)
  )

  # Group numbering: sequential by first appearance of each non-NA preface.
  # Variables with NA preface each get their own unique group (one row each).
  # Variables that share a non-NA preface share a group number.
  group_ids <- integer(length(var_names))
  preface_to_group <- list()
  next_group <- 1L
  for (i in seq_along(var_names)) {
    p <- prefaces[[i]]
    if (is.na(p)) {
      group_ids[[i]] <- next_group
      next_group <- next_group + 1L
    } else {
      existing <- preface_to_group[[p]]
      if (is.null(existing)) {
        preface_to_group[[p]] <- next_group
        group_ids[[i]] <- next_group
        next_group <- next_group + 1L
      } else {
        group_ids[[i]] <- existing
      }
    }
  }

  # Classify each group
  types <- character(length(var_names))
  for (g in unique(group_ids)) {
    idx <- which(group_ids == g)
    group_prefs <- prefaces[idx]
    group_sata <- sata_flags[idx]
    if (length(idx) == 1L) {
      # Singleton group
      types[[idx]] <- "single"
      # Edge case: SATA-flagged with a preface but no peers → warn
      if (isTRUE(group_sata[[1L]]) && !is.na(group_prefs[[1L]])) {
        var_name <- var_names[[idx]]
        cli::cli_warn(
          c(
            "!" = paste0(
              "Variable {.field {var_name}} is marked SATA but has no",
              " shared {.code question_preface} with other variables.",
              " Classified as {.val single}."
            )
          ),
          class = "surveycore_warning_sata_no_preface",
          call = call
        )
      }
    } else {
      # Shared-preface group: sata if any flag is TRUE, else battery
      if (any(group_sata)) {
        types[idx] <- "sata"
        if (!all(group_sata)) {
          preface <- group_prefs[[1L]]
          cli::cli_warn(
            c(
              "!" = paste0(
                "Variables sharing {.code question_preface} {.val {preface}}",
                " have mixed SATA status. Treating entire group as",
                " {.val sata}. Use {.fn set_sata} to mark all variables",
                " in the group."
              )
            ),
            class = "surveycore_warning_sata_mixed_group",
            call = call
          )
        }
      } else {
        types[idx] <- "battery"
      }
    }
  }

  # Renumber groups to ensure contiguous 1..n in first-appearance order
  # (they already are by construction, but unname for cleanliness).
  tibble::tibble(
    variable = unname(var_names),
    question_preface = unname(prefaces),
    type = types,
    group = group_ids
  )
}


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Validate Value Labels Against Observed Data Values
#'
#' Checks whether all observed (non-NA) values of a variable have a
#' corresponding label. Extra labels (for values not in the data) are silently
#' allowed — they document the full coding scheme. A warning is issued when
#' some observed values lack a label.
#'
#' @param var A vector of data values (one column from `@data`).
#' @param labels A fully named vector of value labels.
#' @param var_name Character string naming the variable (for warning messages).
#' @param strict Logical. If `TRUE`, error instead of warn on missing labels.
#'   Default `FALSE`.
#'
#' @return `invisible(TRUE)`.
#' @keywords internal
#' @noRd
.validate_val_labels <- function(var, labels, var_name, strict = FALSE) {
  unique_vals <- unique(var[!is.na(var)])
  # label_vals: the DATA CODES covered by labels (the vector VALUES, not names)
  label_vals <- as.character(labels)
  missing <- setdiff(as.character(unique_vals), label_vals)

  if (length(missing) > 0L) {
    if (strict) {
      cli::cli_abort(
        c(
          "x" = "Not all values of {.field {var_name}} are labeled.",
          "i" = "Unlabeled values: {.val {missing}}"
        ),
        class = "surveycore_error_missing_labels"
      )
    } else {
      cli::cli_warn(
        c(
          "!" = "Not all values of {.field {var_name}} are labeled.",
          "i" = "Unlabeled values: {.val {missing}}"
        ),
        class = "surveycore_warning_missing_labels"
      )
    }
  }

  invisible(TRUE)
}


#' Extract haven-Style Metadata from a Data Frame
#'
#' Scans each column of `data` for `"label"`, `"labels"`, and
#' `"question_preface"` attributes set by haven or by
#' `infer_question_prefaces()` (or any tool following the same convention).
#' Does NOT import haven — uses only base R `attr()`.
#'
#' Edge cases handled:
#' 1. Zero-length or empty-string variable labels — not stored.
#' 2. `NA` as a key in value labels — preserved (SPSS user-defined missing).
#' 3. Empty value-label vectors — not stored.
#' 4. Columns with no attributes — silently skipped.
#'
#' @param data A `data.frame`.
#' @return A [survey_metadata] object. All properties are empty lists if no
#'   haven attributes are found.
#' @keywords internal
#' @noRd
.extract_haven_metadata <- function(data) {
  var_labels <- list()
  val_labels <- list()
  q_prefaces <- list()
  notes <- list()
  universe <- list()
  missing_codes <- list()

  for (col_name in names(data)) {
    col <- data[[col_name]]

    # ── Variable label ────────────────────────────────────────────────────────
    # haven: attr(x, "label") — a length-1 character string
    var_lbl <- attr(col, "label", exact = TRUE)
    if (
      !is.null(var_lbl) &&
        is.character(var_lbl) &&
        length(var_lbl) > 0L &&
        nzchar(var_lbl[[1L]])
    ) {
      var_labels[[col_name]] <- var_lbl[[1L]]
    }

    # ── Value labels ──────────────────────────────────────────────────────────
    # haven: attr(x, "labels") — a named numeric or character vector
    val_lbl <- attr(col, "labels", exact = TRUE)
    if (
      !is.null(val_lbl) &&
        length(val_lbl) > 0L &&
        !is.null(names(val_lbl))
    ) {
      # NA keys (SPSS user-defined missing values) are preserved as-is
      val_labels[[col_name]] <- val_lbl
    }

    # ── Question preface (set by infer_question_prefaces() on data frames) ────
    q_preface <- attr(col, "question_preface", exact = TRUE)
    if (
      !is.null(q_preface) &&
        is.character(q_preface) &&
        nzchar(q_preface[[1L]])
    ) {
      q_prefaces[[col_name]] <- q_preface[[1L]]
    }

    # ── Analyst note ──────────────────────────────────────────────────────────
    var_note <- attr(col, "note", exact = TRUE)
    if (
      !is.null(var_note) &&
        is.character(var_note) &&
        nzchar(var_note[[1L]])
    ) {
      notes[[col_name]] <- var_note[[1L]]
    }

    # ── Universe description ──────────────────────────────────────────────────
    var_universe <- attr(col, "universe", exact = TRUE)
    if (
      !is.null(var_universe) &&
        is.character(var_universe) &&
        nzchar(var_universe[[1L]])
    ) {
      universe[[col_name]] <- var_universe[[1L]]
    }

    # ── Missing codes ─────────────────────────────────────────────────────────
    var_missing <- attr(col, "missing_codes", exact = TRUE)
    if (
      !is.null(var_missing) &&
        is.atomic(var_missing) &&
        length(var_missing) > 0L
    ) {
      missing_codes[[col_name]] <- var_missing
    }
  }

  survey_metadata(
    variable_labels = var_labels,
    value_labels = val_labels,
    question_prefaces = q_prefaces,
    notes = notes,
    universe = universe,
    missing_codes = missing_codes
  )
}
