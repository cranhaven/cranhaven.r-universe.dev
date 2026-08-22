#' @title Validates Inputs For [mab_from_rct()]
#' @name check_rct_args
#' @description This function checks to ensure that all required arguments
#' have been properly passed to [mab_from_rct()] before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose = TRUE`, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @returns Throws an error if an argument is missing or misspecified.
#' @keywords internal
#' @family checks
check_rct_args <- function(
  data,
  control_augment,
  random_assign_prop,
  period_method,
  time_unit,
  period_length,
  prior_periods,
  discount_rate,
  delayed_feedback,
  whole_experiment,
  col_names,
  verbose,
  ndraws,
  r,
  keep_data,
  keep_models,
  blocking
) {
  col_conflict_check(data = data)

  check_logical(
    verbose,
    whole_experiment,
    delayed_feedback,
    keep_data,
    keep_models
  )

  check_cols(
    data = data,
    period_method = period_method,
    time_unit = time_unit,
    delayed_feedback = delayed_feedback,
    col_names = col_names,
    verbose = verbose,
    blocking = blocking
  )

  check_period_method(
    period_method = period_method,
    time_unit = time_unit,
    verbose = verbose,
    period_length = period_length
  )

  check_prop(control_augment, random_assign_prop, discount_rate)
  check_posint(r, ndraws, prior_periods)

  check_data(
    data = data,
    col_names = col_names,
    period_method = period_method,
    period_length = period_length,
    time_unit = time_unit,
    delayed_feedback
  )
}
#---------------------------------------------------------------------------------------
#' @describeIn check_rct_args Helper to [check_rct_args()]. This function accepts the user's
#' settings for the Multi-Arm-Bandit trial, and checks whether columns in the data have been properly
#' specified based on these settings.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @returns Throws an error if columns which are required have not been declared
#' or are not present in the data, or are the wrong primitive data type. Additionally throws warning messages,
#' if unnecessary columns have been provided, only when `verbose = TRUE`.
#' @keywords internal
#'
check_cols <- function(
  period_method,
  time_unit,
  delayed_feedback,
  col_names,
  data,
  verbose,
  blocking
) {
  all_cols <- c(
    "success_col",
    "condition_col",
    "date_col",
    "month_col",
    "success_date_col",
    "assignment_date_col",
    "block_cols",
    "cluster_col"
  )

  all_reasons <- list(
    success_col = "it is always required",
    condition_col = "it is always required",
    date_col = "period_method is 'date'",
    month_col = "time_unit is 'month' and you provided a `month_col`",
    success_date_col = "delayed_feedback is TRUE",
    assignment_date_col = "delayed_feedback is TRUE",
    cluster_col = "it is always required when provided in `formula`"
  )
  data_types <- c(
    "numeric",
    "logical",
    "integer",
    "character",
    "factor",
    "Date",
    "POSIXt"
  )
  test_funcs <- c(
    is.numeric,
    is.logical,
    is.character,
    is.factor,
    lubridate::is.Date,
    lubridate::is.POSIXt
  )
  required_types <- list(
    success_col = list(classes = data_types[1:3], tests = test_funcs[1:2]),
    condition_col = list(classes = data_types[1:5], tests = test_funcs[1:4]),
    date_col = list(classes = data_types[6:7], tests = test_funcs[5:6]),
    month_col = list(
      classes = data_types[c(1, 3, 4, 5)],
      tests = test_funcs[c(1, 3, 4)]
    ),
    success_date_col = list(classes = data_types[6:7], tests = test_funcs[5:6]),
    assignment_date_col = list(
      classes = data_types[6:7],
      tests = test_funcs[5:6]
    ),
    cluster_col = list(
      classes = data_types[c(1, 3, 4, 5, 6)],
      tests = test_funcs[c(1, 3, 4, 5, 6)]
    )
  )

  required_cols <- c("success_col", "condition_col")

  if (period_method == "date") {
    required_cols <- c(required_cols, "date_col")
    if (time_unit == "month" && !is.null(col_names[["month_col"]])) {
      required_cols <- c(required_cols, "month_col")
    }
  }
  if (delayed_feedback) {
    required_cols <- c(required_cols, "success_date_col", "assignment_date_col")
  }
  if (!is.null(col_names[["cluster_col"]])) {
    required_cols <- c(required_cols, "cluster_col")
  }
  req_reasons <- all_reasons[required_cols]
  required_types <- required_types[required_cols]

  purrr::pwalk(
    list(required_cols, req_reasons, required_types),
    ~ {
      if (!..1 %in% names(col_names)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not declared in `col_names`.", ..1),
          "x" = sprintf("reason: %s", ..2)
        ))
      }
      provided_col <- col_names[[..1]]
      if (!provided_col %in% names(data)) {
        rlang::abort(c(
          sprintf("Required column `%s` is not found in provided `data`.", ..1),
          "x" = sprintf("reason: %s", ..2),
          "x" = sprintf("Your column: %s", provided_col)
        ))
      }
      data_type <- class(data[[col_names[[..1]]]])
      if (
        !any(vapply(
          ..3[["tests"]],
          \(fn) fn(data[[col_names[[..1]]]]),
          FUN.VALUE = logical(1)
        ))
      ) {
        rlang::abort(c(
          sprintf("Required column `%s` is the wrong data type.", ..1),
          "x" = sprintf("Your type: %s", paste(data_type, collapse = ", ")),
          "i" = sprintf(
            "Permissible types: %s",
            paste(..3[["classes"]], collapse = ", ")
          )
        ))
      }
    }
  )

  if (blocking) {
    purrr::walk(col_names[["block_cols"]], \(col) {
      if (!col %in% names(data)) {
        rlang::abort(sprintf(
          "`%s is not in the data, but was chosen as a block.",
          col
        ))
      }
    })
  }

  if (verbose) {
    non_required_cols <- setdiff(all_cols, required_cols)
    non_req_reasons <- list(
      date_col = "period_method is not 'date'",
      month_col = "time_unit is not 'month'",
      success_date_col = "delayed_feedback is FALSE",
      assignment_date_col = "delayed_feedback is FALSE"
    )
    non_req_reasons <- non_req_reasons[non_required_cols]

    purrr::iwalk(non_req_reasons, \(reason, col_name) {
      if (col_name %in% names(col_names)) {
        rlang::warn(c(
          "i" = sprintf(
            "`%s` is not required because %s. It will be ignored.",
            col_name,
            reason
          )
        ))
      }
    })
  }
}
#' @describeIn check_rct_args Throws an error if the provided dataset does not meet the specifications
#' of the trial based on user settings.
#' @returns Nothing; Throws an error if the provided dataset does not meet the specifications
#' of the trial based on user settings.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @keywords internal
check_data <- function(
  data,
  col_names,
  period_method,
  period_length,
  time_unit,
  delayed_feedback
) {
  if (period_method == "batch" && period_length > nrow(data)) {
    rlang::abort(c(
      "`period_length` cannot be larger than data size",
      "x" = sprintf(
        "Your data has %d rows, and your batch size is %d rows",
        nrow(data),
        period_length
      )
    ))
  }
  if (period_method == "date") {
    unit <- switch(
      time_unit,
      "day" = lubridate::days(1),
      "month" = months(1),
      "week" = lubridate::weeks(1)
    )

    data_interval <- lubridate::interval(
      min(data[[col_names[["date_col"]]]]),
      max(data[[col_names[["date_col"]]]])
    ) /
      unit
    data_interval <- round(data_interval, 0)

    if (period_length > data_interval) {
      rlang::abort(c(
        "`period_length` cannot be larger then the date range of your data",
        "x" = sprintf(
          "Your period length is %d %ss but your data only covers %d %ss",
          period_length,
          tolower(time_unit),
          data_interval,
          tolower(time_unit)
        )
      ))
    }
  }
}
# ----------------------------------------------------------------------------
#' @describeIn check_rct_args Helper to [check_rct_args()]. This function accepts arguments relating
#' to how treatment waves are assigned, and checks if they are valid, and if all
#' supporting arguments are passed as necessary.
#' @returns Throws an error if the user is missing necessary arguments to
#' assign treatments or passes invalid ones.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @keywords internal
check_period_method <- function(
  period_method,
  time_unit,
  verbose,
  period_length
) {
  if (period_method == "date") {
    if (
      is.null(time_unit) ||
        length(time_unit) != 1 ||
        isTRUE(is.na(time_unit))
    ) {
      rlang::abort(
        "`time_unit` must be provided when assignment method is `date`."
      )
    }
  }
  if (period_method %in% c("batch", "date")) {
    if (is.null(period_length)) {
      rlang::abort(c(
        "`period_length`, must be provided when date or batch based periods are used."
      ))
    }
    if (!posint(period_length)) {
      rlang::abort(c(
        "`period_length` must be a positive integer.",
        "x" = paste0("You passed: ", deparse(period_length))
      ))
    }
  }
  if (
    verbose &&
      !period_method %in% c("batch", "date") &&
      !is.null(time_unit)
  ) {
    rlang::warn(c(
      "i" = "`time_unit` is not required when assignment method is not `date`. It will be ignored"
    ))
  }
}

#' Check Conflicts with Internal Columns
#' @name col_conflict_check
#' @description Identifies column name conflicts between internal reserved columns and input data
#' for [mab_from_rct()]'s internal procedures
#'
#' @returns throws an error under any conflict
#' @seealso [mab_from_rct()]
#' @keywords internal
#'
col_conflict_check <- function(data) {
  reserved_cols <- reserved_cols()
  conflicts <- intersect(colnames(data), reserved_cols)
  if (length(conflicts) > 0) {
    rlang::abort(
      c(
        "`data` already contains column(s) reserved internally by `mab_from_rct()`.",
        "x" = paste0(
          "Conflicting Columns: ",
          paste0(conflicts, collapse = ", ")
        ),
        "i" = "Rename these columns before proceeding"
      ),
    )
  }
}


#' @rdname col_conflict_check
#' @returns a named character vector of reserved column names.
#' @keywords internal
reserved_cols <- function() {
  c(
    "mab_success",
    "mab_condition",
    "impute_req",
    "impute_block",
    "new_success_date",
    "block",
    "treatment_block",
    "period_number",
    "mab_assign_prob",
    "ipw_weights"
  )
}
