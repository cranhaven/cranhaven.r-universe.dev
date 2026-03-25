#' Checks that `spec$source_table` is a single string and that it is a key in
#' `all_tables`. If so, returns the table itself (or the file path of the
#' table).
#' @noRd
validate_source_table <- function(spec, all_tables, context) {
  n <- spec$source_table

  if (!(is.character(n) && length(n) == 1)) {
    error_not_string(n, "source_table", context)
  }

  if (!(n %in% names(all_tables))) {
    stop_context(
      message = paste0(
        "The name '",
        n,
        "' supplied for 'source_table' was not provided as an input table."
      ),
      context = context
    )
  }

  all_tables[[n]]
}

#' Checks that `spec$output_feature_name` is a single string. If so, returns
#' the string.
#' @noRd
validate_output_feature_name <- function(spec, context) {
  n <- spec$output_feature_name

  if (!(is.character(n) && length(n) == 1)) {
    error_not_string(n, "output_feature_name", context)
  }

  n
}

#' Checks that `spec[[field_name]]` is a single string and that it is a column
#' in the table `tbl`. If so, returns the column name.
#' @noRd
validate_column_present <- function(field_name, spec, tbl, context) {
  n <- spec[[field_name]]

  if (!(is.character(n) && length(n) == 1)) {
    error_not_string(n, field_name, context)
  }

  if (!(n %in% names(tbl))) {
    stop_context(
      message = paste0(
        "The column '",
        n,
        "' supplied for '",
        field_name,
        "' was not found in the input table."
      ),
      context = context
    )
  }

  n
}

#' Same as validate_column_present, but additionally checks that the column has
#' a date type.
#' @noRd
validate_date_column_present <- function(field_name, spec, tbl, context) {
  n <- validate_column_present(field_name, spec, tbl, context)
  column <- tbl[[n]]

  if (!lubridate::is.Date(column)) {
    stop_context(
      message = paste0(
        "The column '",
        n,
        "' supplied for '",
        field_name,
        "' must be of type 'date'. However, the column is of type '",
        typeof(column),
        "'."
      ),
      context = context
    )
  }

  n
}

#' Same as validate_column_present, but allows for multiple (or no!) columns to
#' be specified.
#' @noRd
validate_columns_present <- function(field_name, spec, tbl, context) {
  ns <- spec[[field_name]]

  if (is.null(ns)) {
    return(NULL)
  }

  for (n in ns) {
    if (!(is.character(n))) {
      stop_context(
        message = paste0(
          "The entries in '",
          field_name,
          "' must be strings, ",
          "but the value supplied (",
          n,
          ") is of type '",
          typeof(n),
          "'."
        ),
        context = context
      )
    }
  }

  for (n in ns) {
    if (!(n %in% names(tbl))) {
      stop_context(
        message = paste0(
          "The column '",
          n,
          "' supplied as part of '",
          field_name,
          "' was not found in the input table."
        ),
        context = context
      )
    }
  }

  ns
}


#' Checks that `spec$absent_default_value` is a single number. If so, returns
#' the number.
#' @noRd
validate_absent_default_value <- function(spec, context) {
  n <- spec$absent_default_value

  if (is.null(n)) {
    return(NA)
  }

  if (!(is.numeric(n) && length(n) == 1)) {
    stop_context(
      message = paste0(
        "If provided, 'absent_default_value' must be a single number. ",
        "However, the value supplied (",
        n,
        ") is of type '",
        typeof(n),
        "'."
      ),
      context = context
    )
  }

  n
}


#' Checks that the column specified by a filter object exists in a table. If
#' so, returns the column name.
#' @noRd
validate_filter_column <- function(filter_obj, tbl, context) {
  n <- filter_obj$column

  if (!(is.character(n) && length(n) == 1)) {
    error_not_string(n, "column", context)
  }

  if (!n %in% colnames(tbl)) {
    stop_context(
      message = paste0(
        "The column '",
        n,
        "' to be filtered on was not found in the table."
      ),
      context = context
    )
  }

  n
}

#' Checks that the values specified by a filter object are of the same type as
#' the column to be filtered on. If so, returns the values.
#' @noRd
validate_filter_value <- function(filter_obj, table, context) {
  v <- filter_obj$value
  val_type <- typeof(v)

  # NOTE: It is assumed here that the column exists in the table because it has
  # been verified by `validate_filter_column`
  column_name <- filter_obj$column
  column <- table[[column_name]]
  col_type <- typeof(column)

  if (length(v) == 0) {
    stop_context(
      message =
        "The 'value' field of a filter object must contain at least one item",
      context = context
    )
  }

  # To pass validation, we require any of the following:
  #  - the column is all NAs (in this case it will be read as 'logical')
  #  - both value and column are numeric, or
  #  - value and column have exactly the same type
  compatible <- (
    all(is.na(column)) ||
      val_type == col_type ||
      is.numeric(v) && is.numeric(column)
  )
  if (!compatible) {
    stop_context(
      message = paste0(
        "The 'value' field of a filter object must be of the same type as ",
        "the column to be filtered on. However, the column '",
        column_name,
        "' is of type '",
        col_type,
        "', while the value given is of type '",
        val_type,
        "'."
      ),
      context = context
    )
  }

  v
}

#' @noRd
validate_filter_date_value <- function(filter_obj, table, context) {
  v <- filter_obj$value
  val_type <- typeof(v)

  # NOTE: It is assumed here that the column exists in the table because it has
  # been verified by `validate_filter_column`
  column_name <- filter_obj$column
  column <- table[[column_name]]
  col_type <- typeof(column)

  if (length(v) == 0) {
    stop_context(
      message =
        "The 'value' field of a filter object must contain at least one item",
      context = context
    )
  }

  ymd_with_check <- function(v) {
    v2 <- lubridate::ymd(v)
    if (is.na(v2)) {
      stop_context(
        message = paste0(
          "The 'value' field of a date filter object must be a date in the ",
          "format 'YYYY-MM-DD'. However, the value supplied (",
          v,
          ") could not be parsed as a valid date."
        ),
        context = context
      )
    }
    v2
  }

  # Check that the column consists of dates or NAs only
  if (!all(sapply(column, function(x) is.na(x) || lubridate::is.Date(x)))) {
    stop_context(
      message = paste0(
        "The 'column' field of a date filter object must refer to a ",
        "column which is of type 'date'. However, the column '",
        column_name,
        "' is of type '",
        col_type,
        "'."
      ),
      context = context
    )
  }

  purrr::map_vec(v, ymd_with_check)
}

#' @noRd
validate_weight <- function(spec, context) {
  w <- spec$weight

  if (!(is.numeric(w) && length(w) == 1)) {
    error_not_number(w, "weight", context)
  }

  w
}

#' @noRd
error_not_string <- function(value, name, context) {
  stop_context(
    message = paste0(
      "'",
      name,
      "' must be a single string, ",
      "but the value supplied (",
      value,
      ") is of type '",
      typeof(value),
      "'."
    ),
    context = context
  )
}

#' @noRd
error_not_number <- function(value, name, context) {
  stop_context(
    message = paste0(
      "'",
      name,
      "' must be a single number, ",
      "but the value supplied (",
      value,
      ") is of type '",
      typeof(value),
      "'."
    ),
    context = context
  )
}
