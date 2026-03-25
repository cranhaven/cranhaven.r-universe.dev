#' Generic function for a basic filter, parametrised over the type of
#' comparison operator used to select rows. The values to be taken are
#' dates instead of numbers, though.
#'
#' @param table A data frame
#' @param filter_obj A list containing the following elements:
#'               - type: must be 'date_in', 'date_lt', 'date_lt_eq', 'date_gt',
#'                       or 'date_gt_eq' (case-insensitive)
#'               - column: the name of the column to filter on
#'               - value: a vector of values to filter on (for type 'in'), or
#'                 a single value (for all other types)
#' @param context A string to be used in logging or error messages. Defaults to
#' NULL.
#'
#' @return A list with the following elements:
#'               - passed: data frame with the rows that passed the filter
#'               - rejected: all other rows
#' @noRd
filter_basic_date <- function(table,
                              filter_obj,
                              context = NULL) {
  context <- c(context, "filter_basic_date")
  trace_context(context)

  t <- tolower(filter_obj$type)
  valid_filter_types <- c(
    "date_in", "date_lt",
    "date_lt_eq", "date_gt", "date_gt_eq"
  )
  if (!(t %in% valid_filter_types)) {
    stop_context(
      message = paste0(
        "Expected filter type to be one of ",
        paste(
          sapply(valid_filter_types, function(x) paste0("'", x, "'")),
          collapse = ", "
        ),
        ", but got '", filter_obj$type, "'."
      ),
      context = context
    )
  }

  column_name <- validate_filter_column(filter_obj, table, context)
  value <- validate_filter_date_value(filter_obj, table, context)
  operator <- switch(t,
    "date_in" = `%in%`,
    "date_lt" = `<`,
    "date_lt_eq" = `<=`,
    "date_gt" = `>`,
    "date_gt_eq" = `>=`
  )

  # Add a sentinel column indicating whether the row passed the filter
  private_sentinel <- "EIDER_PRIVATE_FILTERED"
  table <- table %>%
    mutate(
      !!private_sentinel :=
        operator(.data[[column_name]], value)
    )

  # Split the table into passed and rejected rows, and remove the sentinel
  # column
  list(
    passed = table %>%
      filter(.data[[private_sentinel]]) %>%
      select(-all_of(private_sentinel)),
    rejected = table %>%
      filter(!(.data[[private_sentinel]])) %>%
      select(-all_of(private_sentinel))
  )
}
