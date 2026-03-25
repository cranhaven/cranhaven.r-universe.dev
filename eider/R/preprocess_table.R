#' Perform (date) preprocessing on a given data table
#'
#' @param input_table The original input table which will be mutated
#' @param spec The specification which dictates the changes
#' @param context Execution context, defaulting to NULL
#'
#' @return A mutated version of the initial input table
#' @noRd
preprocess_table <- function(input_table, spec, context = NULL) {
  context <- c(context, "preprocess_table")

  if ("preprocess" %in% names(spec)) {
    debug_context(
      context,
      message = paste0("Preprocessing table ", spec$source_table)
    )

    on <- validate_columns_present(
      "on", spec$preprocess, input_table, context
    )
    retain_min <- validate_columns_present(
      "retain_min", spec$preprocess, input_table, context
    )
    retain_max <- validate_columns_present(
      "retain_max", spec$preprocess, input_table, context
    )
    replace_with_sum <- validate_columns_present(
      "replace_with_sum", spec$preprocess, input_table, context
    )

    # Check that no column is present in multiple lists
    all_cols <- c(retain_min, retain_max, replace_with_sum)
    duplicate_cols <- all_cols[duplicated(all_cols)]
    if (length(duplicate_cols) > 0) {
      stop_context(
        context = context,
        message = paste0(
          "The column(s) '",
          paste(duplicate_cols, collapse = ", "),
          "' are present in multiple preprocessing steps. ",
          "Each column should only be present in one list."
        )
      )
    }

    input_table <- input_table %>% group_by(across(all_of(on)))

    for (col in retain_min) {
      input_table <- input_table %>%
        mutate(!!col := min(!!sym(col)))
    }

    for (col in retain_max) {
      input_table <- input_table %>%
        mutate(!!col := max(!!sym(col)))
    }

    for (col in replace_with_sum) {
      input_table <- input_table %>%
        mutate(!!col := sum(!!sym(col)))
    }

    input_table %>% ungroup()
  } else {
    debug_context(
      context,
      message = "No preprocessing specified for this feature"
    )
    input_table
  }
}
