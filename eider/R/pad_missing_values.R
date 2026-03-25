#' Given an ID column in a table, and a missing value, appends id-value pairs
#' for all values that are not in a second table. It is assumed that the IDs
#' are added to the 'id' column in the second table.
#'
#' @param table The table containing all the values that should be present
#' @param id_col The name of the column containing the IDs
#' @param missing_value The value that should be present for all IDs
#' @param id_table The table to be padded
#'
#' @return A table like `id_table`, but with rows appended for each ID that is
#' present in `table` but not present in `id_table`.
#' @noRd
pad_missing_values <- function(
    table,
    id_col,
    missing_value,
    id_table,
    context) {
  debug_context(
    message = paste0(
      "Adding absent_default_value: ",
      missing_value,
      " to any IDs not present in the calculated feature"
    ),
    context = context
  )

  missing_ids <- setdiff(table[[id_col]], id_table$id)
  if (length(missing_ids) == 0) {
    return(id_table)
  }

  missing_table <- data.frame(id = missing_ids, value = missing_value)
  names(missing_table) <- names(id_table)
  tibble::tibble(rbind(id_table, missing_table))
}
