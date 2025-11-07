#' Get a user-friendly display name
#'
#' This function provides a user-friendly name for a column based on a mapping table, if available.
#'
#' @param col_name A string specifying the name of the column.
#' @param mapping_table A named list with as name the original colum name and as value the display
#' name
#' @return A string containing the user-friendly name for the column.
#' @export
#' @examples
#'   mapping <- list(
#' col1 = "Column 1",
#' col2 = "Column 2"
#' )
#' display_name("col1", mapping)
display_name <- function(col_name, mapping_table) {
  if (col_name %in% names(mapping_table)) {
    col_name <- mapping_table[[col_name]]
  }

  return(col_name)
}


#' Quietly run a function
#'
#' This function is a wrapper that allows a function to be run quietly without the need to create a separate quiet function.
#'
#' @param func The function to be run.
#' @param ... Optional further arguments passed to the 'func' function.
#' @return The list result of the 'func' function with messages, warnings, and output captured.
#' @export
#' @examples
#' warning_func_arugment <- function(info) {
#'   warning(info)
#'   return("Complete")
#' }
#' result <- quietly_run(warning_func_arugment, "Just checking")
quietly_run <- function(func, ...) {
  args <- list(...)

  quietly_func <- purrr::quietly(func)

  if(length(args) == 0) {
    # No arguments passed, just call func()
    quietly_func()
  } else {
    quietly_func(...)
  }

}
