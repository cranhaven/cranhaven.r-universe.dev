
#' Return all possible STICS outputs for var.mod
#'
#' @description Helper function to print the list of all possible variables
#' to set as output from the STICS model.
#'
#' @param stics_version Name of the STICS version. Optional, can be used to
#' search parameters information relative to a specific STICS version.
#' By default the latest version returned by `get_stics_versions_compat()`
#' is used.
#'
#' @seealso `get_var_info()`, `gen_varmod()`, and `get_stics_versions_compat()`
#'
#' @examples
#' \dontrun{
#' all_out_var()
#' }
#' @keywords internal
#'
#' @noRd
#'
all_out_var <- function(stics_version = "latest") {

  # Checking and getting the right version
  stics_version <- check_version_compat(stics_version = stics_version)

  if (get_version_num(stics_version = stics_version) < 9.2) {
    cols_idx <- 1:4
  } else {
    cols_idx <- c(1:3, 5)
  }

  var_df <- utils::read.csv2(
    file.path(
      get_examples_path(file_type = "csv",
                        stics_version = stics_version),
      "outputs.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )[, cols_idx]

  names(var_df) <- c("name", "definition", "unit", "type")

  # Adding a version  attribute
  attr(x = var_df, which = "version") <- stics_version
  return(var_df)
}


#' Find STICS output variable names and description
#'
#' @description Helper function that returns names and descriptions of
#' STICS output variables from a partial name and/or descriptive keywords.
#'
#' @param var Vector of variable names (or partial names).
#' Optional, if not provided, the function returns information for
#' all variables.
#' @param keyword Search by keyword instead of variable name
#' (search in the name and description field)
#' @param stics_version Name of the STICS version. Optional, can be used
#' to search parameters information relative to a specific STICS version.
#' By default the latest version returned by `get_stics_versions_compat()`
#' is used.

#' @details The function understand \code{\link[base]{regex}} as input.
#'
#' @return A data.frame with information about variable(s) with columns
#'        `name`, `definition`, `unit`, `type`
#'
#' @examples
#'
#' # Find by variable name (fuzzy search):
#' SticsRFiles::get_var_info("lai")
#'
#' # Find by keyword (fuzzy search in variable name and description):
#' SticsRFiles::get_var_info(keyword = "lai")
#'
#' # Find for a particular version:
#' SticsRFiles::get_var_info("lai", stics_version = "V9.0")
#'
#'
#' @export
#'
get_var_info <- function(var = NULL,
                         keyword = NULL,
                         stics_version = "latest") {

  all_vars <- all_out_var(stics_version)
  if (!is.null(var)) {
    var <- var_to_col_names(var)
    vars_names_parsed <- var_to_col_names(all_vars$name)
    idx <- unlist(lapply(var,
                         function(x) {
                           grep(x, vars_names_parsed, ignore.case = TRUE)
                           }
                         )
                  )
    all_vars[idx, ]
  } else if (!is.null(keyword)) {
    idx <- grepl(keyword, all_vars$definition, ignore.case = TRUE)
    idx <- idx | grepl(keyword, all_vars$name, ignore.case = TRUE)
    all_vars[idx, ]
  } else {
    all_vars
  }
}

#' Search if a STICS variable exist
#'
#' @description Tells if one or more variable names are valid STICS
#' output variables.
#'
#' @param var     A vector of variable names
#' @param stics_version Name of the STICS version. Optional, can be used
#' to search parameters information relative to a specific STICS version.
#' By default the latest version returned by `get_stics_versions_compat()`
#' is used.
#'
#' @return A boolean vector: `TRUE` if the variable exist, `FALSE` otherwise
#'
#' @seealso `get_var_info()` for interactive use.
#'
#' @export
#'
#' @examples
#' is_stics_var(c("lai(n)", "masec(n)", "unknown"))
#'
is_stics_var <- function(var,
                         stics_version = "latest") {

  all_vars <- all_out_var(stics_version)
  var_parsed <- var_to_col_names(var)
  vars_names_parsed <- var_to_col_names(all_vars$name)

  index_var <- match(var_parsed, vars_names_parsed)
  var_found <- !is.na(index_var)
  if (any(!var_found)) {
    cli::cli_alert_warning(
      paste0("Variable{?s} {.var {var_parsed[!var_found]}}",
             " not found. Try {.code get_var_info()}.")
    )
  }
  return(var_found)
}
