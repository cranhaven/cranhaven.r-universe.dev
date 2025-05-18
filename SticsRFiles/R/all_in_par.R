
#' Return all possible STICS inputs parameters
#'
#' @description Helper function to print the list of all possible parameters
#' to set as input from the STICS model.
#'
#' @param stics_version Name of the STICS version. Optional, can be used to
#' search parameters information relative to a specific STICS version.
#' By default the latest version returned by `get_stics_versions_compat()`
#' is used.
#'
#' @seealso `get_param_info()` and `get_stics_versions_compat()`
#'
#' @examples
#' \dontrun{
#' all_in_par()
#' }
#' @keywords internal
#'
#' @noRd
#'
all_in_par <- function(stics_version = "latest") {

  # Checking and getting the right version
  stics_version <- check_version_compat(stics_version = stics_version)

  #  if (get_version_num(stics_version = stics_version) < 9.2) {
  #    cols_idx <- 1:4
  #  } else {
  cols_idx <- c(1, 4, 7:8,2)
  #  }

  par_df <- utils::read.csv2(
    file.path(
      get_examples_path(file_type = "csv",
                        stics_version = stics_version),
      "inputs.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )[, cols_idx]

  names(par_df) <- c("name", "file", "min", "max", "definition")

  # Adding a version  attribute
  attr(x = par_df, which = "version") <- stics_version
  return(par_df)
}


#' Finding parameters information using partial search words
#'
#' @description Helper function that returns names and descriptions of
#' STICS input parameters from a partial name and/or descriptive keywords.
#'
#' @param param Vector of parameter names (or partial names). Optional, if not
#' provided, the function returns information for all parameters
#'
#' @param keyword Optional, strings or a vector of to be used for searching
#' in parameters names and definition
#'
#' @param stics_version Name of the STICS version. Optional, can be used
#' to search parameters information relative to a specific STICS version.
#' By default the latest version returned by `get_stics_versions_compat()`
#' is used.
#'
#' @details The function understand \code{\link[base]{regex}} as input.
#'
#' @return A data.frame with information about parameter(s) with columns
#'        `name`,`file`,`min`,`max`, `definition`
#'
#'
#' @examples
#'
#' # Find by parameter name (fuzzy search):
#' SticsRFiles::get_param_info("alb")
#' SticsRFiles::get_param_info("alb[e]?")
#'
#' # Find by keyword (fuzzy search in parameter name and description):
#' SticsRFiles::get_param_info(keyword = "bdil")
#'
#' # Find for a particular version:
#' SticsRFiles::get_param_info("alb", stics_version = "V9.0")
#'
#'
#' @export
#'
get_param_info <- function(param = NULL,
                           keyword = NULL,
                           stics_version = "latest") {

  all_pars <- all_in_par(stics_version)

  if (!is.null(keyword)) {
    keyword_param <- unique(c(param, keyword))

    idx <- get_idx_matches(keyword_param, all_pars$definition)
    idx <- unique(idx, get_idx_matches(keyword_param, all_pars$name))

    return(all_pars[idx, ])
  }

  if (!is.null(param)) {
    param <- var_to_col_names(param)
    pars_names_parsed <- var_to_col_names(all_pars$name)

    idx <- get_idx_matches(param, pars_names_parsed)
    return(all_pars[idx, ])
  }

  return(all_pars)
}

get_idx_matches <- function(string, names){
  unlist(lapply(string,
                function(x) {
                  grep(x, names, ignore.case = TRUE)
                }
  )
  )
}

#' Search if a STICS parameter exist
#'
#' @description Tells if one or more parameter names are valid STICS
#' input parameters.
#'
#' @param param     A vector of parameter names
#'
#' @param stics_version Name of the STICS version. Optional, can be used
#' to search parameters information relative to a specific STICS version.
#' By default the latest version returned by `get_stics_versions_compat()`
#' is used.
#'
#' @return A boolean vector: `TRUE` if the parameter exist, `FALSE` otherwise
#'
#' @seealso `get_param_info()` for interactive use.
#'
#' @export
#'
#' @examples
#' is_stics_param(c("adil", "adilmax", "unknown"))
#'
is_stics_param <- function(param,
                           stics_version = "latest") {


  all_pars <- all_in_par(stics_version)
  par_parsed <- var_to_col_names(param)
  pars_names_parsed <- var_to_col_names(all_pars$name)
  index_par <- match(par_parsed, pars_names_parsed)
  par_found <- !is.na(index_par)
  if (any(!par_found)) {
    cli::cli_alert_warning(
      paste0("paremeters{?s} {.var {par_parsed[!par_found]}}",
             " not found. Try {.code get_param_info()}.")
    )
  }
  return(par_found)
}
