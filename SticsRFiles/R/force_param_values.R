#' @title Generates files to force parameters values in STICS simulations
#' @description Generates a param.sti file and sets code optim in
#' new_travail.usm to force parameters values in STICS simulations
#' (this function is typically called before `SticsOnR::run_stics()`)
#' @param workspace Path of the workspace containing the STICS (txt)
#' input files.
#' @param values named vector of parameter values to force.
#' See Details for more information.
#' @param javastics Path of JavaSTICS
#' @param param_values `r lifecycle::badge("deprecated")` `param_values`
#' is no longer supported, use `values` instead.
#'
#' @details This function operates on STICS text input files.
#' Do not use it before calling `gen_usms_xml2txt()`, otherwise
#' param.sti and new_travail.usm files will be overwritten.
#'
#'   This function has been created to be called before
#'   `SticsOnR::run_stics()`. It can not work with `SticsOnR::run_javastics()`,
#'   that will overwrite param.sti and new_travail.usm files.
#'
#'   `values` can contain NA. In this case, the corresponding parameter(s)
#'   will not be forced (default value(s), i.e. read in STICS input files,
#'   will be used). If values==NA or values==NULL,
#'   not any parameter will be forced (all default values used).
#'
#' @return A logical status TRUE if successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' example_txt_dir <- get_examples_path(file_type = "txt")
#' force_param_values(example_txt_dir,
#'   setNames(object = c(220, 330), c("stlevamf", "stamflax")),
#'   javastics = "/path/to/javastics"
#' )
#' }
#'
#' @seealso `SticsOnR::run_stics()`
#'
#' @export
#'
force_param_values <- function(workspace,
                               values,
                               javastics,
                               param_values = lifecycle::deprecated()) {
  if (lifecycle::is_present(param_values)) {
    lifecycle::deprecate_warn("1.0.0",
                              "force_param_values(param_values)",
                              "force_param_values(values)")
  } else {
    param_values <- values # to remove when we update inside the function
  }

  if (is.null(param_values) || all(is.na(param_values))) {
    # remove param.sti in case of previous run using it ...
    if (suppressWarnings(file.remove(file.path(
      workspace,
      "param.sti"
    )))) {
      tryCatch(set_codeoptim(workspace, value = 0), error = function(cond) {
        return(FALSE)
      })
    }
  } else {

    # convert into vector in case a tibble is given instead of a vector
    param_values <-
      stats::setNames(as.numeric(param_values), names(param_values))

    # Checking parameters names
    param_names <- names(param_values)
    param_exist <- exist_param_csv(param_names, javastics)
    if (!all(param_exist)) {
      stop(
        "Unknown parameters detected for STICS version ",
        ":\n", # stics_version, ": \n",
        paste(param_names[!param_exist], collapse = ", ")
      )
    }

    ind_non_na <- !is.na(param_values)
    if (!all(ind_non_na)) {
      warning(paste(
        "Parameter(s)", paste(names(param_values[!ind_non_na]), collapse = ","),
        "will not be forced (maybe their values are not numeric?",
        " In that case please use set_param_*** functions)."
      ))
    }
    param_values <- param_values[ind_non_na]

    # converting par names to STICS names
    # names conversion done in exist_param_csv()
    stics_names <- names(param_exist[ind_non_na])

    ret <- gen_paramsti(workspace, stics_names, param_values)
    if (!ret) {
      return(invisible(FALSE))
    }

    tryCatch(set_codeoptim(workspace, value = 1), error = function(cond) {
      return(FALSE)
    })
  }

  return(invisible(TRUE))
}
