#' @title Generating a var.mod type file
#' @description Generating a daily variable list file from variables names
#' @param workspace Path of the directory containing the STICS var.mod file
#' to modify
#' @param var vector of variables names (see details)
#' @param append if TRUE, `var` data are appended to `file_name`
#' @param file_name file name to generate
#' (without path, default value: "var.mod")
#' @param stics_version   Name of the STICS version
#' (used to check variable names)
#' @param force     Force variables writing even if they are not a
#' STICS variable (default: FALSE).
#'
#' @param var_names `r lifecycle::badge("deprecated")` `var_names` is no
#'   longer supported, use `var` instead.
#' @param version `r lifecycle::badge("deprecated")` `version` is no
#'   longer supported, use `stics_version` instead.
#'
#' @details Variable names can be found using `get_var_info()`. They are
#' checked before writing. If any variable name does not exist,
#' it will not be written by default, but the function will still write
#' the variables that exist. `force= TRUE` may however be used to write
#' variables that do not exist.
#'
#' @return None
#'
#' @examples
#' gen_varmod(tempdir(), c("lai(n)", "hauteur"))
#' # Add a variable to the others:
#' gen_varmod(tempdir(), "masec(n)", append = TRUE)
#' # NB: var.mod will have "lai(n)","hauteur" and "masec(n)"
#'
#'
#' @export
#'
gen_varmod <- function(workspace,
                       var,
                       append = FALSE,
                       file_name = "var.mod",
                       stics_version = "latest",
                       force = FALSE,
                       var_names = lifecycle::deprecated(),
                       version = lifecycle::deprecated()) {


  # var_names
  if (lifecycle::is_present(var_names)) {
    lifecycle::deprecate_warn(
      "1.0.0", "gen_varmod(var_names)",
      "gen_varmod(var)"
    )
  } else {
    var_names <- var # to remove when we update inside the function
  }
  # version
  # added a second condition because
  # if version is not given as an arg.
  # version always exist and giving detailed information
  # about R version and platform (see ?version)
  if (lifecycle::is_present(version) && length(version) == 1) {
    lifecycle::deprecate_warn(
      "1.0.0", "gen_varmod(version)",
      "gen_varmod(stics_version)"
    )
  } else {
    version <- stics_version # to remove when we update inside the function
  }



  # Checking if workspace exists
  if (!dir.exists(workspace)) {
    stop(paste(workspace, ": directory does not exist !"))
  }

  file_path <- file.path(workspace, file_name)

  # Checking if file exists in append use case
  if (append && isFALSE(file.exists(file_path))) {
    msg <- ": file does not exist, remove append argument or set it to FALSE) !"
    stop(paste(file_path, msg))
  }

  # Just in case: unique variable names list
  var_names <- unique(var_names)

  # Check if the variable exist:
  var_exist <- is_stics_var(var_names, version)


  if (any(!var_exist) && isFALSE(force)) {
    var_names <- var_names[var_exist]
  }

  if (!length(var_names))
    warning("Not any variable name to add to the var.mod file!")

  if (isTRUE(force)) {
    var_names[var_exist] <- var_to_stics_name(var_names[var_exist])
  } else {
    var_names <- var_to_stics_name(var_names)
  }

  # Add possibility to append a variable to var.mod.
  if (isTRUE(append)) {
    vars <- readLines(file_path)
    commonvars <- var_names %in% vars
    if (any(commonvars)) {
      cli::cli_alert_warning(paste0("Variable{?s} {.var ",
                                    "{var_names[commonvars]}} already in",
                                    " {.code var.mod}. Not repeating it."))
    }
    var_names <- var_names[!commonvars]
    if (length(var_names) == 0) {
      invisible()
    }
  }

  cat(var_names, file = file_path, sep = "\n", append = append)
}
