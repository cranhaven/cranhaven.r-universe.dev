#' Checking existing parameters names in XML files
#'
#' @param param Parameter name or a vector of
#' @param stics_version An optional version name as listed in
#' get_stics_versions_compat() return
#'
#' @return A logical vector of existing parameters
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' exist_param_xml(param = "albedo")
#'
#' exist_param_xml(param = "albedo", stics_version = "V9.0")
#'
#' exist_param_xml(param = c("albedo", "latitude", "humcapil"))
#'
#' exist_param_xml(param = c("albedo", "latitude", "humcapi"))
#' }
#'
exist_param_xml <- function(param,
                            stics_version = "latest") {

  # Finding exact matchs in found names
  par_names <- get_param_data_df(
    param = param,
    stics_version = stics_version,
    exact = TRUE
  )$name

  # Checking if any correspondence for each element of name
  exist_status <- unlist(lapply(param, function(x) any(par_names %in% x)))

  # adding param names as vector name
  names(exist_status) <- param

  # Exiting status vector
  return(exist_status)
}



#' Checking existing parameters names in csv input files
#'
#' @param param Parameter name or a vector of
#' @param javastics Path of JavaSTICS
#'
#' @return A named logical vector of existing parameters, with
#' real parameters names
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' exist_param_csv(
#'   param = "albedo",
#'   javastics = "/path/to/JavaSTICS/folder"
#' )
#'
#' exist_param_csv(
#'   param = c("albedo", "latitude"),
#'   javastics = "/path/to/JavaSTICS/folder"
#' )
#' }
#'
exist_param_csv <- function(param,
                            javastics) {


  # Keeping names to modify with real names
  final_names <- param

  # managing environment for storing data frames and file path
  if (!exists("env_stics")) {
    parent <- eval(parse(text = ".GlobalEnv"))
    env_stics <- new.env(parent)
    assign(
      x = "env_stics",
      value = env_stics,
      pos = parent
    )
  }

  inputs_path <- file.path(javastics, "config", "inputs.csv")
  read_csv <- FALSE

  # checking existence of csv_path
  if (!"inputs_path" %in% names(env_stics)) {
    env_stics[["inputs_path"]] <- inputs_path
    read_csv <- TRUE
  } else {
    if (env_stics[["inputs_path"]] != inputs_path) {
      env_stics[["inputs_path"]] <- inputs_path
      read_csv <- TRUE
    }
  }

  # checking existence of par_names
  if (read_csv) {
    par_names <- get_param_data_df(
      file = inputs_path,
    )$name
    env_stics[["par_names"]] <- par_names
  } else {
    par_names <- env_stics[["par_names"]]
  }


  # searching parameters names
  idx_param <- unlist(lapply(param, function(x) any(par_names %in% x)))

  # replacing underscores with ()
  conv_param <- col_names_to_var(param)
  idx_conv_param <- unlist(lapply(
    conv_param,
    function(x) any(par_names %in% x)
  ))

  # dimensioning existence vector
  exist_status <- rep(FALSE, length(param))

  # detecting parameter names
  any_idx <- any(idx_param)
  any_conv_idx <- any(idx_conv_param)

  # input names
  if (any_idx) {
    exist_status[idx_param] <- TRUE
    final_names[idx_param] <- param[idx_param]
  }

  # rewritten names
  if (any_conv_idx) {
    exist_status[idx_conv_param] <- TRUE
    final_names[idx_conv_param] <- conv_param[idx_conv_param]
  }

  # full names list as vector names
  names(exist_status) <- final_names

  return(exist_status)
}
