#' Getting parameters description
#'
#' @param file_path csv file path
#' @param stics_version The STICS version. See `get_stics_versions_compat()`
#' to get all compatible versions. Default
#' to "latest", a special code to get the latest version.
#' @param name a name vector for selecting loaded content using
#' name column matching
#' @param kind a name vector for selecting loaded content using
#' kind column matching
#'
#' @keywords internal
#'
#' @noRd
#'
get_param_desc <- function(file_path = NULL,
                           stics_version = "latest",
                           name = NULL,
                           kind = FALSE) {

  # TODO
  # file_path : check if it is a JavaSTICS dir, calculate
  # check if the file exists in the dir
  if (base::is.null(file_path)) {
    file_path <- file.path(get_examples_path(file_type = "csv",
                                             stics_version = stics_version),
                           "inputs.csv")
  }



  if (!file.exists(file_path)) stop(paste("Unknown file:", file_path))

  param_df <- utils::read.csv2(file_path,
                               header = FALSE,
                               stringsAsFactors = FALSE,
                               strip.white = TRUE)

  param_df <- param_df[, 1:8]
  colnames(param_df) <-
    c("name", "definition", "unit", "kind", "dim", "type", "min", "max")


  if (!base::is.null(name)) {
    idx <- grep(pattern = name, x = param_df$name)
    param_df <- param_df[idx, ]
  }



  # Adding a version  attribute
  stics_version <- check_version_compat(stics_version = stics_version)
  attr(x = param_df, which = "version") <- stics_version

  return(param_df)
}
