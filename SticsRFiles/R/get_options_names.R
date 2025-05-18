#' @title Getting options parameter names from an xml parameter file
#'
#' @description Extracting options parameter name from an xml file data,
#' and checking names if provided in options_names
#'
#' @param xml_file_path path of xml parameter file
#' @param option_names options names vector (optional)
#'
#' @return A vector of strings of options parameter names
#'
#' @examples
#' \dontrun{
#'
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")
#'
#' get_options_names(xml_path)
#'
#' get_options_names(xml_path, c("codemonocot", "codlainet"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_options_names <- function(xml_file_path, option_names = NULL) {

  # Loading xml file
  xml_param <- xmldocument(xml_file_path)

  # Getting all options names
  param_names <- get_attrs_values(xml_param, "//option", "nomParam")

  # Checking if given names exist in param_names
  if (!base::is.null(option_names)) {
    is_element <- lapply(option_names, function(x) is.element(x, param_names))
    param_names <- option_names
    attr(param_names, "exists") <- unlist(is_element)
  }

  return(param_names)
}
