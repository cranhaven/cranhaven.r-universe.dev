#' @title Getting parameter values from an xml parameter file for an
#' option choice
#'
#' @description Extracting parameter values from an XML file, corresponding
#' to parameters
#' defined in a option choice node (as *param* nodes)
#'
#' @param xml_file_path path of xml parameter file
#' @param option_param_name name of the option
#' @param choice_name_or_code name or code of the option choice desired
#'
#' @return A list containing a vector of strings of parameters values ($values)
#' and a vector of strings of parameters names ($names)
#'
#' @examples
#' \dontrun{
#'
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")
#'
#' get_option_choice_param_values(xml_path, "codetemp", "2")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_option_choice_param_values <- function(xml_file_path,
                                           option_param_name,
                                           choice_name_or_code) {

  # TODO: see if choice_name_or_code could be optional, to give
  # param values for all choices ??

  # testing choice_name_or_code
  # true : choice name
  # false : choice code
  if (is.na(as.numeric(choice_name_or_code))) {
    xpath <- paste0('//option[@nomParam="',
                    option_param_name,
                    '"]/choix[@nom="',
                    choice_name_or_code,
                    '"]/param')
  } else {
    xpath <- paste0('//option[@nomParam="',
                    option_param_name,
                    '"]/choix[@code="',
                    choice_name_or_code,
                    '"]/param')
  }


  param_list <- list()
  xml_param <- xmldocument(xml_file_path)

  param_list$values <- get_values(xml_param, xpath)
  param_list$names <- get_attrs_values(xml_param, xpath, "nom")
  return(param_list)
}
