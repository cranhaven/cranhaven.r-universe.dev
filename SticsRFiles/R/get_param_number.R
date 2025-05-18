#' @title Get the number of a (or a list of) STICS xml parameter(s) occurence
#' from a request in an xml_document
#' @param xml_doc_object an xml_document object (created from an xml file)
#'
#' @param param_name parameter name or a vector of names
#' @param ... other arguments that can be transmitted to get_param_value
#' function (see doc)
#'
#' @return a numeric vector
#'
#'
#' @examples
#' \dontrun{
#' xml_usms <- file.path(get_examples_path(file_type = "xml"), "usms.xml")
#'
#' usms_doc <- xmldocument(xml_usms)
#'
#' par_nb <- get_param_number(usms_doc, "usm")
#'
#' par_nb_vec <- get_param_number(usms_doc, c("usm", "fplt"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
# TODO: may use get_param_types returning values nb in $length field
get_param_number <- function(xml_doc_object, param_name, ...) {
  values <- get_param_value(xml_doc_object, param_name, ...)

  if (is.list(values)) {
    nb <- unlist(lapply(values, length), use.names = FALSE)
    return(nb)
  }

  return(length(values))
}
