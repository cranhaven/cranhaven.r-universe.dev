
#' Test if a parameter exists
#'
#' @description Test if a parameter exists in an XML file
#'
#' @param xml_doc_object XML document
#' @param param_name The parameter name or a vector of
#' @param ... Other parameters to pass to [get_param_number()].
#'
#' @return A vector of logical values
#'
#' @examples
#' xml_sols <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#'
#' xml_doc <- xmldocument(xml_sols)
#'
#' exists_param(xml_doc, "cfes")
#' exists_param(xml_doc, c("cfes", "mulchbat"))
#' @keywords internal
#'
#' @noRd
#'
exists_param <- function(xml_doc_object, param_name, ...) {
  nb <- get_param_number(xml_doc_object, param_name, ...)

  if (is.list(nb)) {
    ex <- unlist(lapply(nb, as.logical))
    return(ex)
  }

  return(as.logical(nb))
}
