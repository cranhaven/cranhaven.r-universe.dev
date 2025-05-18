#' @title Get a list of STICS xml parameters names in an xml_document
#' @param xml_doc_object an xml_document object (created from an xml file)
#'
#' @param type_name type name, one of "option", "param", "colonne"
#' @param unique_val logical, TRUE to get unique names list, FALSE otherwise
#'
#' @return a character vector of parameters names
#'
#' @keywords internal
#'
#' @noRd
#'
get_params_from_doc <- function(xml_doc_object,
                                type_name = NULL,
                                unique_val = TRUE) {
  if (!methods::is(xml_doc_object, "xml_document")) {
    stop("The document is not an xml_document !")
  }

  root_names_attr <- c("fichierparamgen", "fichierpar", "fichiertec",
                       "sols", "fichiersta")
  root_names_node <- c("usms", "initialisations")

  root_node <- XML::xmlRoot(xml_doc_object@content)
  root_name <- XML::xmlName(root_node)

  if (root_name %in% root_names_attr) {
    params <- get_params_from_doc_attr(xml_doc_object,
                                       type_name = type_name,
                                       unique_val = unique_val)
  }

  if (root_name %in% root_names_node) {
    params <- get_params_from_doc_node(root_node, unique_val = unique_val)
  }

  return(params)
}
