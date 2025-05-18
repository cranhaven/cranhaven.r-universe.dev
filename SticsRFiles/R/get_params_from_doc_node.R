#' @title Get a list of STICS xml parameters names from nodes names in
#' an xml_document
#' @param xml_node an xml XMLInternalElementNode
#'
#' @param param_list parameters names vector, used for recursive calls
#' @param unique_val logical, TRUE to get unique names list, FALSE otherwise
#'
#' @return a character vector of parameters names
#'
#' @keywords internal
#'
#' @noRd
#'
get_params_from_doc_node <- function(xml_node,
                                     param_list = c(),
                                     unique_val = TRUE) {

  # for ini, usms files

  if (!methods::is(xml_node, "XMLInternalElementNode")) {
    stop("The document is not an xml node !")
  }

  node_name <- XML::xmlName(xml_node)

  # getting only one usm node
  if (node_name == "usms" && unique_val) {
    xml_node <- XML::xmlChildren(XML::xmlRoot(xml_node))[[1]]
  }

  childs <- XML::xmlChildren(xml_node)
  childs_names <- names(childs)

  if (length(childs_names) == 1 && childs_names == "text") {
    param_list <- c(param_list, XML::xmlName(xml_node))
  } else {
    for (n in childs_names) {
      param_list <- get_params_from_doc_node(childs[[n]], param_list)
    }
  }

  return(param_list)
}
