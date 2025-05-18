#' Add node to an xml_document object
#'
#' @param xml_doc an xml_document
#' @param new_node a new node to add
#' @param nodes_nb nodes number to add to parent node
#' @param parent_path path of the parent node
#'
#'
#' @examples
#' \dontrun{
#'
#' tec_xml <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
#' tec_doc <- xmldocument(tec_xml)
#'
#' # Getting a new irrigation operation node
#' irrigation_node <- get_xml_base_node("tec", "irrigation")
#' parent_path <- get_param_type(
#'   tec_doc, "ta", "formalisme",
#'   "irrigation"
#' )$xpath
#'
#' # Adding one irrigation operation
#' add_node_to_doc(
#'   xml_doc = tec_doc, new_node = irrigation_node,
#'   nodes_nb = 1, parent_path = parent_path
#' )
#'
#' # Checking irrigations operations number
#' irrigations_nb <- length(get_param_value(
#'   tec_doc,
#'   "julapI_or_sum_upvt"
#' ))
#' # Fixing it in nb_interventions attribute
#' set_param_value(
#'   tec_doc, "nb_interventions", irrigations_nb,
#'   "irrigation"
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
add_node_to_doc <- function(xml_doc, new_node, nodes_nb = 1, parent_path) {

  # Checking that parent_path is valid xpath for xml_doc
  if (is.null(get_nodes(xml_doc, parent_path))) {
    warning(paste("Given xpath is not a valid one:", parent_path))
    return(invisible())
  }

  # Adding nodes_nb new_node to the xml_doc under parent node
  # after existing sibling nodes
  replicate(nodes_nb, add_nodes(xml_doc, XML::xmlClone(new_node), parent_path))
}
