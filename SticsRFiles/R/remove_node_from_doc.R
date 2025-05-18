#' Remove a node from an xml object
#'
#' @description Remove a node from an xml_document instance
#'
#' @param xml_doc The XML document
#' @param param_name The parameter name
#' @param parent_name The name of the parent parameter
#' (node name or attribute value of nom, nomParam)
#' @param remove_parent Logical, indicating if the parent node must be removed
#' (TRUE) or not (FALSE)
#' @param nodes_ids The node IDs to be removed (optional)
#'
#' @examples
#' \dontrun{
#'
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
#' tec_doc <- xmldocument(xml_path)
#'
#' # removing a single parameter
#' remove_node_from_doc(tec_doc, param_name = "jultrav")
#'
#' # removing all the parent nodes the parameter belongs to
#' remove_node_from_doc(tec_doc,
#'   param_name = "julapI_or_sum_upvt",
#'   remove_parent = TRUE
#' )
#'
#' # removing some of the parent nodes the parameter belongs to
#' remove_node_from_doc(tec_doc,
#'   param_name = "julapI_or_sum_upvt",
#'   remove_parent = TRUE, nodes_ids = c(1, 3)
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
remove_node_from_doc <- function(xml_doc, param_name,
                                 parent_name = NULL,
                                 remove_parent = FALSE,
                                 nodes_ids = NULL) {

  # Getting the node xpath
  xpath_node <- get_param_type(
    xml_doc = xml_doc,
    param_name = param_name,
    parent_name = parent_name
  )$xpath

  # if the type does not exist
  if (base::is.null(xpath_node)) {
    message(paste("Unknown parameter in xml doc: ", param_name))
    return(invisible())
  }

  # The parent node must be removed
  if (remove_parent) {
    # getting the nodes for parent nodes
    # to which the param_name belongs (I.e. "intervention", for example)
    xml_nodes <- lapply(get_nodes(xml_doc, xpath_node), XML::xmlParent)
  } else {
    # Getting nodes in all others cases
    xml_nodes <- get_nodes(xml_doc, xpath_node)
  }

  if (base::is.null(xml_nodes)) {
    message("No nodes to remove from xml doc !")
    return(invisible())
  }

  nodes_nb <- length(xml_nodes)
  # Calculating indices if no nodes_ids
  if (base::is.null(nodes_ids)) {
    nodes_ids <- 1:nodes_nb
  }


  # Returning the input object
  # ids out of range !
  if (!max(nodes_ids) <= nodes_nb) {
    warning("No nodes removed from the xml document, ids out of range !")
  }

  # Removing nodes from the document object
  XML::removeNodes(xml_nodes[nodes_ids])

}
