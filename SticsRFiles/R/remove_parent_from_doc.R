
#' Remove parent node of a parameter
#'
#' @description Remove a parent node from an XML file.
#'
#' @param xml_doc The XML document
#' @param param_name The parameter name
#' @param nodes_ids The node IDs to be removed (optional)
#'
#' @examples
#' \dontrun{
#'
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
#' tec_doc <- xmldocument(xml_path)
#'
#' # removing all the parent nodes the parameter belongs to
#' remove_parent_from_doc(tec_doc,
#'                                      param_name = "julapI_or_sum_upvt")
#'
#' # removing some of the parent nodes the parameter belongs to
#' remove_parent_from_doc(tec_doc,
#'   param_name = "julapI_or_sum_upvt",
#'   nodes_ids = c(1, 3)
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
remove_parent_from_doc <- function(xml_doc,
                                   param_name,
                                   nodes_ids = NULL) {
  remove_node_from_doc(
    xml_doc = xml_doc,
    param_name = param_name,
    remove_parent = TRUE,
    nodes_ids = nodes_ids
  )

}
