#' @title Generate from a template or modify a STICS sols or usms xml_document

#' @param doc_type Document type
#' @param xml_doc  The xml document
#' @param nodes_nb The number of nodes
#' @param nodes_param Node parameter
#' @param stics_version Version of the STICS model
#'
#' @return An xml_document object
#'
#' @examples
#' \dontrun{
#' # A newly created one, with one or more usms and
#' # fake parameters values
#' xml_doc <- gen_xml_doc(doc_type = "usms")
#' xml_doc <- gen_xml_doc(doc_type = "usms", node_nb = 3)
#'
#' # With changing parameters values, from an existing document
#' # and a data.frame contaning usms parameters values
#' existing_doc <- xmldocument("/path/to/usms.xml")
#'
#' out_xml_doc <- gen_xml_doc(
#'   doc_type = "usms",
#'   xml_doc = existing_doc, nodes_param = param_data_frame
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_usms_sols_doc <- function(doc_type,
                              xml_doc = NULL,
                              nodes_nb = NULL,
                              nodes_param = NULL,
                              stics_version = "latest") {


  # for usms and sols files

  doc_types <- list()
  doc_types$usms <- list(root = "usms", node = "usm")
  doc_types$sols <- list(root = "sols", node = "sol")

  if (!nargs()) {
    return(names(doc_types))
  }

  if (!is.element(doc_type, names(doc_types))) {
    stop(paste0("The doc type is not an existing one: ", doc_type))
  }

  root <- doc_types[[doc_type]]$root
  node <- doc_types[[doc_type]]$node

  root_str <- paste0("/", root)
  node_str <- paste0("//", node)

  # getting a default xml template
  if (base::is.null(xml_doc)) {
    # check/get version of templates xml files
    stics_version <- get_xml_stics_version(stics_version = stics_version)

    # using function get_xml_base_doc
    xml_doc_out <- get_xml_base_doc(
      xml_type = doc_type,
      stics_version = stics_version
    )
  } else {
    xml_doc_out <- xml_doc
  }

  elts_nb <- NULL

  # identity or single usms/sols doc from the template file
  if (all(base::is.null(c(nodes_nb, nodes_param)))) {
    return(xml_doc_out)
  }

  # Calculating nodes number
  if (!base::is.null(nodes_nb)) {
    elts_nb <- nodes_nb
  }

  if ("data.frame" %in% class(nodes_param)) {
    elts_nb <- dim(nodes_param)[1]
  }

  # checking nodes number value
  if (base::is.null(elts_nb)) {
    stop("Error in usm number or in soils parameters table !")
  }

  # getting usm/sol nodes
  xml_nodes <- get_nodes(xml_doc_out, node_str)

  # Nothing to do
  doc_nodes_nb <- length(xml_nodes)
  if (doc_nodes_nb == elts_nb && base::is.null(nodes_param)) {
    return(xml_doc_out)
  }


  # Creating nodes for usms or sols
  add_node_to_doc(xml_doc_out,
                  xml_nodes[[1]],
                  nodes_nb = elts_nb - 1,
                  parent_path = root_str)

  # Warning if nodes number > 1
  # I that case, the xml_doc_out cannot be considered as a template
  if (doc_nodes_nb > 1) {
    stop("Multiple elements in ",
         doc_type,
         " file, cannot be used as a template !")
  }

  # Not any parameters values for overloading
  # existing ones, returning the template content.
  if (base::is.null(nodes_param)) {
    return(xml_doc_out)
  }

  switch(doc_type,
    usms = set_usms_param_xml(xml_doc = xml_doc_out,
                              usms_param = nodes_param,
                              overwrite = TRUE),
    sols = set_sols_param_xml(xml_doc = xml_doc_out,
                              sols_param = nodes_param,
                              overwrite = TRUE)
  )

  rm(xml_nodes)

  return(xml_doc_out)
}
