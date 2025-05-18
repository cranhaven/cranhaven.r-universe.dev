#' Add node to a STICS XML document
#'
#' @param xml_doc XML document
#' @param formalism_name Name of the formalism (optional)
#' @param nodes_nb number of operations to add
#' @param stics_version The version of STICS (eg "V9.1")
#'
#' @examples
#' \dontrun{
#'
#' tec_xml <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
#' tec_doc <- xmldocument(tec_xml)
#'
#' # Adding one irrigation operation
#' add_stics_nodes(tec_doc, "irrigation")
#'
#' # Adding three irrigation operations
#' add_stics_nodes(tec_doc, "irrigation", nodes_nb = 3)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
add_stics_nodes <- function(xml_doc, formalism_name = NULL, nodes_nb = 1,
                            stics_version = "latest") {


  # Getting nodes types that may be added to xml_doc
  node_types <- get_xml_base_node()


  # Getting the file tag corresponding to the file type
  # among usms, sols, tec
  files_tags <- c("usms", "sols", "tec")
  file_tag <- files_tags[c(
    is_stics_usms(xml_doc),
    is_stics_sols(xml_doc),
    is_stics_tec(xml_doc)
  )]

  if (length(file_tag) < 1) {
    stop("The xml document is of wrong type")
  }


  # Getting information on document nodes that may be used
  file_idx <- which(node_types$files_tags == file_tag)

  parent_name <- node_types$parent[file_idx]

  if (!is.null(formalism_name)) {
    # Case : tec
    form_idx <- which(node_types$form_names[[file_tag]] == formalism_name)

    if (!length(form_idx)) {
      stop(paste("Unkown formalism name", formalism_name))
    }

    if (formalism_name == "special techniques") {
      stop("Special cutting techniques are not implemented !")
    }

    parent_path <- get_param_type(
      xml_doc, parent_name,
      "formalisme", formalism_name
    )$xpath
  } else {
    # Case usms, sols
    parent_path <- paste0("//", parent_name)
  }

  # Getting a copy of the new node to add
  new_node <- get_xml_base_node(
    file_tag = file_tag,
    form_name = formalism_name,
    stics_version = stics_version
  )

  # Adding new_node nodes_nb times
  add_node_to_doc(xml_doc, new_node,
    nodes_nb = nodes_nb,
    parent_path = parent_path
  )

  # Updating if needed interventions_nb
  if (XML::xmlName(new_node) == "intervention") {
    nb_interventions <-
      as.numeric(get_attrs_values(xml_doc, parent_path, "nb_interventions")) +
      nodes_nb
    set_attrs_values(xml_doc, parent_path, "nb_interventions", nb_interventions)
  }
}
