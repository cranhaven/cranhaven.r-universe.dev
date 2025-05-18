#' @title Get a list of STICS xml parameters names from nodes attributes in
#' an xml_document
#' @param xml_doc an xml_document object (created from an xml file)
#'
#' @param type_name type name, one of "option", "param", "colonne"
#' @param unique_val logical, TRUE to get unique names list, FALSE otherwise
#'
#' @return a named list of parameter names
#'
#' @keywords internal
#' @noRd
#'
get_params_from_doc_attr <- function(xml_doc,
                                     type_name = NULL,
                                     unique_val = TRUE) {

  # For tec, param newform, param gen, sols, station
  # files
  type_names <- c("option", "param", "colonne", "colonne")
  name_fields <- c("nomParam", "nom", "nom", "nom")
  parent_names <- c("", "", "ta_entete/", "tableau_entete/")


  if (base::is.null(type_name)) {
    type_name <- type_names
  }

  type_id <- type_names %in% type_name

  if (!any(type_id)) {
    stop(paste("Unknown param type_name : ", type_name))
  }

  name_field <- name_fields[type_id]
  parent_name <- parent_names[type_id]

  xpath <- paste0("//", parent_name, type_name)

  nb_types <- length(type_name)

  params <- vector("list", nb_types)
  is_null <- rep(TRUE, nb_types)


  for (t in 1:nb_types) {
    tmp <- get_attrs_values(xml_doc, xpath[t], name_field[t])
    if (!base::is.null(tmp)) {
      params[[t]] <- tmp
      is_null[t] <- FALSE
    }
  }



  params <- params[!is_null]
  names(params) <- type_name[!is_null]

  if (unique_val) {
    params <- lapply(params, unique)
  }

  return(params)
}
