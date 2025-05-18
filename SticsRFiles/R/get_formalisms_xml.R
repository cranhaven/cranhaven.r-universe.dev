#' @title Get the formalism(s) parameters from a file or a list of
#' @param xml_file an xml file path or a vector of paths
#' @param par_name Optional. A parameter name or a vector of. If not provided,
#' all the parameters are extracted.
#' @param by_form Logical. Structuring parameters by formalism (TRUE),
#' or not (structuring output by parameter) for each file
#'
#'
#' @return A list (by file) of list of parameters names by formalism name
#'
#' @examples
#' \dontrun{
#'
#' xml_sta <- file.path(get_examples_path(file_type = "xml"), "file_sta.xml")
#'
#' par_form <- get_formalisms_xml(xml_sta, "zr")
#'
#' par_form_list <- get_formalisms_xml(xml_sta, c("zr", "altistation"))
#'
#' par_form_list <- get_formalisms_xml(xml_sta)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
#'
get_formalisms_xml <- function(xml_file,
                               par_name = NULL,
                               by_form = TRUE) {


  # Just in case to be sure that xml files are different
  xml_file <- unique(xml_file)

  # Loading XML files in a XmlDocument list
  xml_doc <- lapply(xml_file, xmldocument)
  doc_nb <- length(xml_doc)

  # If only one element
  if (doc_nb == 1) xml_doc <- xml_doc[[1]]

  # Getting the parameters formalism list by file
  form_list <- get_param_formalisms(xml_doc, par_name)

  # if only one element
  if (doc_nb == 1) form_list <- list(form_list)

  # Naming with xml files names
  names(form_list) <- base::basename(xml_file)

  # Rewriting a named list using formalisms names
  if (by_form) {
    form_list <- param_by_form(form_list)
  }

  if (is.list(xml_doc)) {
    lapply(xml_doc, delete)
  }

  if (inherits(xml_doc, "xml_document")) delete(xml_doc)

  return(form_list)
}


param_by_form <- function(form_list) {
  files_names <- names(form_list)
  files_nb <- length(files_names)
  out_l <- vector("list", files_nb)
  for (n in 1:files_nb) {
    form_names <- unlist(form_list[[files_names[[n]]]])

    if (all(base::is.na(form_names))) {
      out_l[[n]] <- NA
      next
    }
    u_forms <- unique(form_names)
    nb_forms <- length(u_forms)
    out <- vector("list", length(u_forms))
    for (f in 1:nb_forms) {
      out[[f]] <- names(form_names)[form_names == u_forms[f]]
    }

    names(out) <- u_forms
    out_l[[n]] <- out
  }

  names(out_l) <- files_names

  return(out_l)
}
