#' @title Getting LAI forcing for each usm
#'
#' @description Is LAI forced for usms in usms.xml
#'
#' @param usm_file_path Path to usms.xml file
#' @param usms_list Usm(s) name(s) (optional, see details)
#'
#' @details Use `get_usms_list()` to get the list of the usm names for an
#' usms.xml file.
#'
#' @return A named numeric vector with a Boolean value (`TRUE = forced`)
#' for each usm
#'
#' @examples
#' # Xml case
#' xml_usms <- file.path(get_examples_path(file_type = "xml"), "usms.xml")
#' get_lai_forcing(xml_usms)
#' get_lai_forcing(xml_usms, "wheat")
#' get_lai_forcing(xml_usms, c("wheat", "intercrop_pea_barley"))
#'
#'
#' @export
#'
get_lai_forcing <- function(usm_file_path, usms_list = c()) {
  usm <- grepl(pattern = "\\.usm$", x = usm_file_path)
  usms <- grepl(pattern = "\\.xml$", x = usm_file_path)

  # Neither .usm nor .xml
  if (!(usm || usms)) {
    return()
  }

  if (!base::file.exists(usm_file_path))
    stop(usm_file_path, " does not exist")

  if (usm) {
    return(get_lai_forcing_txt(usm_txt_path = usm_file_path,
                               usm_name = usms_list))
  }

  if (usms) {
    return(get_lai_forcing_xml(usm_xml_path = usm_file_path,
                               usms_list = usms_list))
  }
}





#' @title Is LAI forced ? plants number per usm, all or selected from
#' a given list
#'
#' @description Is the lai forced in usms from a usms.xml file
#'
#' @param usm_xml_path Path to usms.xml file
#' @param usms_list Usms to filter (optional, see details)
#'
#' @details Use `get_usms_list()` to get the list of the usm names.
#'
#' @return A names numeric vector of a boolean value for each usm
#'
#' @keywords internal
#'
#' @noRd
#'
get_lai_forcing_xml <- function(usm_xml_path, usms_list = c()) {

  # Loading xml file as xml_document object
  usms_doc <- xmldocument(usm_xml_path)

  get_lai_forcing_xml_doc(usms_doc, usms_list = usms_list)
}


get_lai_forcing_xml_doc <- function(usm_doc, usms_list = c()) {

  # Getting plants nb per usm
  lai_forced <- as.logical(as.numeric(get_values(usm_doc, "//codesimul")))

  # Xml usms names
  usm_names <- get_attrs(usm_doc, "//usm")
  names(lai_forced) <- usm_names

  # Filtering using usms_list if needed
  if (length(usms_list) != 0) {
    lai_forced <- lai_forced[usm_names %in% usms_list]
  }

  return(lai_forced)
}

get_lai_forcing_txt <- function(usm_txt_path, usm_name = NULL) {
  if (base::basename(usm_txt_path) != "new_travail.usm") {
    return()
  }


  if (length(usm_name) > 1) stop("Only one usm name may be given !")

  # Getting usm parameters
  usm_data <- get_usm_txt(filepath = usm_txt_path)

  # Checking usm name
  if (!base::is.null(usm_name) && usm_data$nom != usm_name)
    stop(usm_name, ": wrong usm name")

  # Returning a named vector
  lai_forced <- usm_data$codesimul == "feuille"
  names(lai_forced) <- usm_data$nom

  return(lai_forced)
}
