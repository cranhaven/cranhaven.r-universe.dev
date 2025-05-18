#' @title Getting plants number per usm for all usms or selected from a usm
#' name vector
#'
#' @description Extracting plant number from usms.xml or new_travail.usm
#' file data
#'
#' @param usms_file Path (including name) of a USM xml file or of a
#' new_travail.usm file
#' @param usms_list Usm(s) name(s) (optional, see details)
#' @param usm_file_path `r lifecycle::badge("deprecated")` `usm_file_path` is no
#' longer supported, use `usms_file` instead.
#'
#' @details Use `get_usms_list()` to get the list of the usm names for
#' an usms.xml file.
#'
#' @return A named numeric vector of plants number per usm
#'
#' @examples
#' # Xml case
#' xml_usms <- file.path(get_examples_path(file_type = "xml"), "usms.xml")
#' get_plants_nb(xml_usms)
#' get_plants_nb(xml_usms, "wheat")
#' get_plants_nb(xml_usms, c("wheat", "intercrop_pea_barley"))
#'
#' # Txt case
#' txt_usm <- file.path(get_examples_path(file_type = "txt"), "new_travail.usm")
#' get_plants_nb(txt_usm)
#'
#' @export
#'
get_plants_nb <- function(usms_file,
                          usms_list = c(),
                          usm_file_path = lifecycle::deprecated()) {
  if (lifecycle::is_present(usm_file_path)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_plants_nb(usm_file_path)",
                              "get_plants_nb(usms_file)")
  } else {
    usm_file_path <- usms_file # to remove when we update inside the function
  }
  usm <- grepl(pattern = "\\.usm$", x = usm_file_path)
  usms <- grepl(pattern = "\\.xml$", x = usm_file_path)

  # Neither .usm nor .xml
  if (!(usm || usms)) {
    return()
  }

  if (!base::file.exists(usm_file_path)) stop(usm_file_path, " does not exist")

  if (usm) {
    return(get_plants_nb_txt(usm_txt_path = usm_file_path,
                             usm_name = usms_list))
  } else {
    return(get_plants_nb_xml(usms_file = usm_file_path, usms_list = usms_list))
  }
  stop("Couldn't read the number of plants")
}


#' @title Getting plants number per usm, all or selected from a given list
#'
#' @description Extracting plant number from usms.xml file data
#'
#' @param usms_file Path of usms.xml file
#' @param usms_list Usms selection list inside usms from usms.xml file
#' (optional, see details)
#'
#' @param usm_xml_path `r lifecycle::badge("deprecated")` `usm_xml_path` is no
#' longer supported, use `usms_file` instead.
#'
#' @details Use `get_usms_list()` to get the list of the usm names.
#'
#' @return A names numeric vector of plants number per usm
#'
#' @examples
#' \dontrun{
#'
#' usms_file <- file.path(get_examples_path(file_type = "xml"), "usms.xml")
#' get_plants_nb(usms_file)
#' get_plants_nb(usms_file, "wheat")
#' get_plants_nb(usms_file, c("wheat", "intercrop_pea_barley"))
#' }
#'
#' @keywords internal
#' @noRd
#'
get_plants_nb_xml <- function(usms_file,
                              usms_list = c(),
                              usm_xml_path = lifecycle::deprecated()) {

  # usm_xml_path
  if (lifecycle::is_present(usm_xml_path)) {
    lifecycle::deprecate_warn(
      "1.0.0",
      "get_plants_nb_xml(usm_xml_path)",
      "get_plants_nb_xml(usms_file)"
    )
  } else {
    usm_xml_path <- usms_file # to remove when we update inside the function
  }

  # Loading xml file as xml_document object
  xml_usms <- xmldocument(usm_xml_path)

  # Getting plants nb per usm
  plants_nb <- as.numeric(get_values(xml_usms, "//nbplantes"))

  # Xml usms names
  usm_names <- get_attrs(xml_usms, "//usm")
  names(plants_nb) <- usm_names

  # Filtering using usms_list if needed
  if (length(usms_list) != 0) {
    plants_nb <- plants_nb[usm_names %in% usms_list]
  }

  return(plants_nb)
}




get_plants_nb_txt <- function(usm_txt_path, usm_name = NULL) {
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
  plants_nb <- as.numeric(usm_data$nbplantes)
  names(plants_nb) <- usm_data$nom
  return(plants_nb)
}
