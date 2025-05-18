#' @title Get an xml_document from a STICS xml file template
#' @param xml_type xml file type (see types returned when calling
#' get_xml_base_doc())
#'
#' @param stics_version the STICS files version to use
#'
#' @return an xml_document object
#'
#' @examples
#' \dontrun{
#' # Getting xml STICS files types list (i.e. keywords)
#' get_xml_base_doc()
#'
#' # Getting a soil document with one soil definition
#' get_xml_base_doc("sols")
#'
#' # STICS version can be provided, V9.1
#' # corresponds to stics_version = "latest"
#' # View available STICS files version
#' get_xml_stics_version()
#' # Giving STICS version
#' get_xml_base_doc("sols", stics_version = "V9.1")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_xml_base_doc <- function(xml_type = NULL,
                             stics_version = "latest") {

  # types list
  types <- c("sols", "usms", "ini", "tec", "sta")
  # returning types if no args
  if (!nargs()) {
    return(types)
  }

  # index for getting files_pref value
  idx <- types %in% xml_type
  # checking the xml_type
  if (!any(idx)) {
    stop("Unknown xml type for getting an xml template xml_document !")
  }

  # check & get version
  stics_version <- get_xml_stics_version(stics_version = stics_version)

  # getting files prefix
  files_pref <- c("one", "one", "file", "file", "file")
  pref <- files_pref[idx]

  # getting a default xmldocument object template
  tmpl_file <- file.path(
    get_examples_path(file_type = "xml_tmpl", stics_version = stics_version),
    paste0(pref, "_", xml_type, ".xml")
  )
  xml_doc_object <- xmldocument(tmpl_file)

  return(xml_doc_object)
}
