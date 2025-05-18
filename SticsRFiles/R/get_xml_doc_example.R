#' @title Get an xml_document from a STICS xml file example
#' @param xml_name xml file name
#' (see file names by calling get_xml_doc_example())
#'
#' @param stics_version the STICS files version to use
#'
#' @return An xml_document object
#'
#' @examples
#' \dontrun{
#' # Retrieving xml examples files to get xml_document from
#' get_xml_doc_example()
#'
#' # Loading an usms.xml file
#' usm_doc <- get_xml_doc_example("usms.xml")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_xml_doc_example <- function(xml_name = NULL,
                                stics_version = "latest") {

  # check/get version
  stics_version <- get_xml_stics_version(stics_version = stics_version)

  # stics_xml_types
  files <- list.files(pattern = ".xml$",
                      path = get_examples_path(file_type = "xml",
                                               stics_version = stics_version)
  )

  if (base::is.null(xml_name)) {
    return(files)
  }

  if (!xml_name %in% files) {
    stop("File does not exist in package examples,",
         "run get_xml_example_doc() to get the list !")
  }

  # getting a default xmldocument object template
  xml_file <- file.path(get_examples_path(file_type = "xml",
                                          stics_version = stics_version),
                        xml_name
  )
  xml_doc_object <- xmldocument(xml_file)

  return(xml_doc_object)
}
