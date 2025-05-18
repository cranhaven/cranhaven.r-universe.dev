#' @title Getting parameters bounds from xml files
#'
#' @description Extracting parameters min and/or max bounds for a parameter
#' or a vector of parameters from a file or a vector of xml files.
#'
#' @param xml_file an xml file path or a vector of paths
#' @param param_name a parameter name of a vector of parameters names
#' @param bounds_name bounds name "min" or "max"
#' (optional, default value c("min","max ))
#' @param output Output data format either "list" or "data.frame" (default)
#'
#' @return A list of parameters bounds values
#'
#' @examples
#' \dontrun{
#' library(SticsRFiles)
#' xml_file <- "path/to/xmlfile"
#' xml_files_list <- c("path/to/xmlfile1", "path/to/xmlfile2")
#' param_bounds <- get_param_bounds_xml(xml_file, "param", "min")
#'
#' param_bounds <- get_param_bounds_xml(xml_file, c("param1", "param2"), "max")
#'
#' param_bounds <- get_param_bounds_xml(xml_file, c("param1", "param2"))
#'
#' param_bounds <- get_param_bounds_xml(xml_files_list, "param", "min")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_param_bounds_xml <- function(xml_file,
                                 param_name,
                                 bounds_name = NULL,
                                 output = "data.frame") {
  if (length(xml_file) > 1) {
    param_bounds <- lapply(
      xml_file,
      function(x) {
        get_param_bounds_xml(
          x,
          param_name,
          bounds_name,
          output
        )
      }
    )
    return(param_bounds)
  }

  xml_doc <- xmldocument(xml_file)

  param_bounds <- get_param_bounds(xml_doc, param_name, bounds_name, output)

  delete(xml_doc)

  return(param_bounds)
}
