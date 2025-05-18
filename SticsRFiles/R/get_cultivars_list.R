#' @title Get the cultivar names for an xml plant file (*_plt.xml)
#'
#' @description Extracts the cultivar names from a plant file
#'
#' @param file The path of a plant file.
#'
#' @return A vector of cultivar names
#'
#' @examples
#' path <- get_examples_path(file_type = "xml")
#'
#' # Read from a plant file (all cultivars available in a plant file)
#' cv_list <- get_cultivars_list(file = file.path(path, "file_plt.xml"))
#'
#' @export
#'
get_cultivars_list <- function(file) {

  xml_doc <- xmldocument(file)

  stopifnot(is_stics_plt(xml_doc))

  xml_name <- "variete"

  cv_list <- unique(
      unlist(
          lapply(
              XML::getNodeSet(doc = xml_doc@content,
                              path = paste0("//", xml_name)),
              function(x) {
                XML::xmlGetAttr(x, "nom")
              }
          )
      )
  )

  return(cv_list)
}
