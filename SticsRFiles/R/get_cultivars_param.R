#' @title Get the values of cultivar-specific parameters from an xml
#' plant file (*_plt.xml)
#'
#' @description Extracts the values of cultivar-specific parameters from a
#' plant file
#'
#' @param file The path of a plant file.
#'
#' @return A data.frame with one row per cultivar and one column per parameter
#'
#' @examples
#' path <- get_examples_path(file_type = "xml")
#'
#' # Read from a plant file (all cultivars available in a plant file)
#' cv_param_df <- get_cultivars_param(file = file.path(path, "file_plt.xml"))
#'
#' @export
#'
get_cultivars_param <- function(file) {

  cv_list <- get_cultivars_list(file)

  out <- get_param_xml(file, select = "variete", select_value = cv_list)
  out <- as.data.frame(do.call(cbind, out[[1]]))
  rownames(out) <- cv_list

  return(out)
}
