#' Converting an attributes list to a matrix
#'
#' @param attr_list XML attributes list
#'
#' @return A matrix with attributes names as column names
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#' sols_doc <- xmldocument(xml_path)
#' node_set <- get_nodes(sols_doc,
#'                                "//*[@nom=\"solcanne\" or @nom=\"mulchbat\"]")
#' attr_list <- sapply(node_set, function(x) XML::xmlAttrs(x))
#'
#' #> [[1]]
#' #> nom
#' #> "solcanne"
#'
#' #> [[2]]
#' #> format        max        min        nom
#' #> "real"      "2.0"      "0.0" "mulchbat"
#'
#' #> [[3]]
#' #> format        max        min        nom
#' #> "real"      "2.0"      "0.0" "mulchbat"
#'
#' attributes_list2matrix(attr_list)
#'
#' #>      nom        format max   min
#' #> [1,] "solcanne" NA     NA    NA
#' #> [2,] "mulchbat" "real" "2.0" "0.0"
#' #> [3,] "mulchbat" "real" "2.0" "0.0"
#' }
#'
#' @keywords internal
#'
#' @noRd
#'

attributes_list2matrix <- function(attr_list) {
  # Getting unique attributes names list
  col_names <- unique(unlist(lapply(attr_list, names)))

  # Creating a NA matrix
  attr_mat <- matrix(NA, length(attr_list), length(col_names))
  colnames(attr_mat) <- col_names

  # Filling the matrix
  for (l in seq_along(attr_list)) {
    col_idx <- sapply(
      names(attr_list[[l]]),
      function(x) which(is.element(col_names, x))
    )
    attr_mat[l, col_idx] <- attr_list[[l]]
  }

  return(attr_mat)
}
