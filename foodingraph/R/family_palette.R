#' Generate a color palette for nodes family
#'
#' From a list of food families, create a color for each family.
#'
#' Very useful when comparing graphs with the same families.
#' It can be used by itself, but this function was created to be the
#' \code{family_palette} argument when calling
#' \code{display_graph_from_links_nodes()} The colors will be
#' automatically added to the graph (nodes and legend)
#'
#' @param family (list) : can be either the family column from the
#' legend table, or just a list of the families.
#'  In all cases, the parameter will be converted as a factor
#'  and sorted (alphabetically or numerically)
#'  Only its unique values are necessary.
#' @return A list of key and values.
#'  - keys are the family names
#'  - values are the color
#' @examples
#' family_palette(c("Fruits", "Vegetables", "Meats"))
#' @importFrom viridis viridis
#' @importFrom stats setNames
#' @export
family_palette <- function(family) {

  if( !is.factor(family) ) {
    family <- sort(family)
    family <- as.factor(family)
  }

  number_of_families <- length(levels(family))
  family <- unique(family)
  color_palette <- viridis(number_of_families) # Create the palette
  color_palette <- setNames(as.list(color_palette), sort(levels(family))) # Convert into key/value

  return(color_palette)
}
