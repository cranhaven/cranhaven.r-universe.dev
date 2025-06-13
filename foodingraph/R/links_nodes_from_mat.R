#' Extracts links and nodes
#'
#' From an adjacency matrix, extracts two data.frames/tibbles
#' \enumerate{
#'   \item Links. columns : from, to, with, weight
#'   \item Nodes. columns : name, title. name corresponds to the names
#'    used in 'from' and 'to'
#' }
#'
#' @param adjacency_matrix : a matrix of size n x n, each element being
#'  a number explaining the relationship e.g. coefficient, information
#'  between two variables given in the column and row names
#'  /!\ As this code is to draw undirected graphs, only the lower
#'  triangular part of adjacency matrix is used to extract the
#'  information.
#' @param legend : a data frame of columns in order :
#'     1) name, str : name of the node in the adjacency matrix, e.g.
#'     CRUDSAL_cat
#'     2) title, str : name of the node, e.g. Raw vegetables
#'     3) family, factor : (optional) the family the node belongs to,
#'     e.g. Vegetables
#' @param threshold numeric) : a number defining the minimal threshold.
#' If the weights are less than this threshold, they will be set to 0.
#' @param abs_threshold (bool) : should the threshold keep negative values,
#'  e.g. if \code{abs_threshold} is set to \code{TRUE}, and threshold is set
#'  to 0.1, all weights between -0.1 and 0.1 will be set to 0
#' @param filter_nodes (bool) : should the variables not in the adjacency
#' matrix be displayed on the graph? Default is TRUE
#' CAREFUL : if set to \code{TRUE}, be sure to have the same colors in the
#' family legend of the graphs. A fixed palette can be set using the
#' \code{\link{family_palette}} func.
#' @return A list of two data frames : links and nodes.
#' @examples
#' adj_matrix <- cor(iris[,-5])
#' legend <- data.frame(name = colnames(iris[,-5]),
#'                      title = colnames(iris[,-5]))
#' links_nodes_from_mat(adj_matrix, legend)
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom dplyr mutate filter
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @export
links_nodes_from_mat <- function(adjacency_matrix,
                                 legend,
                                 threshold = 0,
                                 abs_threshold = TRUE,
                                 filter_nodes = TRUE) {

  # Change the type of the correlation matrix to be able to alter its structure
  if (is.matrix(adjacency_matrix)) {
    adjacency_matrix <- as.data.frame(adjacency_matrix)
  } else if (!is.data.frame(adjacency_matrix) && !is.matrix(adjacency_matrix)) {
    stop("Supply a data-frame or a matrix")
  }

  # As we are working on undirected networks, we don't need half the matrix
  adjacency_matrix[upper.tri(adjacency_matrix)] <- NA

  links <- adjacency_matrix %>%
    rownames_to_column(var = "from") %>%
    gather("to", "weight", -"from") %>%
    na.omit()

  if (abs_threshold == T) {
    links <- links %>%
      mutate(weight = ifelse(abs(.data$weight) < !!threshold, 0, round(.data$weight, 3)))
  } else {
    links <- links %>%
      mutate(weight = ifelse(.data$weight < !!threshold, 0, round(.data$weight, 3)))
  }

  # To represent the weights on the graph, we use the edges' widths
  links <- links %>%
    filter(.data$weight != 0) %>%
    mutate(width = abs(.data$weight))

  nodes <- legend
  if (filter_nodes == TRUE) {
    # Select the nodes for which their names exist in the adjacency matrix
    nodes_names <- which(legend[,1] %in% colnames(adjacency_matrix))
    nodes <- legend[nodes_names,]
  }

  list(
    links = links,
    nodes = nodes
  )
}
