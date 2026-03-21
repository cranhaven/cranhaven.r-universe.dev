#' Sunflower repertoire graph
#'
#' @description Sequence frequency visualization among samples, displayed as
#' rings of nodes inside each other.
#'
#' @param dataset Input object: a matrix or a data frame.
#'
#' First column is located as the outer ring, the second is right after and so
#' on to the last column as the inmost ring. Cell's numeric value determines
#' node size.
#'
#' @param ... Any other arguments.
#'
#' @export
#'
#' @return No return value.
#'
#' @examples
#' data <- matrix(rexp(400,1/4), ncol = 4)
#' sunflower(data)
#'
sunflower <- function (dataset, ...){
  UseMethod("sunflower")
}


#' Default graph
#'
#' @description Default visualization of sequence frequencies among samples as
#' rings inside each other.
#'
#' @param dataset Input object: a matrix or a data frame.
#'
#' First column is located as the outer ring, the second is right after and so
#' on to the last column as the inmost ring. Cell's numeric value determines
#' node size.
#'
#' @param ... Any other arguments.
#'
#'
#' @import igraph
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @return No return value.
#'
#' @examples
#' data <- matrix(rexp(400,1/4), ncol = 4)
#' sunflower(data)
#'
sunflower.default <- function(dataset, ...) {
  mat <- as.matrix(dataset)

  g <- make_empty_graph(length(mat))
  l <- matrix(0, ncol = 2, nrow = vcount(g))

  # Vertices ids vector of each rings
  vi <- seq(vcount(g))
  vi <- ceiling(vi/nrow(mat))
  vi <- split(seq(vcount(g)), vi)

  # Build layout
  for (i in seq(ncol(mat)))
    l <- l + layout_in_circle(g, order = vi[[i]]) * i/ncol(mat)

  color <- rainbow(ncol(mat), alpha=.5)

  V(g)$size <- as.vector(mat)
  V(g)$color <- rep(color, each = nrow(mat), length.out=vcount(g))
  plot(g, layout = l, vertex.label=NA, rescale = FALSE)
}
