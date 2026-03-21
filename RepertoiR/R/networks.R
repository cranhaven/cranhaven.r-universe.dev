#' Sequences distance network
#'
#' @description Computes pairwise string distances among repertoire's sequences
#' and visualize similar pairs as connected nodes, each sized by its frequency.
#'
#'
#' @param dataset A matrix or a data frame includes row names which are used as
#' the compared sequences. Data set's numeric values determine node-size.
#' @param ... Any additional arguments needed by the specialized methods.
#' @param by Index of column to set its values as node-size. first column is
#' default (1).
#' @param nrow Number of nodes to display. Default is 1000 nodes.
#' @param method stringdist method to perform for distance dissimilarity
#' calculation: "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
#' "jaccard", "jw", "soundex". Default is Levenshtein distance ("lv").
#'
#' @export
#'
#' @return No return value.
#'
#' @examples
#'
#' aa <- c(
#'   "G", "A", "V", "L", "I", "P", "F", "Y", "W", "S",
#'   "T", "N", "Q", "C", "M", "D", "E", "H", "K", "R"
#' )
#' data <- matrix(rexp(1 / 2, n = 1000), ncol = 4)
#' cons <- sample(aa, 10)
#' aavec <- c()
#'
#' while (length(aavec) < nrow(data)) {
#'   aaseq <- cons
#'   index <- sample(length(aaseq), sample(length(aaseq) / 3, 1))
#'   aaseq[index] <- sample(aa, length(index), replace = TRUE)
#'   aaseq <- paste0(aaseq, collapse = "")
#'   aavec <- unique(append(aavec, aaseq))
#' }
#'
#' rownames(data) <- aavec
#' colnames(data) <- LETTERS[1:ncol(data)]
#'
#' network(data, by = 3, nrow = 100)
network <- function(dataset, by, nrow, method, ...) {
  UseMethod("network")
}



#' Sequences distance network
#'
#' @description Computes pairwise string distances among repertoire's sequences
#' and visualize similar pairs as connected nodes, each sized by its frequency.
#'
#' @param dataset A matrix or a data frame includes row names which are used as
#' the compared sequences. Data set's numeric values determine node-size.
#'
#'
#'
#' @param by Index of column to set its values as node-size. first column is
#' default (1).
#' @param nrow Number of nodes to display. Default is 1000 nodes.
#' @param method stringdist method to perform for distance dissimilarity
#' calculation: "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
#' "jaccard", "jw", "soundex". Default is Levenshtein distance ("lv").
#' @param ... Any additional arguments needed by the specialized methods.
#'
#' @import utils
#' @importFrom igraph simplify graph.data.frame V cluster_louvain layout_with_fr
#' @importFrom stringdist stringdistmatrix
#' @importFrom reshape2 melt
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @return No return value.
#'
#' @examples
#' aa <- c(
#'   "G", "A", "V", "L", "I", "P", "F", "Y", "W", "S",
#'   "T", "N", "Q", "C", "M", "D", "E", "H", "K", "R"
#' )
#' data <- matrix(rexp(1 / 2, n = 1000), ncol = 4)
#' cons <- sample(aa, 10)
#' aavec <- c()
#'
#' while (length(aavec) < nrow(data)) {
#'   aaseq <- cons
#'   index <- sample(length(aaseq), sample(length(aaseq) / 3, 1))
#'   aaseq[index] <- sample(aa, length(index), replace = TRUE)
#'   aaseq <- paste0(aaseq, collapse = "")
#'   aavec <- unique(append(aavec, aaseq))
#' }
#'
#' rownames(data) <- aavec
#' colnames(data) <- LETTERS[1:ncol(data)]
#'
#' network(data)
network.default <- function(dataset, by = 1, nrow = 1000, method = "lv", ...) {
  dataset <- as.table(dataset)
  # Check arguments
  by <- ifelse(ncol(dataset) < as.numeric(by), ncol(dataset), as.numeric(by))
  nrow <- as.numeric(nrow)
  nrow <- ifelse(nrow(dataset) < nrow, nrow(dataset), nrow)
  method <- ifelse(!method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                  "cosine", "jaccard", "jw", "soundex"), "lv",
                   as.character(method))
  # Order by sample index
  dataset <- dataset[order(dataset[, by], decreasing = TRUE), ]
  mat <- as.matrix(dataset)
  mat <- head(mat, nrow)
  # Create distance matrix
  dist_d <- stringdistmatrix(rownames(mat), method = method)
  dist_d <- as.matrix(dist_d)
  dist_d[dist_d > 2 | dist_d == 0] <- NA
  colnames(dist_d) <- rownames(mat)
  rownames(dist_d) <- rownames(mat)
  # Connect between similar strings with an edge
  pairs <- unique(melt(dist_d, varnames = c("seq1", "seq2"), na.rm = TRUE))
  pairs <- pairs[!is.na(pairs$value), ]
  # Generate the graph
  g <- simplify(graph.data.frame(pairs, vertices=rownames(mat), directed=FALSE))
  V(g)$size <- as.vector(mat[, by])
  lou <- cluster_louvain(g)
  l <- layout_with_fr(g)
  V(g)$color <- rainbow(length(unique(lou$membership)),
                        alpha = 0.5)[lou$membership]
  plot(g, layout=l, vertex.label=NA, edge.arrow.size=0, edge.color="gray")
}
