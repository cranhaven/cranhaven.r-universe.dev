#' Sharpen the edge weights of a weighted graph.
#' 
#' Sharpens the weights of a weighted graph for later pruning.
#' 
#' @section Details:
#' Internally, the edgelist passed to \code{rwclust} is converted 
#' into a transition matrix, whose powers are used to compute the 
#' probability of reaching a vertex \eqn{u} from vertex \eqn{v} in 
#' \eqn{k} steps for all \eqn{v} and \eqn{u}. New edge weights are computed 
#' using the similarity between these "walk probabilities" for each pair
#' of vertices. The intuition is that vertices who have similar 
#' neighborhoods in terms of random walk reachability are similar 
#' to each other. 
#' 
#' The returned weights can be used for clustering by deleting 
#' edges with weights below a certain threshold. The connected 
#' components of the resulting graph form the clusters. 
#' 
#' @references
#' Harel, David, and Yehuda Koren. "On clustering using random walks." 
#' International Conference on Foundations of Software Technology 
#' and Theoretical Computer Science. Springer, Berlin, Heidelberg, 2001.
#' 
#' @param x matrix or dataframe with three columns
#' \enumerate{
#'  \item vertex label (integer)
#'  \item vertex label (integer)
#'  \item edge weights (float)
#' }
#' @param similarity string, the name of the similarity metric used to 
#' update weights
#' @param iter integer, number of iterations
#' @param k integer, maximum length of random walk
#' 
#' @return list
#' \describe{
#'  \item{weights}{A vector of the updated edge weights}
#'  \item{adj}{Updated adjacency matrix containing updated weights}
#' }
#' 
#' @importFrom checkmate assert_count
#' @importFrom checkmate qassert
#' @export
rwclust <- function(x, iter = 5, k = 3, similarity = "hk") {
  assert_count(iter, positive = TRUE)
  assert_count(k, positive = TRUE)
  qassert(k, "N[2, Inf)")
  
  UseMethod("rwclust")
}

#' @export
rwclust.default <- function(x, iter = 5, k = 3, similarity = "hk") {
  stop("x must be a 3-column data frame or matrix")
}

#' @rdname rwclust
#' @export
rwclust.data.frame <- function(x, iter = 5, k = 3, similarity = "hk") {
  rwclust(as.matrix(x), iter, k, similarity)
}

#' @rdname rwclust
#' @export
rwclust.matrix <- function(x, iter = 5, k = 3, similarity = "hk") {
  x <- format_and_check_dataframe(x)
  similarity <- retrieve_similarity_function(similarity)
  
  adj <- Matrix::sparseMatrix(
    i = x[, 1],
    j = x[, 2],
    x = x[, 3],
    symmetric = TRUE
  )
  
  run_main_loop(adj, x, similarity, k, iter)
}
