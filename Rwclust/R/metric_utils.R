#' Apply similarity function to rows of a matrix
#' 
#' @param idx vector of length two containing row indices
#' @param mat a matrix
#' @param similarity similarity function to apply
#' @param ... additional parameters to be passed to the similarity function
#' 
#' @return a scalar
apply_similarity <- function(idx, mat, similarity, ...) {
  similarity(mat[idx[1],], mat[idx[2],], ...)
}

#' Apply similarity function over edges of graph
#' 
#' @param edgelist 3-column dataframe
#' @param mat a matrix
#' @param similarity the similarity function to apply
#' @param ... other parameters to pass to the similarity function
#' 
#' @return a vector containing updated weights
compute_similarities <- function(edgelist, mat, similarity, ...) {
  apply(
    X = edgelist,
    FUN = apply_similarity,
    MARGIN = 1,
    mat = mat, 
    similarity = similarity, 
    ...
  )
}