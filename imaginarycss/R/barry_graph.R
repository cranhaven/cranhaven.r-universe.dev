# Function to stop if the object is not of class barry_graph
stopifnot_barry_graph <- function(x) {

  if (!inherits(x, "barry_graph")) {
    stop(
      "The passed object is not of class `barry_graph`, it is of ",
      "class(es): '", paste(class(x), collapse="', '"), "'."
      )
  }

}

#' Convert to Integer
#' 
#' @description
#' Converts input to integer while preserving dimensions and checking for 
#' non-integer values.
#' 
#' @param x Numeric vector or matrix to convert to integer.
#' 
#' @return
#' Integer vector or matrix with same dimensions as input.
#' 
#' @details
#' This function checks that all values in the input are integers (within
#' machine precision) before converting. If any values are not integers,
#' an error is thrown. For matrices, the dimension attribute is preserved.
#' 
#' @noRd
#' @examples
#' \dontrun{
#' to_integer(c(1, 2, 3))
#' to_integer(matrix(1:6, nrow = 2))
#' }
#' 
#' @keywords internal
to_integer <- function(x) {
  fcall <- as.character(match.call()[[2]])
  if (any(abs(round(x) - x) > .Machine$double.eps ^ 0.5))
    stop("Values in ", fcall, " must be integer")
  # Case of a vector 
  if (!length(dim(x))) {
    as.integer(x)
  }
  # Case of a matrix
  structure(
    as.integer(x),
    dim = dim(x)
  )
}
#' Binary Array Graph
#' @param x Either a matrix or a list of matrices, or an object of class 
#' `barry_graph`.
#' @param ... Currently ignored. 
#' @examples
#' # Using the Krackhardt advice network
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' # Convert edge-list data frame to adjacency matrix
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#' krack_graph
#'
#' # Network size and number of networks
#' netsize(krack_graph)
#' nnets(krack_graph)
#' @return
#' `new_barry_graph()` returns an external pointer object of class
#' `"barry_graph"` with attributes `netsize` (integer scalar giving the
#' size of each individual network) and `endpoints` (integer vector
#' marking the boundary rows of the stacked networks).
#' @export
#' @aliases barry_graph
new_barry_graph <- function(x, ...) UseMethod("new_barry_graph")
#' @export
#' @param n Integer. The size of the original network.
#' @details
#' When `x` is a matrix, it is assumed that it will be a block
#' diagonal matrix, with the first block corresponding to the reference
#' (true) network. 
#' 
#' If `x` is a list, the first matrix is assumed to be the reference
#' (true) network.
#' 
#' @rdname new_barry_graph
new_barry_graph.matrix <- function(x, n, ...) {
  
  # Checking that size matches n
  if (diff(dim(x)) != 0L)
    stop("-x- must be a square matrix.", call. = FALSE)
  
  # This must be evenly able to divide
  N <- nrow(x)
  if (N %% n)
    stop("The modulo between nrow(x) and n is not zero.", call. = FALSE)
  
  if (n < 1L)
    stop("-n- cannot less than 1L", call. = FALSE)
  
  # Identifying the endpoints
  n <- as.integer(n)
  n_nets    <- as.integer(N %/% n)
  # It cannot be a single network
  if (n_nets < 2L)
    stop("The number of networks must be at least 2L.", call. = FALSE)
  endpoints <- cumsum(rep(n, times = n_nets - 1L)) + n
  
  edgelist <- which(x != 0L, arr.ind = TRUE) - 1L
  
  edgelist <- to_integer(edgelist)
  endpoints <- to_integer(endpoints)
  new_barry_graph_cpp(N, edgelist[, 1L], edgelist[, 2L], n, endpoints)
  
}
#' @export
#' @rdname new_barry_graph
new_barry_graph.list <- function(x, ...) {
  
  # Checking all have the same size
  n <- unique(as.vector(sapply(x, dim)))
  if (length(n) != 1L)
    stop("All matrices in -x- should be of the same length.", call. = FALSE)
  
  # Should be of length n
  if (n < 1L)
    stop(
      "The size of the adjacency matrices in -x- should be at least 2L.",
      call. = FALSE
    )
  
  edgelists <- lapply(x, function(x.) {
    which(x. != 0L, arr.ind = TRUE)
  })
  
  # Adjusting indices
  for (i in seq_along(edgelists))
    edgelists[[i]] <- (edgelists[[i]] - 1L) + (i - 1L) * n
  
  edgelists <- do.call(rbind, edgelists)
  
  # Identifying the endpoints
  n_nets    <- length(x)
  endpoints <- cumsum(rep(n, times = n_nets - 1L)) + n
  edgelists <- to_integer(edgelists)
  endpoints <- to_integer(endpoints)
  if (any(edgelists < 0))
    stop("Edgelist cannot have negative values")
  if (any(endpoints < 0))
    stop("Endpoints cannot have negative values")
  
  # Creating the edgelist
  new_barry_graph_cpp(n * n_nets, edgelists[, 1], edgelists[, 2], n, endpoints)
  
}
#' Print Barry Graph
#' 
#' @description
#' Print method for barry_graph objects.
#' 
#' @param x A barry_graph object.
#' @param n Integer. Number of nodes to display (default: min of 10 and
#'   the network size).
#' @param ... Additional arguments passed to print (currently ignored).
#' 
#' @return
#' Invisibly returns the input object. Called for its side effect of printing.
#' 
#' @examples
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#' print(krack_graph)
#' @export
print.barry_graph <- function(x, n = min(10, netsize(x)), ...) {

  if ((n != as.integer(n)) || (n <= 0) || n > (netsize(x) * nnets(x)))
    stop(
      "`n` should be an integer within 1 and netsize(x) * nnets(x). ",
      "It is ", n
      )

  suppressWarnings(print_barry_graph_cpp(x, as.integer(n)))
}

#' @export
#' @return
#' The function `netsize()` returns the size of individual networks
#' (all matching).
#' @rdname new_barry_graph
netsize <- function(x) {

  stopifnot_barry_graph(x)
  attr(x, "netsize")

}

#' @export
#' @rdname new_barry_graph
#' @return
#' `nnets()` returns the number of graphs contained in the `barry_graph`
#' object. 
nnets <- function(x) {

  stopifnot_barry_graph(x)
  length(attr(x, "endpoints"))

}

