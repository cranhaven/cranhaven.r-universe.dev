#' rwclust class constructor
#'
#' Returns a object of class "rwclust" for use with generic summary and plotting functions.
#'
#' @param x output of \code{run_main_loop} function
#' @seealso [run_main_loop()]
new_rwclust <- function(x) {
    structure(x, class="rwclust")
}

#' Generic plotting for rwclust object
#'
#' Generic function for plotting the distribution of weights. Calls \code{hist} under the hood.
#'
#' @param x rwclust object
#' @param cutoff optional numeric, will plot the cutoff value as a vertical line
#' @param ... additional graphical parameters passed to the \code{hist} function
# 
#' @importFrom checkmate assert_number
#' @importFrom graphics hist
#' @importFrom graphics abline
#' @export
plot.rwclust <- function(x, cutoff=NULL, ...) {
    hist(x$weights, main="Distribution of Edge Weights", xlab="Edge Weights", ...)
    if (!is.null(cutoff)) {
        assert_number(cutoff)
        abline(v=cutoff, col="red")
    }
}


# #' Generic helper for extracting weights from rwclust object
# #' @param x rwclust object
# #' @return a numeric vector
# #' @export
# weights.rwclust <- function(x) {
#     attributes(x) <- NULL
#     x
# }


#' Generic helper for extracting adjacency matrix from rwclust object.
#' @param x rwclust object
#' @return Matrix object containing the adjacency matrix of the after the final iteration
#' @export
adjacency <- function(x) {
    UseMethod("adjacency")
}

#' @rdname adjacency
#' @export
adjacency.default <- function(x) {
    stop("x must be of class rwclust")
}

#' @rdname adjacency
#' @export
adjacency.rwclust <- function(x) {
    x$adj
}