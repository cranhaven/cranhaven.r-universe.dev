#' Create an Adjacency Matrix for a set of Preferences
#'
#' Convert a set of preferences to an adjacency matrix summarising wins
#' and losses between pairs of items
#'
#' For a `preferences` object with \eqn{N} items, the adjacency
#' matrix is an \eqn{N} by \eqn{N} matrix, with element \eqn{(i, j)} being the
#' number of times item \eqn{i} wins over item \eqn{j}. For example, in the
#' preferences \{1\} > \{3, 4\} > \{2\}, item 1 wins over items 2, 3, and 4,
#' while items 3 and 4 win over item 2.
#'
#' If `weights` is specified, the values in the adjacency matrix are the
#' weighted counts.
#'
#' @param object a [`preferences`][preferences] object, or an object that can be
#' coerced by `as.preferences`.
#' @param weights an optional vector of weights for the preferences.
#' @param ... further arguments passed to/from methods.
#'
#' @return An \eqn{N} by \eqn{N} matrix, where \eqn{N} is the number of
#' items.
#'
#' @examples
#' X <- matrix(c(
#'   2, 1, 2, 1, 2,
#'   3, 2, 0, 0, 1,
#'   1, 0, 2, 2, 3
#' ), nrow = 3, byrow = TRUE)
#' X <- as.preferences(X, format = "ranking", item_names = LETTERS[1:5])
#' adjacency(X)
#'
#' adjacency(X, weights = c(1, 1, 2))
#'
#' @export
adjacency <- function(object, weights = NULL, ...) {
  if (!inherits(object, "preferences")) {
    object <- as.preferences(object)
  }
  n <- ncol(object)
  if (is.null(weights)) {
    weights <- rep.int(1L, nrow(object))
  } else {
    stopifnot(length(weights) == nrow(object))
  }
  nset <- apply(object, 1L, function(x) max(na.omit(x)))
  m <- max(nset)
  nm <- colnames(object)
  x <- matrix(0.0, nrow = n, ncol = n, dimnames = list(nm, nm))
  for (i in seq_len(m)) {
    r <- which(nset >= (i + 1L))
    for (j in r) {
      # > gives rest; == i + 1 gives next best
      one <- which(unclass(object[j, ]) == i)
      two <- which(unclass(object[j, ]) > i)
      x[one, two] <- x[one, two] + weights[j]
    }
  }
  structure(x, class = c("adjacency", "matrix"))
}
