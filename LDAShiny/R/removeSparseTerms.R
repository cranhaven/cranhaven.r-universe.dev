#' removeSparseTerms Remove Sparse Terms from a
#' Term-Document Matrix function original pakage tm
#'@importFrom tm TermDocumentMatrix
#' @param x A \code{DocumentTermMatrix} or a \code{TermDocumentMatrix}
#' @param sparse A numeric for the maximal allowed sparsity in the range
#' from bigger zero to smaller one.
#'@usage
#'removeSparseTerms(x, sparse)
#' @return A term-document matrix where those terms from \code{x} are removed
#' which have at least a sparse percentage of empty
#' (i.e., terms occurring 0 times in a document) elements. I.e.,
#' the resulting matrix contains only terms with a sparse factor
#'  of less than \code{sparse}
#'@examples \donttest{
#' data("crude")
#' require(tm)
#' tdm <- tm::TermDocumentMatrix(crude)
#' removeSparseTerms(tdm, 0.3)
#' }
#'
#'
#' @export

removeSparseTerms <- function (x, sparse)
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")),
            is.numeric(sparse), sparse > 0, sparse < 1)
  m <- if (inherits(x, "DocumentTermMatrix"))
    t(x)
  else x
  t <- table(m$i) > m$ncol * (1 - sparse)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix"))
    x[, termIndex]
  else x[termIndex, ]
}
