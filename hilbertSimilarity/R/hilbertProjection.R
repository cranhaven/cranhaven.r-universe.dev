#' Project a Cut Reference Matrix to a Different Space through an Hilbert Index
#'
#' Starting from a Hilbert Index generated in a high dimensional space, returns a set of coordinates
#' in a new (lower) dimensional space
#'
#' @param hc the hilbert index returned by \code{\link{do.hilbert}}
#' @param target the number of dimensions in the target space (defaults to 2)
#' @return a matrix with \code{target} columns, corresponding to
#' the projection of each Hilbert index to \code{target} dimensions
#' @details
#' Based on the maximum index and the targeted number of dimensions the number of target bins is computed and used
#' to generate a reference matrix and a reference index. The reference matrix is returned, ordered by the reference index.
#'
#' @example examples/example.projection.R
#'
#' @author Marilisa Neri
#' @author Yann Abraham
#' @author John Skilling (for the original \code{C} function)
#' @export
hilbertProjection <- function(hc,target=2) {
  targetb <- max(hc)^(1/target)
  targetb <- log2(targetb)
  targetb <- 2^ceiling(targetb)
  cat('Projecting the Hilbert curve to',target,'dimensions will require',targetb,'bins\n')
  ref <- lapply(seq(target),function(i) seq(targetb)-1)
  ref <- as.matrix(expand.grid(ref))

  phc <- do.hilbert(ref,targetb)
  proj <- ref[order(phc),]
  colnames(proj) <- c('V1','V2')
  return(proj)
}
