#' Generate the Hilbert Index from a Cut Reference Matrix
#'
#' Generate the Hilbert Index corresponding to the sub-spaces defined by the coordinates
#' generated \emph{via} \code{\link{do.cut}}
#'
#' @param mat the cut reference matrix
#' @param horder the Hilbert order, \emph{i.e.} the number of bins in each dimension
#' @return a vector of indices, one for each line in \code{mat}
#' @details
#' For each line in \code{mat}, the function will compute the corresponding
#' \href{https://en.wikipedia.org/wiki/Hilbert_curve}{Hilbert index}. Each index corresponds to a specific
#' sub-cube of the original high-dimensional space, and consecutive hilbert index correspond to adjacent sub-cubes
#' @examples
#' # generate a random 3D matrix
#' mat <- matrix(rnorm(300),ncol=3)
#' dimnames(mat)[[2]] <- LETTERS[1:3]
#' # generate 2 bins with a minimum bin size of 5
#' cuts <- make.cut(mat,n=3,count.lim=5)
#' show.cut(cuts)
#' # Generate the cuts
#' cut.mat <- do.cut(mat,cuts,type='fixed')
#' head(cut.mat)
#' # generate the Hilber index
#' hc <- do.hilbert(cut.mat,2)
#' plot(table(hc),type='l')
#' @author Marilisa Neri
#' @author Yann Abraham
#' @author John Skilling (for the original \code{C} function)
#' @export
do.hilbert <- function(mat,horder) {
  horder <- log2(horder)
  x1d <- hilbertMapping(mat,horder)
  return(x1d)
}
