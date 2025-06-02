#' Butterfly dataset
#'
#' A toy dataset used to illustrate fuzzy and evidential clustering algorithms. Also called
#' the 'Diamond' dataset. Adapted from Windham (1985), with one outlier added.
#'
#' @docType data
#'
#' @usage data(butterfly)
#'
#' @format A matrix with 12 rows and 2 column.
#'
#' @keywords datasets
#'
#' @references
#' M.P. Windham. Numerical classification of proximity data with assignment measures.
#' Journal of classification, 2:157-172, 1985.
#'
#' M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy c-means algorithm.
#' Pattern Recognition, Vol. 41, Issue 4, pages 1384-1397, 2008.
#'
#'
#' @examples
#' data(butterfly)
#' plot(butterfly[,1],butterfly[,2],xlab=expression(x[1]),ylab=expression(x[2]))
"butterfly"
