#' Apply Cuts to the Reference Matrix
#'
#' Apply cuts generated using the \code{\link{make.cut}} function to the reference matrix
#'
#' @param mat the matrix to cut
#' @param cuts a list of cuts generated using \code{\link{make.cut}}
#' @param type the type of cuts to use (use \code{combined} by default)
#'
#' @details
#' The matrix can be cut using either the fixed cuts (\code{type='fixed'}), or the combined cuts (\code{type='combined'})
#' where the limits have been adjusted to match local minima and maxima.
#' Returned values correspond to the bin defined between the first and second threshold of the specified \code{cuts},
#' then between the second and third threshold, and so on. The values will range between 0 (the first bin) and \code{n-1} where
#' \code{n} is the number of values in the specified \code{cuts}.
#'
#' @return a matrix of the same dimensionality as \code{mat} where values correspond to bins defined by the \code{type}
#'         thresholds defined \code{cuts}.
#'
#' @example examples/example.cut.R
#'
#' @author Yann Abraham
#' @export
do.cut <- function(mat,cuts,type='combined') {
  types <- lapply(cuts,function(x) names(x[['cuts']]))
  types <- unlist(types)
  types <- unique(types)
  if(is.na(pmatch(type,types))) {
    stop(paste(type,'is not a recognized cut type - valid values are',paste(types,sep='',collapse=', ')))
  } else {
    type <- types[pmatch(type,types)]
  }
  if(!all(names(cuts) %in% dimnames(mat)[[2]])) {
    stop('Some cuts are not found in the reference matrix')
  }
  cur.ch.cut <- lapply(names(cuts),function(cur.ch) {
      cut(mat[,cur.ch],
          cuts[[cur.ch]][['cuts']][[type]],
          include.lowest=T,
          labels=F)
    }
  )
  cur.ch.cut <- do.call('cbind',cur.ch.cut)
  dimnames(cur.ch.cut)[[2]] <- names(cuts)
  cur.ch.cut <- cur.ch.cut-1
  return(cur.ch.cut)
}
