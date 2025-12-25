#' Add New Cut Thresholds
#'
#' Add new manual cuts to the \code{cuts} matrix generated using \code{\link{make.cut}}
#'
#' @param cuts a list of cuts generated using \code{\link{make.cut}}
#' @param new.cuts a list of new cut thresholds to be added to \code{cuts}
#' @param cut.id string identifying the new cuts
#' @param update if FALSE (the default) adding a \code{cut.id} that already exists in \code{cuts}
#'        will return an error
#'
#' @details
#' The matrix can be cut using either the fixed cuts (\code{type='fixed'}), or the combined cuts (\code{type='combined'})
#' where the limits have been adjusted to match local minima and maxima.
#'
#' @return an updated \code{cuts} matrix with an extra set of thresholds named \code{cut.id}.
#'
#' @example examples/example.cut.R
#'
#' @author Yann Abraham
#' @export
add.cut <- function(cuts,new.cuts,cut.id='manual',update=FALSE) {
  if(!all(names(cuts) %in% names(new.cuts))) {
    stop('Column names are not matching: please check that all column names are')
  }
  if(cut.id %in% names(cuts[[1]][['cuts']]) & !update) {
    stop(cut.id,' already exists in cuts: please use another identifier or set update to FALSE')
  }
  res <- lapply(names(cuts),function(cur.id) {
    cur.cut <- cuts[[cur.id]]
    cur.cut[['cuts']][[cut.id]] <- new.cuts[[cur.id]]
    return(cur.cut)
  })
  names(res) <- names(cuts)
  return(res)
}
