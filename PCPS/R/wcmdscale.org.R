#' @title  Internal function for organize the results of wcmdscale function
#'
#' @description Internal function for organize the results of \code{\link{wcmdscale}} function.
#' The function computes dissimilarity indices using the function \code{\link{vegdist}}
#' and perform Principal Coordinates Analysis (PCoA) using the function 
#' \code{\link{wcmdscale}}. If data is of class dist, the funcion do not computes
#' the dissimilarity indices.
#' 
#' @encoding UTF-8
#' @importFrom vegan vegdist wcmdscale
#' @importFrom stats cor
#' @param data Data matrix or dissimilarities of class dist.
#' @param method Method for dissimilarity index, as accepted by \code{\link{vegdist}}.
#' @param squareroot Logical argument (TRUE or FALSE) to specify if use square root
#' of dissimilarity index.
#' @param eig Logical argument (TRUE or FALSE) to indicates if eigenvalues are returned.
#' @param correlations Logical argument (TRUE or FALSE) to indicates if correlations between axis 
#' and original data are returned.
#' @param ... Other arguments passed to wcmdscale function.
#' @return \item{values}{The eigenvalues, relative eigenvalues and cumulative relative
#' eigenvalues.} \item{vectors}{The principal coordinates.} \item{correlations}{Correlations
#' between axis and original data.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{vegdist}}, \code{\link{wcmdscale}} 
#' @keywords PCPS
#' @export
wcmdscale.org <- function(data, method, squareroot, eig, correlations, ...){
  res <- list()
  if(inherits(data, "dist")){
    data.dist <- data	
  } else {
    data.dist <- vegan::vegdist(data, method = method)
  }
  if (squareroot) {
    data.dist <- sqrt(data.dist)
  }
  data.ordi <- vegan::wcmdscale(data.dist, eig = eig, ...)
  if(eig){
    res$vectors <- data.ordi$points
    values <- data.ordi$eig[data.ordi$eig>=0]
    res$values <- data.frame(Eigenvalue = values, Relative_eig = values/sum(values), Cumul_eig = cumsum(values/sum(values)))
    if(any(data.ordi$eig<0)){
      warning("Warning: Negative eigenvalues are present in the decomposition result, but only positive eigenvalues were returned", call. = FALSE)
    }
  } else {
    res$vectors<-data.ordi
  }
  colnames(res$vectors) <- paste("Axis.",seq_len(ncol(res$vectors)), sep = "")
  if(correlations){
    res.cor <- stats::cor(data, res$vectors)
    res$correlations <- res.cor
  }
  return(res)
}