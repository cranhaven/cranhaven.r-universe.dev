# @export
#setClass("ps")

#devtools::use_package("emoa")
#' Creates a pareto set from given data
#'
#' Return those points which are not dominated by another point in \code{y} This
#' is the Pareto front approximation of the design set.
#'
#' @param y design space data
#' @param minimization logical representing if the set is to be minimized or not
#' @param light.return logical indicating if the indexes should be written on the
#'   'ps' object
#' @return S3 class object that contains information of the Pareto set
#' @export
#' @examples
#' aps <- ps(matrix(rnorm(1:1000),ncol=2))
#' print(aps)
#'
ps <- function(y, minimization=TRUE, light.return=FALSE){
  y <- t(y)
  if (minimization)
    set <- t(emoa::nondominated_points(y))
  else
    set <- t(-emoa::nondominated_points(-y))
  n <- nrow(set)
  m <- ncol(set)
  colnames(set) <- paste0('f',1:m)
  if (light.return)
    ps <- list(set=set, index=NULL, m=m, n=n)
  else {
    index <- which(apply(matrix(t(y) %in% set, ncol=ncol(set)),1,all))
    ps <- list(set=set,index=index,m=m,n=n)
  }
  class(ps) <- 'ps'
  return(ps)
}
