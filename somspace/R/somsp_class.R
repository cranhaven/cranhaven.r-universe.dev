#' Spatial SOM class
#' 
#' @details The `somsp` objects are created by `somspa` function and contain: 
#' 
#' \itemize{
#' 
#' \item{A summary `data.table` with the coordinates of each SOM node, the distances of objects 
#' to their corresponding winning unit, the number of points of each node, as well as the median 
#' latitude and longitude of each node coordinates and their standard deviation.}
#' 
#' \item{A Self-Organizing Map object (see also \code{\link{kohonen}}).}
#' 
#' \item{The `sominp` object used as input for the SOM, with an id number coressponding to 
#' location and a node number to the classification group of SOM.}
#' }
#' 
#' They can be plotted by `plot` and `plot_ts` functions or summarized by `summary`.
#' 
#' @seealso \code{\link{somspa}}
#' @seealso \code{\link{plot_ts}}
#' 
#' @export

somsp <- list()
class(somsp) <- "somsp"


