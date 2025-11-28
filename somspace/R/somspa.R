#' Spatial SOM
#' 
#' @description `somspa` creates a Self-Organizing Map from spatial data.
#'
#' @param x A `sominp` object.
#' @param ... Other arguments passed to methods from `kohonen::som` function which is used to create the SOM.
#' 
#' @details `x` should be created by `sominp`. 
#' The output `somsp` objects can be plotted by `plot` and `plot_ts` functions or summarized by `summary`
#' 
#' @return A `somsp` object, which contains: 
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
#' @seealso \code{\link{som}} 
#' @seealso \code{\link{sominp}} 
#' 
#' @examples
#' \donttest{
#' dummy <- owda[Time <= 1600] #toy example
#' inp_som <- sominp(dummy)
#' 
#' my_som <- somspa(inp_som, rlen = 100, grid = somgrid(3, 3, "hexagonal"))
#' my_som$summary
#' my_som$som
#' 
#' plot(my_som)
#' plot_ts(my_som, n = 3)
#' plot_ts(my_som, n = c(1, 2, 4, 9)) 
#' plot_ts(my_som, n = 1:max(my_som$summary$node)) #plots all soms}
#' 
#' @rawNamespace import(kohonen, except = map)
#' @export

somspa <- function(x, ...){
  x$som <- som(X = x$input_for_som, ...) 
  som_results <- data.table(id = x$coords$id, 
                            lat = x$coords$lat, 
                            lon = x$coords$lon,
                            node = x$som$unit.classif, 
                            distance = x$som$distances)
  som_results[, node_lat := median(lat), by = node]
  som_results[, node_lon := median(lon), by = node]
  som_results[, node_sd_lat := sd(lat), by = node]
  som_results[, node_sd_lon := sd(lon), by = node]
  som_results[, node_counts := .N, by = node]
  out <- list(summary = som_results, 
              som = x$som, 
              input_dt = merge(som_results[, .(id, node)],
                               x$input_dt, 
                               by = 'id'))
  class(out) <- 'somsp'
  return(out)
}

globalVariables(c("node", "node_lat", "node_lon", "node_sd_lat", "node_sd_lon", "node_counts"))

