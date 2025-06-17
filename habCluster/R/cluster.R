

#' Clustering cells from a raster by Community Detection Algorithm according to the connections between them and return a cluster map
#'
#' @description This function use Community Detection Algorithm to find structure of raster and return a polygon representing the boundary of the clusters.
#'
#' @param r An object of stars or RasterLayer. The value of each cell of the raster is the ‘smoothness’ to indicate how easy the cell connecting with neighbor cells.
#' @param method method from package igraph used to finding community structure. (see details below).
#' @param cellsize Numeric. Re-sample the input raster to given resolution and use the resampled raster to find community structure. Set this to NULL if using the original resolution of of the input raster,given the parameter r is an object of raster.
#' @param relative.distance Boolean. If FALSE, absolute distance between cells is used to compute the edge weight; otherwise, relative distance between cells is used . Default is TRUE.
#' @param silent Boolean. A logical indicating if some “progress report” should be given. Default is TRUE.
#' @param ... Optional arguments to method. For example, can set resolution_parameter for cluster_leiden, or resolution for cluster_louvain.(see details below).
#'
#' @details Choice of the method used to finding community structure(see Mukerjee, 2021). The default method is cluster_fast_greedy, but could also be methods like cluster_leiden, cluster_walktrap, or cluster_louvain. If cluster_leiden is chosen, then we can use resolution_parameter to control the size of clusters. Higher resolution_parameter lead to more smaller clusters, while lower resolution_parameter lead to fewer larger clusters. The parameter of resolution for cluster_louvain is similar. More details about those methods can be found in the document for package "igraph".
#' @return A polygon of sf object for boundaries of habitat clusters,
#' and an object of communities defined in package igraph.
#'
#' @references
#' Mukerjee, S. (2021). A systematic comparison of community detection algorithms for measuring selective exposure in co-exposure networks. Scientific reports 11, 15218. https://doi.org/10.1038/s41598-021-94724-1\cr
#' Traag, V. A., Waltman, L., & van Eck, N. J. (2019). From Louvain to Leiden: guaranteeing well-connected communities. Scientific reports, 9(1), 5233. doi: 10.1038/s41598-019-41695-z\cr
#'
#' @export
#'
#' @examples
#' library(sf)
#' library(stars)
#'
#' # read in habitat suitability data of wolf in Europe
#' hsi.file = system.file("extdata","wolf3_int.tif",package="habCluster")
#' wolf = read_stars(hsi.file)
#'
#' # rescale raster value to 0 - 1
#' wolf = wolf / 100
#'
#' # find habitat cluster using Fast Greedy Algorithm.
#' # Raster will be resampled to 40 km, to cluser at the scale of 40 km and reduce calculation amount.
#' clst = cluster(wolf, method = cluster_fast_greedy, cellsize = 40000)
#'
#' # plot the results
#' image(wolf,col=terrain.colors(100,rev = TRUE),asp = 1)
#' boundary = clst$boundary
#' plot( boundary$geometry, add=TRUE, asp=1, border = "lightseagreen")
#'
#' # discard patches smaller than 1600 sqkm
#' boundary$area = as.numeric(st_area(boundary))
#' boundary = boundary[boundary$area > 40000*40000,]
#' image(wolf,col=terrain.colors(100,rev = TRUE),asp = 1)
#' plot( boundary$geometry, add=TRUE, asp=1, border = "lightseagreen")
#'
#' # can also use RasterLayer object#
#' library(raster)
#' wolf = read_stars(hsi.file)
#' wolf = wolf / 100
#' clst = cluster(wolf, method = cluster_leiden, cellsize = 40000, resolution_parameter = 0.0002)


cluster <- function(r=NULL, method=igraph::cluster_fast_greedy, cellsize=NULL, relative.distance = TRUE, silent=TRUE,...){

  g = raster2Graph(r, cellsize = cellsize, relative.distance = relative.distance, silent = silent)

  if(!silent){
    cat('\nfinding clusters...')
  }

  if(identical(method, igraph::cluster_leiden)){
    clusters = igraph::cluster_leiden(g$graph,...)
  }else{
    clusters = method(g$graph,...)
  }
  if(!silent){
  cat('\npreparing results...')
  }

  cluster.id = igraph::membership(clusters)
  cell.id = as.integer(names(cluster.id))
  cluster.raster = g$raster
  cluster.raster[[1]][cell.id] = cluster.id
  boundaries = sf::st_as_sf(cluster.raster,merge=TRUE)
  names(boundaries)[1] = 'cluster.id'
  out=list()
  out$boundary = boundaries
  out$communities = clusters
  return(out)
}
