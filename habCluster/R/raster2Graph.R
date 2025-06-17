
#' Create a graph from an raster according the connection between cells
#'
#' @param r  An object of stars or RasterLayer. The value of each cell of the raster is the ‘smoothness’ to indicate how easy the cell connecting with neighbor cells.
#' @param cellsize Numeric. Re-sample the input raster to given resolution and use the re-sampled raster to build graph. Set this to NULL if using the original resolution of of the input raster.
#' @param relative.distance Boolean. If fasle, absolute distance between cells is used to compute the edge weight; otherwise, relative distance between cells is used. Default is true
#' @param silent Boolean. A logical indicating if some “progress report” should be given. Default is TRUE.
#'
#' @return a list with an graph and the re-sampled raster (a object of stars). The graph is igraph object, with cells as node and connections as weight.
#' @export
#'
#' @examples
#' # read in habitat suitability data of wolf in Europe
#' library(stars)
#' hsi.file = system.file("extdata","wolf3_int.tif",package="habCluster")
#' wolf = read_stars(hsi.file)
#' # build graph from raster
#' g = raster2Graph(wolf, 40000)

raster2Graph  <- function(r, cellsize=NULL, relative.distance = TRUE, silent=TRUE){

  if(is(r,"RasterLayer")){
    r = stars::st_as_stars(r)
  }

  r2 = NULL
  if(!is.null(cellsize)){
    if(!silent){
    cat('\nresampling...')
    }
    r2 = stars::st_warp(r, crs=sf::st_crs(r), cellsize = cellsize)
  }else{
    r2 = r
  }

  matrix = r2[[1]]
  if(!silent){
  cat('\nextracting edges...')
  }
  edf = getEdgeDF(matrix)
  if(relative.distance == FALSE){
    dim = stars::st_dimensions(r2)
    res.x = abs(dim$x$delta)
    res.y = abs(dim$y$delta)
    if(res.x != res.y){
      stop("Cell size in the x and y dimension are not equal.")
    }

    edf$weight = edf$weight * res.x
  }
  if(!silent){
  cat('\ncreate graph...')
  }
  g =list()
  g$graph = igraph::graph_from_data_frame(edf, directed=F)
  g$raster = r2
  return(g)
}

