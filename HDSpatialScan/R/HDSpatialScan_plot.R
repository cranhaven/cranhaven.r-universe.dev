########################################################################################################################################################
##' @title Schema or map of the clusters
##'
##' @description This function plots a schema or a map of the sites and the clusters
##'
##' @param x ResScanOutput. Output of a scan function (UG, UNP, MG, MNP, PFSS, DFFSS, URBFSS, NPFSS, MPFSS, MDFFSS or MRBFSS)
##' @param type character. Type of plot: "schema", "map" (the clusters are represented by circles) or "map2" (the clusters are colored on the map)
##' @param spobject SpObject. SpatialObject with the same coordinates system the one used for the scan. Only considered if type is "map" or "map2"
##' @param system_conv character. System to convert the coordinates for the plot. Only considered if the system used in the scan was "WGS84" and if type is "schema". Else it will be ignored. Must be entered as in the PROJ.4 documentation
##' @param colors character. Colors of the clusters. If length(colors)=1 all the clusters will be in this color. Else it should be a vector of length the number of clusters to plot.
##' @param only.MLC logical. Should we plot only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' coords <- coordinates(map_sites)
##'
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$NPFSS
##'
##' plot(x = res_npfss, type = "schema", system_conv = "+init=epsg:2154")
##' plot(x = res_npfss, type = "map", spobject = map_sites)
##' plot(x = res_npfss, type = "map2", spobject = map_sites)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data[indices,],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' plot(x = res_npfss, type = "schema", system_conv = "+init=epsg:2154")
##' plot(x = res_npfss, type = "map", spobject = map_sites[indices,])
##' plot(x = res_npfss, type = "map2", spobject = map_sites[indices,])
##' }
##'
##' }
##'
##' @method plot ResScanOutput
##'
##' @return No value returned, plots a schema or a map of the sites and the clusters.
##'
##' @export
##'
##'
plot.ResScanOutput <- function(x, type, spobject = NULL, system_conv = NULL, colors = "red", only.MLC = FALSE, ...){

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(x$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(length(type) != 1){
    stop("Only one type must be provided")
  }
  if(!(type %in% c("schema", "map", "map2"))){
    stop("type must be schema, map or map2")
  }

  if(is.null(spobject)){
    if(type != "schema"){
      stop("For type = map or map2, spobject must be provided")
    }
  }

  if(is.null(x$sites_coord)){
    stop("x must contain a non null sites_coord argument")
  }

  if(type == "schema"){
    if(only.MLC == TRUE){
      plot_schema(output_clusters = x$sites_clusters[1], sites_coord = x$sites_coord, system = x$system, system_conv = system_conv, colors = colors)
    }else{
      plot_schema(output_clusters = x$sites_clusters, sites_coord = x$sites_coord, system = x$system, system_conv = system_conv, colors = colors)
    }
  }
  if(type == "map"){
    if(only.MLC == TRUE){
      plot_map(spobject = spobject, centres = x$centres_clusters[1,,drop = FALSE], radius = x$radius_clusters[1], system = x$system, colors = colors)
    }else{
      plot_map(spobject = spobject, centres = x$centres_clusters, radius = x$radius_clusters, system = x$system, colors = colors)
    }
  }
  if(type == "map2"){
    if(only.MLC == TRUE){
      plot_map2(spobject = spobject, sites_coord = x$sites_coord, output_clusters = x$sites_clusters[1], system = x$system, colors = colors)
    }else{
      plot_map2(spobject = spobject, sites_coord = x$sites_coord, output_clusters = x$sites_clusters, system = x$system, colors = colors)
    }
  }

}
