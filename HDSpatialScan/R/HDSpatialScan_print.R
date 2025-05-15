########################################################################################################################################################
##' @title Prints a result of a scan procedure
##'
##' @description This function prints a result of a scan procedure.
##'
##' @param x ResScanOutput. Output of a scan function (UG, UNP, MG, MNP, PFSS, DFFSS, URBFSS, NPFSS, MPFSS, MDFFSS or MRBFSS)
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
##' print(x = res_npfss)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data[indices,],
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2,
##' MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' print(x = res_npfss)
##' }
##'
##' }
##'
##' @method print ResScanOutput
##'
##' @return No value returned, print the ResScanOutput object.
##'
##' @export
##'
print.ResScanOutput <- function(x, ...){

  name <- paste(x$method, "scan procedure \n", sep = " ")
  cat(name)
  cat(paste(paste(rep("#", nchar(name)), collapse = ""), "\n", sep = " "))


  if(length(x$sites_clusters) == 0){
    cat("No significant cluster has been detected by the scan procedure \n")
  }else{
    if(length(x$sites_clusters) == 1){
      cat(paste(length(x$sites_clusters), "significant cluster has been detected by the scan procedure with p-value = ", x$pval_clusters[1], "\n" ))
    }else{
      cat(paste(length(x$sites_clusters), "significant clusters have been detected by the scan procedure with p-values of", paste(x$pval_clusters, collapse = ", "), "\n" ))
    }
  }

}
