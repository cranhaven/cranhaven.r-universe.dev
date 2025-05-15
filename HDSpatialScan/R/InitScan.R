################################################################
##' @title Initalization of the scan procedures by creating the matrix of potential clusters
##'
##' @description This function initializes the scan procedures by creating the matrix of potential clusters.
##'
##' @param mini_post numeric. A minimum to filter the significant clusters a posteriori (see type_minimaxi_post). The default NULL is for no filtering with a a posteriori minimum.
##' @param maxi_post numeric. A maximum to filter the significant clusters a posteriori (see type_minimaxi_post). The default NULL is for no filtering with a a posteriori maximum.
##' @param type_minimaxi_post character. Type of minimum and maximum a posteriori: by default "sites/indiv": the mini_post and maxi_post are on the number of sites or individuals in the significant clusters. Other possible values are "area": the minimum and maximum area of the clusters, or "radius": the minimum and maximum radius.
##' @param sites_areas numeric vector. Areas of the sites. It must contain the same number of elements than the rows of sites_coord. If the data is on individuals and not on sites, there can be duplicated values. By default: NULL
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param mini integer. A minimum for the clusters (see type_minimaxi). Changing the default value may bias the inference.
##' @param maxi integer. A Maximum for the clusters (see type_minimaxi). Changing the default value may bias the inference.
##' @param type_minimaxi character. Type of minimum and maximum: by default "sites/indiv": the mini and maxi are on the number of sites or individuals in the potential clusters. Other possible values are "area": the minimum and maximum area of the clusters, or "radius": the minimum and maximum radius.
##'
##' @return The list of the following elements:
##' \itemize{
##' \item filtering_post: logical, is there an a posteriori filtering?
##' \item matrix_clusters: the matrix of potential clusters
##' \item centres: the coordinates of the centres of each potential cluster
##' \item radius: the radius of the potential clusters in km if system = WGS84 or in the user units
##' \item areas: the areas of the potential clusters (in the same units as sites_areas).
##' \item sites_coord: coordinates of the sites
##' \item system: system in which the coordinates are expressed
##' \item mini_post: a minimum to filter the significant clusters a posteriori
##' \item maxi_post: a maximum to filter the significant clusters a posteriori
##' \item type_minimaxi_post: type of minimum and maximum a posteriori
##' }
##'
##'
##'
InitScan <- function(mini_post, maxi_post, type_minimaxi_post, sites_areas, sites_coord, system, mini, maxi, type_minimaxi){

  matrix_clusters_provided <- NULL
  area_clusters_provided <- NULL

  if(is.null(mini_post) == FALSE | is.null(maxi_post) == FALSE){

    if(is.null(mini_post) == FALSE){
      if(is.numeric(mini_post)==FALSE){
        stop("mini_post must be numeric")
      }
    }
    if(is.null(maxi_post) == FALSE){
      if(is.numeric(maxi_post)==FALSE){
        stop("maxi_post must be numeric")
      }
    }


    if(!(type_minimaxi_post %in% c("sites/indiv", "area", "radius"))){
      stop("The value of type_minimaxi_post must be sites/indiv or area or radius")
    }

    if(type_minimaxi_post == "area" & is.null(matrix_clusters_provided) == FALSE){
      if(is.null(area_clusters_provided)){
        stop("You must provide area_clusters_provided for the a posteriori filtering")
      }
    }
    if(type_minimaxi_post == "radius" & is.null(matrix_clusters_provided) == FALSE){
      stop("radius a posteriori filtering is only available when matrix_clusters_provided = NULL")
    }
    if(type_minimaxi_post == "area" & is.null(matrix_clusters_provided) == TRUE){
      if(is.null(sites_areas)){
        stop("You must provide sites_areas for the a posteriori filtering")
      }
    }

    # a posteriori filtering:
    filtering_post <- TRUE
  }else{
    filtering_post <- FALSE
  }

  if(is.null(matrix_clusters_provided)){
    if(is.null(sites_coord) | is.null(system) | is.null(mini) | is.null(maxi)){
      stop("You must specify the coordinates of the sites, the system of coordinates used, and the minimum and maximum number of sites in a cluster")
    }else{
      if(! (type_minimaxi %in% c("sites/indiv", "area", "radius")) ){
        stop("The value of type_minimaxi must be sites/indiv or area or radius")
      }
      if(type_minimaxi == "area" & is.null(sites_areas)){
        stop("If type_minimaxi = area you must specify the areas of the sites or individuals")
      }
      if(is.null(sites_areas)==FALSE & length(sites_areas)!=nrow(sites_coord)){
        stop("sites_areas must contain the same number of elements than the number of rows in sites_coord")
      }
      details_clusters <- clusters(sites_coord, system, mini, maxi, type_minimaxi, sites_areas)
      matrix_clusters <- details_clusters$matrix_clusters
      centres <- details_clusters$centres
      radius <- details_clusters$radius
      system <- details_clusters$system
      if(is.null(sites_areas)==FALSE){
        areas <- details_clusters$areas
      }else{
        areas <- NA
      }
      details_clusters <- NULL
    }
  }else{
    matrix_clusters <- matrix_clusters_provided
    if(sum(!(matrix_clusters %in% c(0,1)))>0){
      stop("The matrix of potential clusters must contain only 0 and 1 values")
    }
    if(is.null(area_clusters_provided) == FALSE & length(area_clusters_provided) != nrow(matrix_clusters_provided)){
      stop("Length of area_clusters_provided must be the same as the number of columns of matrix_clusters_provided")
    }
    if(is.null(area_clusters_provided)){
      areas <- NA
    }else{
      areas <- area_clusters_provided
    }
    centres <- NA
    radius <- NA
  }

  return(list(filtering_post = filtering_post, matrix_clusters = matrix_clusters, centres = centres, radius = radius,
         areas = areas, sites_coord = sites_coord, system = system, mini_post = mini_post, maxi_post = maxi_post, type_minimaxi_post = type_minimaxi_post))


}
