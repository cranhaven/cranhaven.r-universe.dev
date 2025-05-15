################################################################
##' @title A posteriori filtering on the number of sites/individuals
##'
##' @description This function allows the a posteriori filtering on the number of sites/individuals.
##'
##' @param mini_post numeric. A minimum to filter the significant clusters a posteriori. The default NULL is for no filtering with a a posteriori minimum.
##' @param maxi_post numeric. A maximum to filter the significant clusters a posteriori. The default NULL is for no filtering with a a posteriori maximum.
##' @param nb_sites numeric. The number of sites/individuals.
##' @param index_clusters_temp numeric vector. The indices of the detected clusters.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. A value of 1 indicate that the site (or the individual) is in the cluster, 0 otherwise.
##'
##'
##' @return The detecting clusters with the a posteriori filtering.
##'
##'
post_filt_nb_sites <- function(mini_post, maxi_post, nb_sites, index_clusters_temp, matrix_clusters){

  if(is.null(mini_post)){
    mini_post <- 0
  }
  if(is.null(maxi_post)){
    maxi_post <- nb_sites
  }
  index_clusters <- index_clusters_temp[which(colSums(matrix_clusters[,index_clusters_temp, drop = FALSE])<=maxi_post & colSums(matrix_clusters[,index_clusters_temp, drop = FALSE])>=mini_post)]

  return(index_clusters)

}

################################################################
##' @title A posteriori filtering on the radius
##'
##' @description This function allows the a posteriori filtering on the radius.
##'
##' @param mini_post numeric. A minimum to filter the significant clusters a posteriori. The default NULL is for no filtering with a a posteriori minimum.
##' @param maxi_post numeric. A maximum to filter the significant clusters a posteriori. The default NULL is for no filtering with a a posteriori maximum.
##' @param radius numeric vector. The radius of each cluster.
##' @param index_clusters_temp numeric vector. The indices of the detected clusters.
##'
##'
##' @return The detecting clusters with the a posteriori filtering.
##'
##'
post_filt_radius <- function(mini_post, maxi_post, radius, index_clusters_temp){

  if(is.null(mini_post)){
    mini_post <- 0
  }
  if(is.null(maxi_post)){
    maxi_post <- max(radius)
  }
  index_clusters <- index_clusters_temp[which(radius[index_clusters_temp]<=maxi_post & radius[index_clusters_temp]>=mini_post)]

  return(index_clusters)

}


################################################################
##' @title A posteriori filtering on the area
##'
##' @description This function allows the a posteriori filtering on the area.
##'
##' @param mini_post numeric. A minimum to filter the significant clusters a posteriori. The default NULL is for no filtering with a a posteriori minimum.
##' @param maxi_post numeric. A maximum to filter the significant clusters a posteriori. The default NULL is for no filtering with a a posteriori maximum.
##' @param areas_clusters numeric vector. The areas of the clusters.
##' @param index_clusters_temp numeric vector. The indices of the detected clusters.
##'
##'
##' @return The detecting clusters with the a posteriori filtering.
##'
##'
post_filt_area <- function(mini_post, maxi_post, areas_clusters, index_clusters_temp){

  if(is.null(mini_post)){
    mini_post <- 0
  }
  if(is.null(maxi_post)){
    maxi_post <- max(areas_clusters)
  }

  index_clusters <- index_clusters_temp[which(areas_clusters[index_clusters_temp]<=maxi_post & areas_clusters[index_clusters_temp]>=mini_post)]

  return(index_clusters)

}

