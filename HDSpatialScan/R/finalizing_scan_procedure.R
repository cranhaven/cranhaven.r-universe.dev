################################################################
##' @title Finalization of the scan procedures
##'
##' @description This function finalizes the scan procedures.
##'
##' @param index_clusters_temp numeric vector. Indices of the significant clusters.
##' @param index numeric vector. Index of concentration for each potential cluster.
##' @param filtering_post logical. Is there an a posteriori filtering?
##' @param type_minimaxi_post character. Type of minimum and maximum a posteriori: by default "sites/indiv": the mini_post and maxi_post are on the number of sites or individuals in the significant clusters. Other possible values are "area": the minimum and maximum area of the clusters, or "radius": the minimum and maximum radius.
##' @param mini_post numeric. A minimum to filter the significant clusters a posteriori (see type_minimaxi_post). The default NULL is for no filtering with a a posteriori minimum.
##' @param maxi_post numeric. A maximum to filter the significant clusters a posteriori (see type_minimaxi_post). The default NULL is for no filtering with a a posteriori maximum.
##' @param nb_sites numeric. The number of considered sites or individuals.
##' @param matrix_clusters matrix. The matrix of potential clusters taking the value 1 at lign i and column j if the cluster j contains the site i, 0 otherwise.
##' @param radius numeric vector. The radius of the potential clusters.
##' @param areas numeric vector. The areas of the potential clusters.
##' @param centres numeric matrix. The coordinates of the centres of each potential cluster.
##' @param pvals numeric vector. The pvalue of each potential cluster.
##' @param maximize logical. Should the index be maximized? By default TRUE. If FALSE it will be minimized.
##'
##' @return The list of the following elements:
##' \itemize{
##' \item pval_clusters: pvalues of the selected clusters.
##' \item sites_clusters: the indices of the sites of the selected clusters.
##' \item centres_clusters: the coordinates of the centres of each selected cluster.
##' \item radius_clusters: the radius of the selected clusters.
##' \item areas_clusters: the areas of the selected clusters.
##' }
##'
##'
FinScan <- function(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals, maximize = TRUE){

  if(length(index_clusters_temp)>0){
    if(maximize == TRUE){
      ordre <- order(index[index_clusters_temp], decreasing = TRUE)
    }else{
      ordre <- order(index[index_clusters_temp], decreasing = FALSE)
    }
    index_clusters_temp <- index_clusters_temp[ordre]

    # a posteriori filtering
    if(filtering_post == TRUE){
      if(type_minimaxi_post == "sites/indiv"){
        index_clusters <- post_filt_nb_sites(mini_post, maxi_post, nb_sites, index_clusters_temp, matrix_clusters)
      }
      if(type_minimaxi_post == "radius"){
        index_clusters <- post_filt_radius(mini_post, maxi_post, radius, index_clusters_temp)
      }
      if(type_minimaxi_post == "area"){
        index_clusters <- post_filt_area(mini_post, maxi_post, areas, index_clusters_temp)

      }
    }else{
      index_clusters <- index_clusters_temp
    }
  }else{
    index_clusters <- index_clusters_temp
  }

  # non overlapping clusters:
  final_index <- non_overlap(index_clusters, matrix_clusters)

  pval_clusters <- pvals[final_index]
  sites_clusters <- lapply(final_index, function(j) which(matrix_clusters[,j]==1))

  if(sum(is.na(centres)) == 0){
    centres_clusters <- centres[final_index,,drop = FALSE]
  }else{
    centres_clusters <- NA
  }
  if(sum(is.na(radius)) == 0){
    radius_clusters <- radius[final_index]
  }else{
    radius_clusters <- NA
  }
  if(sum(is.na(areas)) == 0){
    areas_clusters <- areas[final_index]
  }else{
    areas_clusters <- NA
  }

  return(list(pval_clusters = pval_clusters, sites_clusters = sites_clusters,
              centres_clusters = centres_clusters, radius_clusters = radius_clusters,
              areas_clusters = areas_clusters))
}
