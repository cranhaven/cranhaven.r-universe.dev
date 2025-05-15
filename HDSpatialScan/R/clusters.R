############################################################################################################################################################################################
##' @title Creation of the matrix of potential clusters
##'
##' @description This function creates the matrix in which each column corresponds to a potential clusters, taking the value 1 when a site (or an individual) is in the potential cluster and 0 otherwise.
##'
##' @param sites_coord numeric matrix. Matrix of the coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates). It has the same number of rows as the number of sites or individuals and 2 columns.
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param mini numeric. Minimum for the clusters (see type_minimaxi).
##' @param maxi numeric. Maximum for the clusters (see type_minimaxi).
##' @param type_minimaxi character. Type of minimum and maximum: "area": the minimum and maximum area of the clusters, "radius": the minimum and maximum radius, or "sites/indiv": the minimum and maximum number of sites or individuals in the clusters.
##' @param sites_areas numeric vector. Areas of the sites. It must contain the same number of elements than the rows of sites_coord. If the data is on individuals and not on sites, there can be duplicated values. By default: NULL
##'
##'
##' @return The list of the following elements:
##' \itemize{
##' \item matrix_clusters: numeric matrix of 0 and 1
##' \item centres: the coordinates of the centres of each cluster (numeric matrix)
##' \item radius: the radius of the clusters in km if system = "WGS84" or in the coordinates unit otherwise (numeric vector)
##' \item areas: the areas of the clusters (in same units as in sites_areas). Provided only if sites_areas is not NULL. Numeric vector
##' \item system: the system of coordinates (character)
##' }
##'
##'
clusters <- function(sites_coord, system, mini, maxi, type_minimaxi, sites_areas){
  if(is.null(system)){
    stop("Specify a correct system: Euclidean or WGS84")
  }
  if(length(system)!=1){
    stop("Only one system must be specified")
  }
  if(system != "Euclidean" & system != "WGS84"){
    stop("Specify a correct system: Euclidean or WGS84")
  }
  if(is(sites_coord, "matrix") == FALSE){
    stop("sites_coord must be a matrix with two columns")
  }
  if(ncol(sites_coord)!=2){
    stop("sites_coord must be a matrix with two columns")
  }
  if(system == "Euclidean"){
    dists <- spDists(sites_coord, longlat = FALSE)
  }else{
    dists <- spDists(sites_coord, longlat = TRUE)
  }
  if(is.numeric(mini)==FALSE | is.numeric(maxi)==FALSE){
    stop("mini and maxi must be numeric")
  }

  if(mini>maxi){
    stop("mini must be smaller than maxi")
  }
  if(maxi <= 0){
    stop("maxi must be strictly positive")
  }

  indices <- which(duplicated(sites_coord, MARGIN = 1) == FALSE)
  sites_coord_unique <- sites_coord[indices,, drop = FALSE]
  dists_unique <- dists[indices,, drop = FALSE]

  dist_sites <- list()
  for(i in 1:nrow(sites_coord_unique)){
    dist_sites[[i]] <- unique(dists_unique[i,order(dists_unique[i,])])
  }

  nb_clusters <- length(unlist(dist_sites))
  matrix_clusters <- matrix(ncol = nb_clusters, nrow = nrow(sites_coord))
  centres <- matrix(nrow = nb_clusters, ncol = 2)
  radius <- numeric(nb_clusters)
  if(is.null(sites_areas)==FALSE){
    areas <- numeric(nb_clusters)
  }


  column <- 1

  for(i in 1:nrow(sites_coord_unique)){
    for(j in dist_sites[[i]]){
      selection_cluster <- which(dists_unique[i,]<=j)
      if(type_minimaxi == "sites/indiv"){
        if(length(selection_cluster)<=maxi & length(selection_cluster) >= mini){
          matrix_clusters[selection_cluster, column] <- 1
          matrix_clusters[-selection_cluster, column] <- 0
          centres[column,] <- sites_coord_unique[i,]
          radius[column] <- j
          if(is.null(sites_areas)==FALSE){
            # selection_cluster is the individuals
            # we select the indices of the non duplicated sites
            non_dupl_sites <- selection_cluster[duplicated(sites_coord[selection_cluster,, drop = FALSE], MARGIN = 1)==FALSE]
            tot_area <- sum(sites_areas[non_dupl_sites])
            areas[column] <- tot_area
          }
          column <- column + 1
        }
      }
      if(type_minimaxi == "radius"){
        if(j<=maxi & j >= mini){
          matrix_clusters[selection_cluster, column] <- 1
          matrix_clusters[-selection_cluster, column] <- 0
          centres[column,] <- sites_coord_unique[i,]
          radius[column] <- j
          if(is.null(sites_areas)==FALSE){
            # selection_cluster is the individuals
            # we select the indices of the non duplicated sites
            non_dupl_sites <- selection_cluster[duplicated(sites_coord[selection_cluster,, drop = FALSE], MARGIN = 1)==FALSE]
            tot_area <- sum(sites_areas[non_dupl_sites])
            areas[column] <- tot_area
          }
          column <- column + 1
        }
      }
      if(type_minimaxi == "area"){
        # selection_cluster is the individuals
        # we select the indices of the non duplicated sites
        non_dupl_sites <- selection_cluster[duplicated(sites_coord[selection_cluster,, drop = FALSE], MARGIN = 1)==FALSE]
        tot_area <- sum(sites_areas[non_dupl_sites])
        if(tot_area<=maxi & tot_area >= mini){
          matrix_clusters[selection_cluster, column] <- 1
          matrix_clusters[-selection_cluster, column] <- 0
          centres[column,] <- sites_coord_unique[i,]
          radius[column] <- j
          areas[column] <- tot_area
          column <- column + 1
        }
      }

    }
  }

  a_suppr <- which(is.na(colSums(matrix_clusters)))
  if(length(a_suppr)>0){
    matrix_clusters <- matrix_clusters[,-a_suppr,drop = FALSE]
    centres <- centres[-a_suppr,,drop = FALSE]
    radius <- radius[-a_suppr]

    if(is.null(sites_areas)==FALSE){
      areas <- areas[-a_suppr]
    }
  }

  if(length(radius)>=2){

    to_keep <- (duplicated(matrix_clusters, MARGIN = 2) == FALSE)

    matrix_clusters <- matrix_clusters[,to_keep, drop = FALSE]
    centres <- centres[to_keep,, drop = FALSE]
    radius <- radius[to_keep]
    if(is.null(sites_areas)==FALSE){
      areas <- areas[to_keep]
    }
  }else{
    stop("Change the values of mini and maxi to have at least two potential clusters")
  }

  if(length(radius)<=1){
    stop("Change the values of mini and maxi to have at least two potential clusters")
  }


  if(is.null(sites_areas)==FALSE){
    return(list(matrix_clusters = matrix_clusters, centres = centres, radius = radius, areas = areas, system = system))
  }else{
    return(list(matrix_clusters = matrix_clusters, centres = centres, radius = radius, system = system))
  }

}




