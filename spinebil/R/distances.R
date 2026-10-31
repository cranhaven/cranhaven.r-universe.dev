#' Collecting all pairwise distances between input planes.
#'
#' The distribution of all pairwise distances is useful to understand
#' the optimisation in a guided tour, to compare e.g. different optimisation
#' methods or different number of noise dimensions.
#'
#' @param planes Input planes (e.g. result of guided tour)
#' @param nn Set true to only consider nearest neighbour distances (dummy, not yet implemented)
#' @return numeric vector containing all distances
#' @export
#' @examples
#' planes1 <- purrr::rerun(10, tourr::basis_random(5))
#' planes2 <- purrr::rerun(10, tourr::basis_random(10))
#' d1 <- distance_dist(planes1)
#' d2 <- distance_dist(planes2)
#' d <- tibble::tibble(dist=c(d1, d2), dim=c(rep(5,length(d1)),rep(10,length(d2))))
#' ggplot2::ggplot(d) + ggplot2::geom_boxplot(ggplot2::aes(factor(dim), dist))
distance_dist <- function(planes, nn=FALSE){
  #nn could be used to turn on only distance to nearest neighbour?
  planes <- as.list(planes)
  maxI <- length(planes)
  distL <- numeric(choose(maxI, 2))
  i <- 1
  idx <- 1
  while(i<maxI+1){
    j <- i+1
    while(j<maxI+1){
      cDist <- tourr::proj_dist(planes[[i]], planes[[j]])
      distL[idx] <- cDist
      j <- j+1
      idx <- idx+1
    }
    i <- i+1
  }
  return(distL)
}

#' Collecting distances between input planes and input special plane.
#'
#' If the optimal view is known, we can use the distance between a given plane
#' and the optimal one as a proxy to diagnose the performance of the guided tour.
#'
#' @param planes Input planes (e.g. result of guided tour)
#' @param special_plane Plane defining the optimal view
#' @return numeric vector containing all distances
#' @export
#' @examples
#' planes <- purrr::rerun(10, tourr::basis_random(5))
#' special_plane <- basis_matrix(1,2,5)
#' d <- distance_to_sp(planes, special_plane)
#' plot(d)
distance_to_sp <- function(planes, special_plane){
  maxI <- length(planes)
  distL <- numeric(maxI)
  i <- 1
  while(i<maxI+1){
    cDist <- tourr::proj_dist(planes[[i]], special_plane)
    distL[i] <- cDist
    i <- i+1
  }
  return(distL)
}

#' Estimating squint angle of 2-d structure in high-d dataset under selected index.
#'
#' We estimate the squint angle by interpolating from a random starting plane
#' towards the optimal view until the index value of the selected index function
#' is above the selected cutoff. Since this depends on the direction, this is repeated
#' with n randomly selected planes giving a distribution representative of the squint angle.
#'
#' @param data Input data
#' @param indexF Index function
#' @param cutoff Threshold index value above which we assume the structure to be visible
#' @param structure_plane Plane defining the optimal view
#' @param n Number of random starting planes (default = 100)
#' @param step_size Interpolation step size fixing the accuracy (default = 0.01)
#' @return numeric vector containing all squint angle estimates
#' @export
#' @examples
#' data <- spiral_data(50, 4)
#' indexF <- scag_index("stringy")
#' cutoff <- 0.7
#' structure_plane <- basis_matrix(3,4,4)
#' squint_angle_estimate(data, indexF, cutoff, structure_plane, n=1)
squint_angle_estimate <- function(data, indexF, cutoff, structure_plane, n = 100, step_size = 0.01){
  data <- as.matrix(data) # make sure data is in matrix format
  angles <- numeric(length = n) # collecting all individual estimates
  i <- 1
  p <- ncol(data)
  while(i <= n){
    # first generate random direction
    # make sure it is not too close to structure plane
    dist <- 0.
    while(dist < 0.1){
      rBasis <- tourr::basis_random(p)
      dist <- tourr::proj_dist(rBasis, structure_plane)
    }
    # now interpolate from rBasis to structure plane with selected step size
    # since planned tour ignores first two entries
    # we first generate some random planes to be ignored
    notUsed1 <- tourr::basis_random(p)
    notUsed2 <- tourr::basis_random(p)
    tourHist <- tourr::save_history(data,
                                    tour_path =
                                      tourr::planned_tour(
                                        list(notUsed1,
                                             notUsed2,
                                             rBasis,
                                             structure_plane)))
    allBases <- as.list(tourr::interpolate(tourHist, angle = step_size))
    cIndex <- 0.
    j <- 1
    while(cIndex < cutoff && j <= length(allBases)) {
      cProj <- data %*% allBases[[j]]
      cIndex <- indexF(cProj)
      j <- j + 1
    }
    
    if (j > length(allBases)) {
      warning("Did not reach cutoff; using final frame.")
      j <- length(allBases)
    }
    
    cDist <- tourr::proj_dist(allBases[[j]], structure_plane)
    angles[i] <- cDist
    i <- i+1
  }
  return(angles)
}
