#' Diversive magnetic clustering
#'
#' The function \code{dimac} implements the \strong{di}visive
#' \strong{ma}gnetic \strong{c}lustering algorithm to partition fatty acid
#' signatures into clusters.  The DiMaC algorithm was modified from the diana
#' algorithm of the package cluster (Maechler et al. 2016).  \code{dimac} is
#' intended to be called by the user, but only after the fatty acid signatures
#' have been prepared for analysis by calls to the functions
#' \code{\link{prep_fa}} and \code{\link{prep_sig}}.  Consequently, error
#' checking of the arguments associated with the signatures (\code{sigs},
#' \code{id}, \code{type}, and \code{loc}) is necessarily limited, and
#' calling \code{dimac} without preceding calls to \code{\link{prep_fa}}
#' and \code{\link{prep_sig}} could return meaningless results.  Please see
#' Details or the vignette for additional information.
#'
#' @param sigs A numeric matrix of fatty acid signatures in column-major
#'   format.
#' @param id A character vector with a unique sample ID for each signature.
#' @param type A character vector of prey or predator type names.
#' @param loc A numeric matrix specifying the location of signatures within
#'   \code{sig} for each \code{type}.
#' @param dist_meas A integer indicator of the distance measure to use. Default
#'   value 1.
#' @param gamma The power parameter of the chi-square distance measure. Default
#'   value 1.
#'
#' @return A list containing the following elements: \describe{
#'   \item{clust}{A data frame denoting cluster assignments at each iteration
#'   of the algorithm.}
#'   \item{clust_dist}{A numeric matrix of the summed distance within clusters
#'   at each iteration.}
#'   \item{err_code}{An integer error code(0 if no error is detected).}
#'   \item{err_message}{A string containing a brief summary of the results.}
#' }
#'
#' @section Details:
#' The signatures in \code{sigs} are presumed to be ready for analysis, which
#' is best accomplished by a call to the function \code{prep_sig}.  Please
#' refer to the documentation for \code{\link{prep_sig}} and/or the vignette
#' for additional details.
#'
#' The matrix \code{loc} provides a mapping of the location of data for each
#' \code{type} within \code{sig}.  It must contain a row for each \code{type}
#' and two columns, which contain integers designating the first and last
#' signature of each \code{type} within \code{sigs}.  Such a matrix is returned
#' by the function \code{\link{prep_sig}}.
#'
#' Please refer to the documentation for the function
#' \code{\link{dist_between_2_sigs}} for information regarding permissable
#' values for the arguments \code{dist_meas} and \code{gamma}.
#'
#' The DiMaC algorithm is initialized with all signatures in one cluster.  The
#' first two magnets are chosen as the two signatures having the greatest
#' distance between them and each non-magnet signature is placed in the cluster
#' associated with the closest magnet.  The algorithm then enters an iterative
#' phase.  At each iteration, the cluster with the greatest average distance
#' between its signatures and the mean signature is identified as the "active"
#' cluster.  The two signatures within the active cluster having the greatest
#' distance between them are selected as new magnets.  One of the two new
#' magnets replaces the original magnet for the active cluster and the second
#' starts the formation of an additional cluster.  Each non-magnet signature is
#' placed in the cluster associated with the closest magnet, without regard for
#' its cluster designation in the preceding iteration.  Consequently, the
#' algorithm is not simply bifurcating, but rather is much more dynamic and
#' flexible.  The iterations continue until each signature is in its own
#' cluster.
#'
#' Unfortunately, there is no objective method to determine the most appropriate
#' number of clusters for each prey or predator \code{type}.  Our suggestion is
#' to examine the distance results and identify any substantial reductions in
#' distance, which are likely caused by the discovery of structure within that
#' \code{type}, that are followed by a more gradual decrease in distance as the
#' number of clusters increases.  For diet estimation applications, partitioning
#' a prey library into more clusters than the number of fatty acids used to
#' estimate diet may result in estimates that are not unique.  In such a case,
#' estimates of diet composition need to be pooled into a smaller number of
#' "reporting groups" (e.g., Bromaghin 2008; Meynier et al. 2010).
#'
#' Utility functions called by \code{dimac}:
#' \itemize{
#' \item \code{\link{dist_pairs_map}}
#' \item \code{\link{dist_sigs_2_mean}}
#' }
#'
#' @section References:
#' Bromaghin, J.F. 2008. BELS: Backward elimination locus selection for studies
#' of mixture composition or individual assignment. \emph{Molecular Ecology
#' Resources} 8:568-571.
#'
#' Maechler, M., P. Rousseeuw, A. Struyf, M. Hubert, and K. Hornik. 2016.
#' cluster: cluster analysis basics and extensions. R package version 2.0.4.
#'
#' Meynier, L., P.C.H. morel, B.L. Chilvers, D.D.S. Mackenzie, and P. Duignan.
#' 2010. Quantitative fatty acid signature analysis on New Zealand sea lions:
#' model sensitivity and diet estimates. \emph{Journal of Mammalogy}
#' 91:1484-1495.
#'
#' @examples
#' dimac(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                       0.04, 0.11, 0.29, 0.56,
#'                       0.10, 0.05, 0.35, 0.50,
#'                       0.12, 0.03, 0.37, 0.48,
#'                       0.10, 0.06, 0.35, 0.49,
#'                       0.05, 0.15, 0.35, 0.45), ncol=6),
#'       id = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5", "ID_6"),
#'       type = c("Type_1", "Type_2", "Type_3"),
#'       loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'       dist_meas = 1,
#'       gamma = NA)
#' dimac(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                       0.04, 0.11, 0.29, 0.56,
#'                       0.10, 0.05, 0.35, 0.50,
#'                       0.12, 0.03, 0.37, 0.48,
#'                       0.10, 0.06, 0.35, 0.49,
#'                       0.05, 0.15, 0.35, 0.45), ncol=6),
#'       id = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5", "ID_6"),
#'       type = c("Type_1", "Type_2", "Type_3"),
#'       loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'       dist_meas = 2,
#'       gamma = NA)
#' dimac(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                       0.04, 0.11, 0.29, 0.56,
#'                       0.10, 0.05, 0.35, 0.50,
#'                       0.12, 0.03, 0.37, 0.48,
#'                       0.10, 0.06, 0.35, 0.49,
#'                       0.05, 0.15, 0.35, 0.45), ncol=6),
#'       id = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5", "ID_6"),
#'       type = c("Type_1", "Type_2", "Type_3"),
#'       loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2),
#'       dist_meas = 3,
#'       gamma = 0.5)
#' dimac(sigs = matrix(c(0.05, 0.10, 0.30, 0.55,
#'                       0.04, 0.11, 0.29, 0.56,
#'                       0.10, 0.05, 0.35, 0.50,
#'                       0.12, 0.03, 0.37, 0.48,
#'                       0.10, 0.06, 0.35, 0.49,
#'                       0.05, 0.15, 0.35, 0.45), ncol=6),
#'       id = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5", "ID_6"),
#'       type = c("Type_1", "Type_2", "Type_3"),
#'       loc = matrix(c(1, 3, 5, 2, 4, 6), ncol=2))
#'
#' @export
#'
################################################################################


dimac <- function(sigs, id, type, loc, dist_meas=1, gamma=1){


# Error check arguments. -------------------------------------------------------
clust <- NA
clust_dist <- NA


# Check that sigs is a numeric matrix.
if(!(is.numeric(sigs) & is.matrix(sigs))){
  err_code <- 1
  err_message <- "The argument sigs is not a numeric matrix!"

  return(list(clust = clust,
              clust_dist = clust_dist,
              err_code = err_code,
              err_message = err_message))
}


# Check that the signatures are non-negative and their sums do not exceed 1.
# If sig_prep() was called with scale = 2, the signatures will not sum to 1.
if((min(sigs) < 0) | is.na(min(sigs)) | (max(apply(sigs, 2, sum)) > 1)){
  err_code <- 2
  err_message <- "One or more signatures are invalid!"

  return(list(clust = clust,
              clust_dist = clust_dist,
              err_code = err_code,
              err_message = err_message))
}


# Check the value of dist_meas.
if(!(dist_meas %in% 1:3)){
  err_code <- 3
  err_message <- "The argument dist_meas must equal 1, 2, or 3!"

  return(list(clust = clust,
              clust_dist = clust_dist,
              err_code = err_code,
              err_message = err_message))
}


# Check combinations of dist_meas, gamma, and the minimum signature proportion.
dum <- min(sigs)
if(dist_meas < 3){

  # Check value of the minimum signature proportion.
  if(dum <= 0){
    err_code <- 4
    err_message <- paste("If dist_meas equals 1 or 2,",
                         "signature proportions must exceed 0!",
                         sep=" ")

    return(list(clust = clust,
                clust_dist = clust_dist,
                err_code = err_code,
                err_message = err_message))
  }
} else{

  # Check value of the minimum signature proportion.
  if(dum < 0){
    err_code <- 5
    err_message <- "Signature proportions cannot be negative!"

    return(list(clust = clust,
                clust_dist = clust_dist,
                err_code = err_code,
                err_message = err_message))
  }

  # Check value of gamma.
  if(is.na(gamma) | gamma <= 0 | gamma > 1){
    err_code <- 6
    err_message <- paste("If dist_meas equals 3, gamma must exceed 0 and",
                         "cannot exceed 1!",
                         sep=" ")
    return(list(clust = clust,
                clust_dist = clust_dist,
                err_code = err_code,
                err_message = err_message))
  }
}


# Check the number of signatures.
if(ncol(sigs) != loc[nrow(loc),2]){
  err_code <- 7
  err_message <- paste("The number of signatures in sigs does not equal",
                       "the last element of loc!",
                       sep=" ")

  return(list(clust = clust,
              clust_dist = clust_dist,
              err_code = err_code,
              err_message = err_message))
}


# Check lengths of type and loc.
if(length(type) != nrow(loc)){
  err_code <- 8
  err_message <- paste("The number of type names does not equal",
                       "the number of rows in loc!",
                       sep=" ")

  return(list(clust = clust,
              clust_dist = clust_dist,
              err_code = err_code,
              err_message = err_message))
}


# Check length of id and number of signatures.
if(length(id) != ncol(sigs)){
  err_code <- 9
  err_message <- paste("The number of signatures does not equal",
                       "the number of sample ids!",
                       sep=" ")

  return(list(clust = clust,
              clust_dist = clust_dist,
              err_code = err_code,
              err_message = err_message))
}


# Preliminary preparations. ----------------------------------------------------

# Determine number of types and type sample sizes.
n_type <- nrow(loc)
ss <- loc[,2] - loc[,1] + 1


# Allocate memory for the results.
max_clust <- max(ss)
clust_dist <- matrix(data=0, nrow=max_clust, ncol=n_type)
colnames(clust_dist) <- type

clust <- matrix(data=0, nrow=ncol(sigs), ncol=max_clust)
colnames(clust) <- paste("Clust", 1:max_clust, sep="_")
clust[,1] <- 1


# Compute the summed distance between individual signatures and the mean
# signature within each type.
for(li1 in 1:n_type){
  clust_dist[1,li1] <- dist_sigs_2_mean(sigs[,loc[li1,1]:loc[li1,2]],
                                        dist_meas,
                                        gamma)$dist_sum
  }




# Clustering algorithm ---------------------------------------------------------

# Consider each type.
for(li1 in 1:n_type){

  # Check type sample size.
  if(ss[li1] == 1) next

  if(ss[li1] == 2){
    clust[loc[li1,1]:loc[li1,2],2] <- 1:ss[li1]
    next
  }


  # Allocate memory for cluster indicators for this type.
  this_clust <- vector(mode = "numeric", length = ss[li1])


  # Data location for this prey type.
  type_loc <- loc[li1,1]:loc[li1,2]
  rel_loc <- 1:ss[li1]


  # Compute the distance between all pairs of signatures.
  type_sig <- sigs[,type_loc]
  dist_pairs <- dist_pairs_map(type_sig)


  # Find the pair of signatures with the greatest distance and identify them as
  # the initial pair of magnets.
  pair_loc <- which.max(dist_pairs$dist)


  # Configure magnets.
  magnets <- c(dist_pairs$sig_1[pair_loc], dist_pairs$sig_2[pair_loc])
  n_magnets <- 2
  non_magnets <- rel_loc[-magnets]


  # Assign magnets to clusters.
  this_clust[magnets[1]] <- 1
  this_clust[magnets[2]] <- 2


  # Assign non-magnets to these initial clusters.
  for(li2 in 1:length(non_magnets)){

    # Assign to the first magnet.
    this_clust[non_magnets[li2]] <- 1
    min_dist <- dist_pairs$dist[(dist_pairs$sig_1 == non_magnets[li2] |
                                 dist_pairs$sig_1 == magnets[1]) &
                                (dist_pairs$sig_2 == non_magnets[li2] |
                                   dist_pairs$sig_2 == magnets[1])]


    # Consider the remaining magnets.
    for(li3 in 2:n_magnets){
      this_dist <- dist_pairs$dist[(dist_pairs$sig_1 == non_magnets[li2] |
                                    dist_pairs$sig_1 == magnets[li3]) &
                                   (dist_pairs$sig_2 == non_magnets[li2] |
                                    dist_pairs$sig_2 == magnets[li3])]
      if(this_dist < min_dist){
        this_clust[non_magnets[li2]] <- li3
        min_dist <- this_dist
      }
    }
  }


  # Update the cluster matrix.
  clust[type_loc,2] <- this_clust


  # Compute the total distance within clusters.
  clust_dist[2,li1] <- 0
  for(li2 in 1:n_magnets){
    use_sigs <- rel_loc[this_clust==li2]
    clust_dist[2,li1] <- clust_dist[2,li1] +
                         dist_sigs_2_mean(type_sig[,use_sigs], dist_meas,
                                          gamma)$dist_sum
  }


  # Start the clustering loop.
  for(li2 in 3:(ss[li1]-1)){

    # Identify the cluster with the largest mean distance from signatures to
    # their mean as the active cluster.
    active_cluster <- 1
    max_dist <- dist_sigs_2_mean(type_sig[,this_clust==1], dist_meas,
                                 gamma)$dist_mean
    for(li3 in 2:n_magnets){
      this_dist <- dist_sigs_2_mean(type_sig[,this_clust==li3], dist_meas,
                                    gamma)$dist_mean
      if(this_dist > max_dist){
        active_cluster <- li3
        max_dist <- this_dist
      }
    }


    # Find the pair of signatures in the active cluster with maximum distance.
    in_cluster <- rel_loc[this_clust==active_cluster]
    pairs_in_cluster <- (dist_pairs$sig_1 %in% in_cluster) &
                        (dist_pairs$sig_2 %in% in_cluster)
    pair_loc <- which.max(dist_pairs$dist[pairs_in_cluster])
    new_magnet_1 <- dist_pairs$sig_1[pairs_in_cluster][pair_loc]
    new_magnet_2 <- dist_pairs$sig_2[pairs_in_cluster][pair_loc]


    # Reconfigure magnets and non-magnets.
    magnets[active_cluster] <- new_magnet_1
    magnets <- c(magnets, new_magnet_2)
    n_magnets <- n_magnets + 1
    non_magnets <- rel_loc
    non_magnets <- non_magnets[-magnets]


    # Assign magnets to clusters.
    for(li3 in 1:n_magnets){
      this_clust[magnets[li3]] <- li3
    }


    # Assign non-magnets to these initial clusters.
    for(li3 in 1:length(non_magnets)){

      # Assign to the first magnet.
      this_clust[non_magnets[li3]] <- 1
      min_dist <- dist_pairs$dist[(dist_pairs$sig_1 == non_magnets[li3] |
                                   dist_pairs$sig_1 == magnets[1]) &
                                  (dist_pairs$sig_2 == non_magnets[li3] |
                                   dist_pairs$sig_2 == magnets[1])]


      # Consider the remaining magnets.
      for(li4 in 2:n_magnets){
        this_dist <- dist_pairs$dist[(dist_pairs$sig_1 == non_magnets[li3] |
                                      dist_pairs$sig_1 == magnets[li4]) &
                                     (dist_pairs$sig_2 == non_magnets[li3] |
                                      dist_pairs$sig_2 == magnets[li4])]
        if(this_dist < min_dist){
          this_clust[non_magnets[li3]] <- li4
          min_dist <- this_dist
        }
      }
    }


    # Update the partition matrix.
    clust[type_loc,li2] <- this_clust


    # Compute the total distance within clusters.
    clust_dist[li2,li1] <- 0
    for(li3 in 1:n_magnets){
      use_sigs <- rel_loc[this_clust==li3]
      clust_dist[li2,li1] <- clust_dist[li2,li1] +
                             dist_sigs_2_mean(type_sig[,use_sigs], dist_meas,
                                              gamma)$dist_sum
    }
  }


  # The last partition.
  clust[type_loc,ss[li1]] <- rel_loc


  # Prepare for next type.
  rm(this_clust, type_loc, rel_loc, type_sig, dist_pairs, magnets, non_magnets)
}




# Organize results. ------------------------------------------------------------

# Combine types, sample IDs, and partitions.
clust <- data.frame(rep(type, ss), id, clust)
colnames(clust) <- c("Type", "ID", paste("Clust", 1:max_clust, sep="."))

err_code <- 0
err_message <- "Success!"

return(list(clust = clust,
            clust_dist = clust_dist,
            err_code = err_code,
            err_message = err_message))
}
