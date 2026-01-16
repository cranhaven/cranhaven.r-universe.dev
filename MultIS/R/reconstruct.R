#' Apply a clustering algorithm recursively to a given time course.
#'
#' @export
#' @param readouts The time course for which to find clusters.
#' @param method Either "kmedoids", "kmeans" or any string permitted as a
#'               method for stats::hclust.
#' @param sim A similarity matrix used with all methods except "kmeans".
#' @param split_similarity Similarity Threshold. If any two elements within a
#'                        cluster are below this threshold, another split is
#'                        initiated.
#' @param combine_similarity After Splitting, a combination phase is activated.
#'                          If any two elements between two clusters have a
#'                          similarity higher than this threshold, the cluster
#'                          are combined.
#' @param use_silhouette If TRUE, silhouette is used to define number of cluster
#'                      during splitting, otherwise cluster are always split
#'                      into two new clusters.
#' @param cluster_obj If TRUE, a clusterObject with the readouts, similarity
#'                   and clustering is returned.
#' @return A matrix with two columns: "Clone" and "IS" or if
#'         cluster_obj = TRUE a cluster object, which can be used to plot the
#'         clustering.
reconstruct_recursive <- function(readouts,
                                  method = "kmedoids",
                                  sim = MultIS::get_similarity_matrix(
                                    readouts = readouts,
                                    upper = TRUE
                                  ),
                                  split_similarity = .7,
                                  combine_similarity = .9,
                                  use_silhouette = TRUE,
                                  cluster_obj = FALSE) {


  # delete NA values to have a proper clustering and additionally delete out
  # the 0 only rows in readouts
  sim <- sim[, !apply(is.na(sim), MARGIN = 2, all)]
  sim <- sim[!apply(is.na(sim), MARGIN = 1, all), ]
  readouts <- readouts[!apply(readouts == 0, MARGIN = 1, all), ]

  # start with one cluster
  clusters <- list(colnames(sim))
  min_similarities <- min(sim) # minimal similarity in cluster

  # recursively split until all clusters consists of a
  # minimal inner cluster similiarity > split_similarity
  while (any(min_similarities < split_similarity)) {
    i <- which(min_similarities < split_similarity)[1]
    cluster <- clusters[[i]]
    clusters[[i]] <- NULL

    sm_cluster <- sim[cluster, cluster]
    data_cluster <- readouts[cluster, ]

    if (length(cluster) == 2) { # direct split
      mapping <- matrix(
        c("1", "2", cluster),
        nrow = 2,
        dimnames = list(NULL, c("Clone", "IS"))
      )
    } else {
      nr_cluster <- 2
      if (use_silhouette) {
        best_nr_cluster <- MultIS::find_best_nr_cluster(
          data = data_cluster,
          sim = sm_cluster,
          method_reconstruction = method,
          returnAll = TRUE
        )
        nr_cluster <- as.integer(names(which.max(best_nr_cluster)))
      }

      mapping <- MultIS::reconstruct(
        readouts = data_cluster,
        sim = sm_cluster,
        method = method,
        target_communities = nr_cluster,
        cluster_obj = TRUE
      )$mapping
    }

    clusters <- c(clusters, lapply(
      unique(mapping[, "Clone"]), function(clone) {
        mapping[mapping[, "Clone"] == clone, "IS"]
      }
    ))

    # read out the min similarities
    min_similarities <- unlist(lapply(clusters, function(cluster) {
      min(sim[cluster, cluster])
    }))
  }

  mapping <- as.matrix(do.call("rbind", lapply(
    seq_len(length(clusters)), function(i) {
      data.frame(as.character(i), clusters[[i]])
    }
  )))

  colnames(mapping) <- c("Clone", "IS")
  rownames(mapping) <- mapping[, "IS"]

  mapping <- mapping[rownames(readouts), ]
  rownames(mapping) <- NULL

  # calculate the means for all cluster and put
  # together the ones with a similarity > combine_similarity
  clusters <- do.call("rbind", lapply(
    as.character(sort(as.numeric(unique(mapping[, "Clone"])))),
    function(cluster) {
      colMeans(
        readouts[
          mapping[mapping[, "Clone"] == cluster, "IS"], ,
          drop = FALSE
        ]
      )
    }
  ))
  rownames(clusters) <- seq_len(nrow(clusters))
  sm <- get_similarity_matrix(readouts = clusters) > combine_similarity

  final_clusters <- list()
  for (i in seq_len(ncol(sm))) {
    # check whether i or any of elements is already in a cluster
    ci <- unlist(lapply(final_clusters, function(e) {
      any(which(sm[, i]) %in% e)
    }))
    if (is.null(ci) | all(ci == FALSE)) {
      ci <- length(final_clusters) + 1
      final_clusters[[ci]] <- as.integer(which(sm[, i]))
      next
    }
    ci <- which(ci)[1]

    final_clusters[[ci]] <- sort(unique(c(
      final_clusters[[ci]],
      as.integer(which(sm[, i]))
    )))
  }
  length_fc <- Inf
  while (length(final_clusters) != length_fc) {
    length_fc <- length(final_clusters)

    for (i in seq_len(length(final_clusters))) {
      combines <- unlist(lapply(final_clusters, function(e) {
        any(final_clusters[[i]] %in% e)
      }))
      if (sum(combines) < 2) next
      final_clusters[[length(final_clusters) + 1]] <-
        sort(unique(unlist(final_clusters[which(combines)])))
      final_clusters <- final_clusters[-which(combines)]
      break
    }
  }

  mapping_tmp <- mapping
  for (i in seq_len(length(final_clusters))) {
    mapping[mapping_tmp[, "Clone"] %in%
      as.character(final_clusters[[i]]), "Clone"] <- as.character(i)
  }

  if (cluster_obj) {
    cluster_obj <- list(
      readouts = readouts,
      sim = sim,
      mapping = mapping
    )
    class(cluster_obj) <- c(class(cluster_obj), "clusterObj")

    return(cluster_obj)
  }

  return(mapping)
}


#' Apply a clustering algorithm to a given time course.
#'
#' @export
#' @param readouts The time course for which to find clusters.
#' @param target_communities The number of clusters to cluster for.
#' @param method Either "kmedoids", "kmeans" or any string permitted as a
#'               method for stats::hclust.
#' @param sim A similarity matrix used with all methods except "kmeans".
#' @param cluster_obj If TRUE, a clusterObject with the readouts, similarity
#'                    and clustering is returned.
#' @return A matrix with two columns: "Clone" and "IS" or if
#'         cluster_obj = TRUE a cluster object, which can be used to plot the
#'         clustering.
reconstruct <- function(readouts,
                        target_communities,
                        method = "kmedoids",
                        sim = MultIS::get_similarity_matrix(
                          readouts = readouts,
                          upper = TRUE
                        ),
                        cluster_obj = FALSE) {


  # delete NA values to have a proper clustering and additionally delete
  # out the 0 only rows in readouts
  sim <- sim[, !apply(is.na(sim), MARGIN = 2, all)]
  sim <- sim[!apply(is.na(sim), MARGIN = 1, all), ]
  readouts <- readouts[!apply(readouts == 0, MARGIN = 1, all), ]

  diag(sim) <- 0

  if (method == "kmedoids") {
    # TODO: inline reconstructKmedoid here
    ret <- reconstruct_kmedoid(
      readouts = readouts,
      target_communities = target_communities,
      sim = sim
    )
  } else if (method == "kmeans") {
    clus <- stats::kmeans(x = readouts, centers = target_communities)$cluster
  } else {
    d <- stats::as.dist(max(sim) - sim, diag = TRUE, upper = TRUE)
    hc <- stats::hclust(d = d, method = method)
    clus <- stats::cutree(hc, k = target_communities)
  }

  if (method != "kmedoids") {
    ret <- matrix(
      c(clus, names(clus)),
      nrow = length(clus),
      ncol = 2,
      dimnames = list(c(), c("Clone", "IS"))
    )
  }

  if (cluster_obj) {
    cluster_obj <- list(
      readouts = readouts,
      sim = sim,
      mapping = ret
    )
    class(cluster_obj) <- c(class(cluster_obj), "clusterObj")

    return(cluster_obj)
  }

  return(ret)
}

#' Calculate the k-medoids clustering for a given time course.
#'
#' @param readouts The time course for which to find clusters.
#' @param target_communities The number of clusters to cluster for.
#' @param sim A similarity matrix for the time course.
#' @return A matrix with two columns: "Clone" and "IS".
reconstruct_kmedoid <- function(readouts,
                                target_communities,
                                sim = MultIS::get_similarity_matrix(
                                  readouts = readouts,
                                  self = 0,
                                  upper = TRUE
                                )) {
  m <- sim
  m <- m / max(m, na.rm = TRUE)
  if (sum(m, na.rm = TRUE) <= 0) {
    return(NULL)
  }
  diag(m) <- 1

  m <- stats::as.dist(1 - m)

  clus <- cluster::pam(m, target_communities)

  ret <- matrix(
    c(clus$clustering, rownames(readouts)),
    nrow = length(clus$clustering),
    ncol = 2,
    dimnames = list(c(), c("Clone", "IS"))
  )

  return(ret)
}
