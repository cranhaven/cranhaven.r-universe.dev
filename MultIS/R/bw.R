#' Calculate the bw index
#'
#' @param distance Distance or Dis-Similarity Matrix
#' @param clusters The clustering to evaluate.
#' @param bw_balance The balance [0, 1] between inner cluster similarity
#'                   (Compactness) and the similarity between clusters
#'                   (Separation). A balance value < 1 increases the importance
#'                   of Compactness, whereas a value > 1 increases the
#'                   importance of Separation.
#' @param ind_cluster If true, the bw value for all individual clusters is
#'                    returned.
#' @return A score that describes how well the clustering fits the data.
bw <- function(distance, clusters, bw_balance = 1.0, ind_cluster = FALSE) {
  cluster_ids <- unique(clusters)

  s <- sapply(seq_len(nrow(distance)), function(i) {
    ic <- clusters == clusters[i]
    ic[i] <- FALSE
    ic <- which(ic) # innerCluster Elements

    oc <- seq_len(nrow(distance))
    oc <- oc[!(oc %in% c(i, ic))]

    if (length(ic) == 0) {
      return(0)
    }

    a <- mean(distance[i, ic])

    b <- min(sapply(cluster_ids[!(cluster_ids %in% clusters[i])], function(e) {
      mean(distance[i, clusters == e])
    }))

    return((b - bw_balance * a) / max(bw_balance * a, b))
  })

  ifelse(ind_cluster,
    return(sapply(cluster_ids, function(e) {
      mean(s[clusters == e])
    })),
    return(mean(s))
  )
}
