#' Evaluate a clustering using the given method
#'
#' @export
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @param method The method to evaluate the given clustering. This might be one
#'               of "silhouette", "sdindex", "ptbiserial", "dunn", "bw", or
#'               "custom'.
#' @param custom_eval A custom function to be run for evaluating a clustering.
#'                    Only used with method "custom".
#' @param ... Further arguments that are passed to a custom function.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering <- function(readouts,
                               clustering,
                               sim,
                               method,
                               custom_eval = NULL,
                               ...) {
  methods <- c("silhouette", "sdindex", "ptbiserial", "dunn", "bw", "custom")

  if (!is.character(method)) {
    stop("The method must be given as a string.")
  }
  if (!method %in% methods) {
    stop(paste0("The method must be one of: ", paste(methods, collapse = ", ")))
  }

  ret <- switch(method,
    "silhouette" = evaluate_clustering_silhouette(readouts, clustering, sim),
    "sdindex"    = evaluate_clustering_sdindex(readouts, clustering, sim),
    "ptbiserial" = evaluate_clustering_ptbiserial(readouts, clustering, sim),
    "dunn"       = evaluate_clustering_dunn(readouts, clustering, sim),
    "bw"         = evaluate_clustering_bw(readouts, clustering, sim, ...),
    "custom"     = evaluate_clustering_custom(readouts, clustering, sim,
                                            custom_eval, ...)
  )

  return(ret)
}

#' Evaluate a clustering using the silhouette index
#'
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering_silhouette <- function(readouts, clustering, sim) {
  dsim <- max(sim, na.rm = TRUE) - sim
  sil <- cluster::silhouette(as.integer(clustering[, "Clone"]), dsim)
  ssil <- summary(sil)
  return(ssil$avg.width)
}

#' Evaluate a clustering using the SD-index
#'
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering_sdindex <- function(readouts, clustering, sim) {
  # Code from BCA package
  sd_clv <- function(x, clus, alpha) {
    if (!is.data.frame(x)) x <- as.data.frame(x)
    scatt <- clv::clv.Scatt(x, clus)
    dis <- clv::clv.Dis(scatt$cluster.center)
    sd <- clv::clv.SD(scatt$Scatt, dis, alfa = alpha)
    return(sd)
  }
  cl <- as.integer(clustering[, "Clone"])
  score <- sd_clv(x = readouts, clus = cl, alpha = clv::clv.Dis(readouts))

  return(score)
}

#' Evaluate a clustering using the point-biserial index
#'
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering_ptbiserial <- function(readouts, clustering, sim) {
  clones <- as.integer(clustering[, "Clone"])
  dsim <- max(sim, na.rm = TRUE) - sim

  combs <- t(utils::combn(x = seq_len(nrow(readouts)), m = 2))
  x <- apply(combs, MARGIN = 1, function(x) {
    dsim[x[1], x[2]]
  })
  y <- apply(combs, MARGIN = 1, function(x) {
    as.integer(clones[x[1]] != clones[x[2]])
  })

  score <- ltm::biserial.cor(x = x, y = y, level = 2)

  return(score)
}

#' Evaluate a clustering using the dunn index
#'
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering_dunn <- function(readouts, clustering, sim) {
  clones <- as.integer(clustering[, "Clone"])
  dsim <- max(sim, na.rm = TRUE) - sim

  score <- clValid::dunn(distance = dsim, clusters = clones)

  return(score)
}

#' Evaluate a clustering using the bw index
#'
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @param ... Further arguments that are passed to the bw function.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering_bw <- function(readouts, clustering, sim, ...) {
  clones <- as.integer(clustering[, "Clone"])
  dsim <- max(sim, na.rm = TRUE) - sim

  score <- bw(distance = dsim, clusters = clones, ...)

  return(score)
}

#' Evaluate a clustering using a custom evaluation function
#'
#' @param readouts The readouts the clustering and similarity matrix are based
#'                 on.
#' @param clustering The clustering to evaluate.
#' @param sim The similarity matrix, this clustering is based on.
#' @param custom_eval The custom function to be run for evaluating a clustering.
#' @param ... Further arguments that are passed to the custom function.
#' @return A score that describes how well the clustering fits the data.
evaluate_clustering_custom <- function(readouts,
                                       clustering,
                                       sim,
                                       custom_eval,
                                       ...) {
  return(custom_eval(readouts, clustering, sim, ...))
}
