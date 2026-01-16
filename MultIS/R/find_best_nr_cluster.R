#' Finds the best number of clusters according to silhouette
#'
#' @export
#' @param data The barcode data in a matrix.
#' @param sim A similarity matrix.
#' @param method_reconstruction The clustering method to use.
#' @param method_evaluation The evaluation method to use.
#' @param report Whether the current progress should be reported. Note that
#'               this will not work if parallel is set to TRUE.
#' @param parallel Whether the clustering should be performed in parallel.
#' @param best The method to use to determine the best clustering.
#' @param return_all Whether to return the silhouette score for all
#'                   clusterings.
#' @param ... passed params to evaluating clustering
#' @return The R^2 value for rows is1 and is2 in matrix dat
find_best_nr_cluster <- function(data,
                              sim,
                              method_reconstruction = "kmedoids",
                              method_evaluation = "silhouette",
                              report = FALSE,
                              parallel = FALSE,
                              best = max,
                              return_all = FALSE,
                              ...) {
  if (parallel) {
    dox <- foreach::`%dopar%`
  } else {
    dox <- foreach::`%do%`
  }

  # delete NA values to have a proper clustering and additionally delete out
  # the 0 only rows in readouts
  sim <- sim[, !apply(is.na(sim), MARGIN = 2, all)]
  sim <- sim[!apply(is.na(sim), MARGIN = 1, all), ]
  data <- data[!apply(data == 0, MARGIN = 1, all), ]

  k <- 0
  ks <- dox(foreach::foreach(k = 2:(nrow(sim) - 1)), {
    if (report) {
      print(sprintf("Trying %d of max %d clusters", k, nrow(sim) - 1))
    }
    curr_val <- tryCatch({
        curr_rec <- MultIS::reconstruct(readouts = data,
                                        target_communities = k,
                                        sim = sim,
                                        method = method_reconstruction)
        curr_val <- MultIS::evaluate_clustering(readouts = data,
                                               clustering = curr_rec,
                                               sim = sim,
                                               method = method_evaluation,
                                               ...)
      },
      error = function(x) {
        NA
      }
    )

    return(curr_val)
  })
  names(ks) <- 2:(nrow(sim) - 1)

  ks <- unlist(ks)
  m <- as.integer(names(ks)[ks == best(ks)])

  if (return_all) {
    return(ks)
  } else {
    return(m[1])
  }
}
