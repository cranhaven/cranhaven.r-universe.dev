#' Fast CLARA-like clustering using Jaccard dissimilarity
#'
#' Implements a CLARA (Clustering Large Applications) strategy using Jaccard dissimilarity
#' computed on individual patients state matrices. The algorithm repeatedly samples subsets of the data,
#' performs PAM clustering on each subset, and selects the medoids that minimise the total dissimilarity across
#' the full dataset. Final assignments are made by mapping all data points to the nearest selected medoid.
#'
#' This implementation adapts the original CLARA method described by Kaufman and Rousseeuw (1990)
#' in "Finding Groups in Data: An Introduction to Cluster Analysis".
#'
#' @references
#' Kaufman, L. & Rousseeuw, P. J. (1990). *Finding Groups in Data: An Introduction to Cluster Analysis*. Wiley.
#'
#' @param data A state matrix of censored time-to-event indicators as computed by the \code{make_state_matrix} function.
#' @param k Number of returned clusters.
#' @param samples Number of random samples drawn from the analysed population.
#' @param samplesize Number of patients per sample (default: min(50 + 5k, ncol(data))).
#' @param seed Random seed for reproducibility (default: 123).
#' @param frac Fraction of the population to use for cost computation (default: 1).
#' @importFrom fastkmedoids fastpam
#' @return A list with index of patients from the sample a, medoid indices, cluster assignment, and cost.
#' \describe{
#'   \item{clustering}{An integer vector of cluster assignments for each patient.}
#'   \item{medoids}{Indices of medoids associated witht the lower cost.}
#'   \item{sample}{Indices of the sampled columns used in clustering.}
#'   \item{cost}{Total cost (sum of dissimilarities to assigned medoids).}
#' }
#' @note To improve efficiency, the function used fastpam procedure from the fastkmedoids library and uses optimized Jaccard index computation.
#' For simulation purpose, the \code{frac} parameter can be used to reduce time when computing the cost for each sample. The final cost is given using medoids associated with lower cost computed on fractionned data. A final analysis using the proper CLARA method should be conducted setting \code{frac} to 1.
#' @export
fast_clara_jaccard <- function(data, k, samples = 20, samplesize = NULL,
                               seed = 123 , frac = 1  ) {
  if (inherits(data, "dist"))
    data <- as.matrix(data)
  set.seed(seed)
  n <- ncol(data)
  if (is.null(samplesize)) samplesize <- min(40 + 3 * k, n)

  costs <- vector(length = samples)
  best_cost <- Inf
  best_result <- NULL
s_dist <- list()
  for (s in seq_len(samples)) {
    message("Sample ", s)

    # Step 1: Sample indices
    idx <- sample(seq_len(n), samplesize)
    sample_data <- data[, idx, drop = FALSE]

    # Step 2: Compute Jaccard distance on sample (upper triangle)
    d_sample <- jaccard_index_rcpp_upper(sample_data)
    d_sample[is.na(d_sample)] <- 1
    diag(d_sample) <- 0


    # Step 3: PAM clustering on sample
    pam_res <- fastkmedoids::fastpam(rdist = as.dist(t(d_sample)), n = samplesize, k = k)
    cluster_assign_sample <- pam_res@assignment
    medoids_local_idx <- pam_res@medoids
    medoids_global_idx <- idx[medoids_local_idx]

    # Step 4: Extract medoid data
    medoid_data <- data[, medoids_global_idx, drop = FALSE]

    # Step 5: Full dissimilarity: medoids × full data
    nc <- ncol(data)
    idx <- setdiff( seq_len( nc ) ,  medoids_global_idx )
    nc2 <- round(nc / frac)
    if(frac != 1){
      s2 <- sample( idx , size =  nc2 )
    } else {
      s2 <- seq_len(nc)
    }
    d_full <- jaccard_index_rcpp_parallel( medoid_data , data[,s2] )
    diss_to_medoids <- t(d_full)
    diss_to_medoids[is.na(diss_to_medoids)] <- 1

    # Step 6: Assign each column to closest medoid
    cluster_assign <- max.col(-diss_to_medoids, ties.method = "first")

    # Step 7: Compute cost
    cost_vector <- diss_to_medoids[cbind(seq_len(nc2), cluster_assign)]
    total_cost <- sum(cost_vector)
    costs[s] <- sum(cost_vector)

    # Step 8: Update best result if needed
    if (total_cost < best_cost) {
      best_cost <- total_cost
      best_result <- list(
        sample = idx,
        medoids = medoids_global_idx,
        clustering = cluster_assign,
        cost = total_cost
      )
    }
  }
  # If frac > 1 need to compute the full cost
  # Else nothing
  if(frac > 1 ){
    medoids_global_idx <- best_result$medoids
    # Full dissimilarity for final assignment
    medoid_data <- data[, medoids_global_idx, drop = FALSE]
    d_full <- jaccard_index_rcpp_parallel(medoid_data, data)
    # Assign the highest value if NA
    d_full[is.na(d_full)] <- 1

    # Assign all items (columns) to nearest medoid
    diss_to_medoids <- t(d_full)
    cluster_assign <- max.col(-diss_to_medoids, ties.method = "first")
    cost_vector <- diss_to_medoids[cbind(seq_len(ncol(data)), cluster_assign)]
    total_cost <- sum(cost_vector)

    # Update best_result
    best_result <- list(
      sample = idx,
      medoids = medoids_global_idx,
      clustering = cluster_assign,
      cost = total_cost
    )
  }

  return(best_result)
}
