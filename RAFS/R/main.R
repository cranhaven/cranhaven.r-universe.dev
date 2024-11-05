#' Default (example) feature selection function for RAFS
#'
#' See \code{\link{run_rafs}} for how it is used. Only the train portion of the
#' dataset is to be fed into this function.
#'
#' The function MUST use this \code{train_data} and MAY ignore
#' the \code{train_decision}.
#'
#' If the function depends on randomness, it MUST use the seed parameter to seed
#' the PRNG.
#'
#' The function needs to return a \code{\link{list}} with at least two elements:
#' \code{rel_vars} and \code{rel_vars_rank}, which are vectors and contain,
#' respectively, the indices of variables considered relevant and the rank
#' for each relevant variable. The function MAY return a list with more elements.
#'
#' Other examples of sensible functions are included in the tests of this package.
#'
#' @param train_data input data where columns are variables and rows are observations (all numeric)
#' @param train_decision decision variable as a binary sequence of length equal to number of observations
#' @param seed a numerical seed
#' @return A \code{\link{list}} with at least two fields:
#' \code{rel_vars} and \code{rel_vars_rank}, which are vectors and contain,
#' respectively, the indices of variables considered relevant and the rank
#' for each relevant variable.
#' @export
#' @importFrom MDFS MDFS
default_fs_fun <- function(train_data, train_decision, seed) {
  fs_result <- MDFS(
    train_data,
    train_decision,
    dimensions = 1,
    divisions = 1,
    discretizations = 30,
    seed = seed
  )

  return(list(
    rel_vars = fs_result$relevant.variables,
    rel_vars_rank = order(order(fs_result$p.value[fs_result$relevant.variables]))
  ))
}

#' Default hclust methods
#'
#' As used in \code{\link{run_rafs}} to call \code{\link{hclust}}.
#'
#' @export
default_hclust_methods <- c("ward.D2", "complete", "average", "single")

#' Default feature dissimilarity functions
#'
#' As used in \code{\link{run_rafs}}.
#'
#' The default functions compute:
#' Pearson's correlation (\code{cor}: \code{\link{cor_dist}}),
#' Variation of Information (\code{vi}: \code{\link{vi_dist}}) and
#' Symmetric Target Information Gain (\code{stig}: \code{\link{stig_dist}}).
#'
#' These functions follow a similar protocol to \code{\link{default_fs_fun}}.
#' They expect the same input except for the assumption that the data passed in is relevant.
#' Each of them outputs a matrix of distances (dissimilarities) between features.
#'
#' See also \code{\link{builtin_dist_funs}}.
#'
#' @export
default_dist_funs <- list(
  cor = cor_dist,
  vi = vi_dist,
  stig = stig_dist
)


#' All built-in feature dissimilarity functions
#'
#' To be used in \code{\link{run_rafs}}.
#'
#' See also \code{\link{default_dist_funs}}.
#'
#' @export
builtin_dist_funs <- c(default_dist_funs, list(
  stig_stable = stig_stable_dist,
  stig_from_ig = stig_from_ig_dist
))


#' Compute preliminary feature selection results for RAFS
#'
#' This is a secondary function, useful when experimenting with different
#' feature selection filters and rankings. Its output is used in \code{\link{run_rafs_with_fs_results}}
#' and it is called for the user in \code{\link{run_rafs}}.
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param k number of folds for internal cross validation
#' @param seeds a vector of seeds used for fold generation for internal cross validation
#' @param fs_fun function to compute feature selection p-values, it must have the same signature as \code{\link{default_fs_fun}} (which is the default, see its help to learn more)
#' @return A \code{\link{list}} with feature selection results, e.g. from \code{\link{default_fs_fun}}.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' fs_results <- compute_fs_results(madelon$data, madelon$decision, 2, c(12345))
#' run_rafs_with_fs_results(madelon$data, madelon$decision, fs_results)
#' @export
compute_fs_results <- function(data, decision, k, seeds, fs_fun = default_fs_fun) {
  fs_results <- list()

  for (seed in seeds) {
    folds <- create_seeded_folds(decision, k, seed)

    for (i in 1:k) {
      run_id <- get_run_id(seed, k, i)

      fs_results[[run_id]] <- fs_fun(
        data[-folds[[i]], ],
        decision[-folds[[i]]],
        bitwXor(seed, i)
      )
    }
  }

  attr(fs_results, "k") <- k
  attr(fs_results, "seeds") <- seeds

  class(fs_results) <- "RafsFsResults"

  fs_results
}


#' Robust Aggregative Feature Selection (RAFS)
#'
#' This is the main function of the RAFS library to run for analysis.
#'
#' Depending on your pipeline, you may want to also check out \code{\link{run_rafs_with_fs_results}} and \code{\link{compute_fs_results}}
#' which this function simply wraps over.
#'
#' The results from this function can be fed into one of the helper functions
#' to analyse them further: \code{\link{get_rafs_reps_popcnts}},
#' \code{\link{get_rafs_rep_tuples_popcnts}},
#' \code{\link{get_rafs_rep_tuples_matrix}} and
#' \code{\link{get_rafs_occurrence_matrix}}.
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param k number of folds for internal cross validation
#' @param seeds a vector of seeds used for fold generation for internal cross validation
#' @param fs_fun function to compute feature selection p-values, it must have the same signature as \code{\link{default_fs_fun}} (which is the default, see its help to learn more)
#' @param dist_funs a list of feature dissimilarity functions computed over the relevant portion of the training dataset (see the example \code{\link{default_dist_funs}} and \code{\link{builtin_dist_funs}} to learn more)
#' @param hclust_methods a vector of \code{\link{hclust}} methods to use
#' @return A nested \code{\link{list}} with \code{\link{hclust}} results.
#'   The first level is per the cross validation run.
#'   The second level is per the feature dissimilarity function.
#'   The third (and last) level is per the hclust method.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' @export
run_rafs <- function(data, decision, k = 5, seeds = sample.int(32767, 10), fs_fun = default_fs_fun, dist_funs = default_dist_funs, hclust_methods = default_hclust_methods) {
  fs_results <- compute_fs_results(data, decision, k, seeds, fs_fun = fs_fun)

  run_rafs_with_fs_results(data, decision, fs_results, dist_funs = dist_funs, hclust_methods = hclust_methods)
}


#' Robust Aggregative Feature Selection (RAFS) from feature selection results
#'
#' This is a secondary function, useful when experimenting with different
#' feature selection filters and rankings. The output is exactly the same as
#' from \code{\link{run_rafs}}.
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param fs_results output from \code{\link{compute_fs_results}} computed for the same \code{data} and \code{decision}
#' @param dist_funs a list of feature dissimilarity functions computed over the relevant portion of the training dataset (see the example \code{\link{default_dist_funs}} to learn more)
#' @param hclust_methods a vector of \code{\link{hclust}} methods to use
#' @return A nested \code{\link{list}} with \code{\link{hclust}} results.
#'   The first level is per the cross validation run.
#'   The second level is per the feature dissimilarity function.
#'   The third (and last) level is per the hclust method.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' fs_results <- compute_fs_results(madelon$data, madelon$decision, 2, c(12345))
#' run_rafs_with_fs_results(madelon$data, madelon$decision, fs_results)
#' @importFrom fastcluster hclust
#' @importFrom stats as.dist
#' @export
run_rafs_with_fs_results <- function(data, decision, fs_results, dist_funs = default_dist_funs, hclust_methods = default_hclust_methods) {
  result <- list()

  k <- attr(fs_results, "k", exact = TRUE)
  seeds <- attr(fs_results, "seeds", exact = TRUE)

  for (seed in seeds) {
    folds <- create_seeded_folds(decision, k, seed)

    for (i in 1:k) {
      run_id <- get_run_id(seed, k, i)
      run_result <- list()

      rel_vars <- fs_results[[run_id]]$rel_vars
      if (length(rel_vars) < 2) {
        next
      }
      relevant_train_data <- data[-folds[[i]], rel_vars]
      train_decision <- decision[-folds[[i]]]

      for (dist_fun_name in names(dist_funs)) {
        local_dist_fun <- dist_funs[[dist_fun_name]]
        dist_list <- list()

        tmp_matrix <- local_dist_fun(relevant_train_data = relevant_train_data, train_decision = train_decision, seed = bitwXor(seed, i))

        for (hclust_method in hclust_methods) {
          dist_list[[hclust_method]] <- hclust(as.dist(tmp_matrix), method = hclust_method)
        }

        run_result[[dist_fun_name]] <- dist_list
      }

      result[[run_id]] <- run_result
    }
  }

  attr(result, "fs_results") <- fs_results
  attr(result, "dist_funs_names") <- names(dist_funs)
  attr(result, "hclust_methods") <- hclust_methods

  class(result) <- "RafsResults"

  result
}


#' Get top popularity counts (popcnts) from FS results
#'
#' This function obtains popularity counts (popcnts) of top variables computed
#' over all runs of FS.
#'
#' These results might be fed into further helper functions:
#' \code{\link{get_rafs_top_reps_from_popcnts}} and \code{\link{get_rafs_all_reps_from_popcnts}}.
#'
#' @param fs_results RAFS FS results as obtained from \code{\link{compute_fs_results}}
#' @param n_top_range range of top number to obtain popcnts for
#' @return A nested \code{\link{list}} with popcnts.
#'   The first level is per the number of top variables.
#'   The second (and last) level is popcnts per top variable.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' fs_results <- compute_fs_results(madelon$data, madelon$decision, 2, c(12345))
#' get_rafs_tops_popcnts(fs_results, 2:5)
#' @export
get_rafs_tops_popcnts <- function(
    fs_results, n_top_range = 2:15
) {
  result <- list()

  for (i in n_top_range) {
    result[[paste(i)]] <- get_local_tops_popcnts(
      fs_results,
      n = i)
  }

  result
}


#' Get representatives' popularity counts (popcnts) from RAFS results
#'
#' This function obtains popularity counts (popcnts) of representatives
#' present at each count of clusters (from \code{n_clusters_range}) computed
#' over all runs of RAFS.
#'
#' These results might be fed into further helper functions:
#' \code{\link{get_rafs_top_reps_from_popcnts}} and \code{\link{get_rafs_all_reps_from_popcnts}}.
#'
#' @param rafs_results RAFS results as obtained from \code{\link{run_rafs}}
#' @param n_clusters_range range of clusters number to obtain popcnts for
#' @return A nested \code{\link{list}} with popcnts.
#'   The first level is per the RAFS variant (combination of feature dissimilarity function and hclust method).
#'   The second level is per the number of clusters.
#'   The third (and last) level is popcnts per representative.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' get_rafs_reps_popcnts(rafs_results, 2:5)
#' @export
get_rafs_reps_popcnts <- function(
    rafs_results, n_clusters_range = 2:15
) {
  fs_results <- attr(rafs_results, "fs_results", exact = TRUE)
  dist_funs_names <- attr(rafs_results, "dist_funs_names", exact = TRUE)
  hclust_methods <- attr(rafs_results, "hclust_methods", exact = TRUE)

  rafs_reps_popcnts_per_variant <- list()

  for (dist_fun_name in dist_funs_names) {
    for (hclust_method in hclust_methods) {
      rafs_reps_popcnts_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]] <- list()
    }
  }

  for (i in n_clusters_range) {
    for (dist_fun_name in dist_funs_names) {
      for (hclust_method in hclust_methods) {
        rafs_reps_popcnts_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]][[paste(i)]] <- get_local_reps_popcnts(
          rafs_results, fs_results,
          dist_fun_name = dist_fun_name, hclust_method = hclust_method,
          n = i)
      }
    }
  }

  rafs_reps_popcnts_per_variant
}

#' Get representatives' tuples' popularity counts (popcnts) from RAFS results
#'
#' This function obtains popularity counts (popcnts) of representatives' tuples
#' present at each count of clusters (from \code{n_clusters_range}) computed
#' over all runs of RAFS.
#'
#' @param rafs_results RAFS results as obtained from \code{\link{run_rafs}}
#' @param n_clusters_range range of clusters number to obtain popcnts for
#' @return A nested \code{\link{list}} with popcnts.
#'   The first level is per the RAFS variant (combination of feature dissimilarity function and hclust method).
#'   The second level is per the number of clusters.
#'   The third (and last) level is popcnts per representatives' tuple.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' get_rafs_rep_tuples_popcnts(rafs_results, 2:5)
#' @export
get_rafs_rep_tuples_popcnts <- function(
    rafs_results, n_clusters_range = 2:15
) {
  fs_results <- attr(rafs_results, "fs_results", exact = TRUE)
  dist_funs_names <- attr(rafs_results, "dist_funs_names", exact = TRUE)
  hclust_methods <- attr(rafs_results, "hclust_methods", exact = TRUE)

  rafs_rep_tuples_popcnts_per_variant <- list()

  for (dist_fun_name in dist_funs_names) {
    for (hclust_method in hclust_methods) {
      rafs_rep_tuples_popcnts_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]] <- list()
    }
  }

  for (i in n_clusters_range) {
    for (dist_fun_name in dist_funs_names) {
      for (hclust_method in hclust_methods) {
        rafs_rep_tuples_popcnts_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]][[paste(i)]] <- get_local_rep_tuples_popcnts(
          rafs_results, fs_results,
          dist_fun_name = dist_fun_name, hclust_method = hclust_method,
          n = i)
      }
    }
  }

  rafs_rep_tuples_popcnts_per_variant
}

#' Get representatives' tuples' co-representation matrix from RAFS results
#'
#' This function obtains a matrix of representatives's describing a graph of co-representation
#' at each count of clusters (from \code{n_clusters_range}) computed
#' over all runs of RAFS.
#'
#' If a single result over a cluster number range is desired, the selected matrices can be summed.
#'
#' @param rafs_results RAFS results as obtained from \code{\link{run_rafs}}
#' @param interesting_reps the interesting representatives to build matrices for
#' @param n_clusters_range range of clusters number to obtain matrices for
#' @return A nested \code{\link{list}} with matrices.
#'   The first level is per the RAFS variant (combination of feature dissimilarity function and hclust method).
#'   The second level is per the number of clusters.
#'   The third (and last) level is the co-representation matrix.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' rafs_reps_popcnts <- get_rafs_reps_popcnts(rafs_results, 5)
#' rafs_top_reps <- get_rafs_top_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)
#' get_rafs_rep_tuples_matrix(rafs_results, rafs_top_reps, 5)
#' @export
get_rafs_rep_tuples_matrix <- function(
    rafs_results, interesting_reps, n_clusters_range = 2:15
) {
  fs_results <- attr(rafs_results, "fs_results", exact = TRUE)
  dist_funs_names <- attr(rafs_results, "dist_funs_names", exact = TRUE)
  hclust_methods <- attr(rafs_results, "hclust_methods", exact = TRUE)

  rafs_rep_tuples_matrix_per_variant <- list()

  for (dist_fun_name in dist_funs_names) {
    for (hclust_method in hclust_methods) {
      rafs_rep_tuples_matrix_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]] <- list()
    }
  }

  for (i in n_clusters_range) {
    for (dist_fun_name in dist_funs_names) {
      for (hclust_method in hclust_methods) {
        rafs_rep_tuples_matrix_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]][[paste(i)]] <- get_local_rep_tuples_matrix(
          rafs_results, fs_results, interesting_reps,
          dist_fun_name = dist_fun_name, hclust_method = hclust_method,
          n = i)
      }
    }
  }

  rafs_rep_tuples_matrix_per_variant
}

#' Get co-occurrence matrix from RAFS results
#'
#' This function obtains a matrix describing a graph of co-occurrence
#' at each count of clusters (from \code{n_clusters_range}) computed
#' over all runs of RAFS.
#'
#' If a single result over a cluster number range is desired, the selected matrices can be summed.
#'
#' @param rafs_results RAFS results as obtained from \code{\link{run_rafs}}
#' @param interesting_reps the interesting representatives to build matrices for (in principle, these need not be representatives but it is more common)
#' @param n_clusters_range range of clusters number to obtain matrices for
#' @return A nested \code{\link{list}} with matrices.
#'   The first level is per the RAFS variant (combination of feature dissimilarity function and hclust method).
#'   The second level is per the number of clusters.
#'   The third (and last) level is the co-occurrence matrix.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' rafs_reps_popcnts <- get_rafs_reps_popcnts(rafs_results, 5)
#' rafs_top_reps <- get_rafs_top_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)
#' get_rafs_occurrence_matrix(rafs_results, rafs_top_reps, 5)
#' @export
get_rafs_occurrence_matrix <- function(
    rafs_results, interesting_reps, n_clusters_range = 2:15
) {
  fs_results <- attr(rafs_results, "fs_results", exact = TRUE)
  dist_funs_names <- attr(rafs_results, "dist_funs_names", exact = TRUE)
  hclust_methods <- attr(rafs_results, "hclust_methods", exact = TRUE)

  occurrence_matrix_per_variant <- list()

  for (dist_fun_name in dist_funs_names) {
    for (hclust_method in hclust_methods) {
      occurrence_matrix_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]] <- list()
    }
  }

  for (i in n_clusters_range) {
    for (dist_fun_name in dist_funs_names) {
      for (hclust_method in hclust_methods) {
        occurrence_matrix_per_variant[[paste(dist_fun_name, hclust_method, sep = "_")]][[paste(i)]] <- get_local_occurrence_matrix(
          rafs_results, fs_results, interesting_reps,
          dist_fun_name = dist_fun_name, hclust_method = hclust_method,
          n = i)
      }
    }
  }

  occurrence_matrix_per_variant
}


#' Get top (i.e., most common) representatives from their popcnts
#'
#' This helper function works on results of \code{\link{get_rafs_reps_popcnts}}
#' to obtain the desired number of top (most common) representatives at the chosen number of clusters.
#'
#' @param reps_popcnts popcnts for the chosen variant as obtained from \code{\link{get_rafs_reps_popcnts}}
#' @param n_clusters the desired number of clusters
#' @param n_reps the desired number of top representatives
#' @return A vector of top representatives.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' rafs_reps_popcnts <- get_rafs_reps_popcnts(rafs_results, 5)
#' get_rafs_top_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)
#' @export
get_rafs_top_reps_from_popcnts <- function(reps_popcnts, n_clusters, n_reps = n_clusters) {
  scoped_reps_popcnts <- reps_popcnts[[paste0(n_clusters)]]
  as.numeric(names(scoped_reps_popcnts)[order(unlist(scoped_reps_popcnts), decreasing = TRUE)][seq_len(min(length(scoped_reps_popcnts), n_reps))])
}

#' Get all representatives from their popcnts
#'
#' This helper function works on results of \code{\link{get_rafs_reps_popcnts}}
#' to obtain all representatives at the chosen number of clusters.
#'
#' @param reps_popcnts representatives' popcnts for the chosen variant as obtained from \code{\link{get_rafs_reps_popcnts}}
#' @param n_clusters the desired number of clusters
#' @return A vector of all representatives.
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' rafs_reps_popcnts <- get_rafs_reps_popcnts(rafs_results, 5)
#' get_rafs_all_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)
#' @export
get_rafs_all_reps_from_popcnts <- function(reps_popcnts, n_clusters) {
  scoped_reps_popcnts <- reps_popcnts[[paste0(n_clusters)]]
  as.numeric(names(scoped_reps_popcnts))
}


#' Get top (i.e., most common) representatives's tuples from their popcnts
#'
#' This helper function works on results of \code{\link{get_rafs_rep_tuples_popcnts}}
#' to obtain the desired number of top (most common) representatives' tuples at the chosen number of clusters.
#'
#' @param rep_tuples_popcnts tuples' popcnts for the chosen variant as obtained from \code{\link{get_rafs_rep_tuples_popcnts}}
#' @param n_clusters the desired number of clusters
#' @param n_tuples the desired number of top tuples
#' @return A list of top tuples (each tuple being a vector of representatives).
#' @examples
#' library(MDFS)
#' mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
#' data(madelon)
#' rafs_results <- run_rafs(madelon$data, madelon$decision, 2, c(12345))
#' rafs_rep_tuples_popcnts <- get_rafs_rep_tuples_popcnts(rafs_results, 5)
#' get_rafs_top_rep_tuples_from_popcnts(rafs_rep_tuples_popcnts$stig_single, 5)
#' @export
get_rafs_top_rep_tuples_from_popcnts <- function(rep_tuples_popcnts, n_clusters, n_tuples = 1) {
  scoped_rep_tuples_popcnts <- rep_tuples_popcnts[[paste0(n_clusters)]]
  s <- names(scoped_rep_tuples_popcnts)[order(unlist(scoped_rep_tuples_popcnts), decreasing = TRUE)][1:n_tuples]
  lapply(strsplit(s, "_"), as.numeric)
}
