# n - number of clusters
get_local_tops_popcnts <- function(fs_results, n) {
  run_ids <- names(fs_results)

  result <- list()

  for (run_id in run_ids) {
    rel_vars <- fs_results[[run_id]]$rel_vars
    rel_vars_rank <- fs_results[[run_id]]$rel_vars_rank

    # restore global indices
    local_reps <- rel_vars[order(rel_vars_rank)[1:n]]

    for (rep_idx in local_reps) {
      rep <- paste0(rep_idx)
      if (rep %in% names(result)) {
        result[[rep]] <- result[[rep]] + 1
      } else {
        result[[rep]] <- 1
      }
    }
  }

  result
}


# n - number of clusters
get_local_reps_popcnts <- function(rafs_results, fs_results, dist_fun_name, hclust_method, n) {
  run_ids <- names(rafs_results)

  reps_popcnts <- list()

  for (run_id in run_ids) {
    rel_vars <- fs_results[[run_id]]$rel_vars
    rel_vars_rank <- fs_results[[run_id]]$rel_vars_rank

    local_reps <- get_hclust_reps(
      rafs_results[[run_id]][[dist_fun_name]][[hclust_method]],
      n,
      rel_vars_rank
    )

    # restore global indices
    local_reps <- rel_vars[local_reps]

    for (rep_idx in local_reps) {
      rep <- paste0(rep_idx)
      if (rep %in% names(reps_popcnts)) {
        reps_popcnts[[rep]] <- reps_popcnts[[rep]] + 1
      } else {
        reps_popcnts[[rep]] <- 1
      }
    }
  }

  reps_popcnts
}


# n - number of clusters
get_local_rep_tuples_popcnts <- function(rafs_results, fs_results, dist_fun_name, hclust_method, n) {
  run_ids <- names(rafs_results)

  rep_tuples_popcnts <- list()

  for (run_id in run_ids) {
    rel_vars <- fs_results[[run_id]]$rel_vars
    rel_vars_rank <- fs_results[[run_id]]$rel_vars_rank

    local_reps <- get_hclust_reps(
      rafs_results[[run_id]][[dist_fun_name]][[hclust_method]],
      n,
      rel_vars_rank
    )

    # restore global indices
    local_reps <- rel_vars[local_reps]

    # canonicalise the tuple identifier (clusters' order may vary!)
    tuple_id <- paste(local_reps[order(local_reps)], collapse = "_")

    if (tuple_id %in% names(rep_tuples_popcnts)) {
      rep_tuples_popcnts[[tuple_id]] <- rep_tuples_popcnts[[tuple_id]] + 1
    } else {
      rep_tuples_popcnts[[tuple_id]] <- 1
    }
  }

  rep_tuples_popcnts
}


# n - number of clusters
get_local_rep_tuples_matrix <- function(rafs_results, fs_results, interesting_reps, dist_fun_name, hclust_method, n) {
  run_ids <- names(rafs_results)

  result_matrix <- matrix(0, nrow = length(interesting_reps), ncol = length(interesting_reps))
  rownames(result_matrix) <- colnames(result_matrix) <- interesting_reps

  for (run_id in run_ids) {
    rel_vars <- fs_results[[run_id]]$rel_vars
    rel_vars_rank <- fs_results[[run_id]]$rel_vars_rank

    local_reps <- get_hclust_reps(
      rafs_results[[run_id]][[dist_fun_name]][[hclust_method]],
      n,
      rel_vars_rank
    )

    # restore global indices
    local_reps <- rel_vars[local_reps]
    # filter for interesting
    is_interesting <- local_reps %in% interesting_reps
    interesting_local_reps <- local_reps[is_interesting]
    # stringify
    interesting_local_reps_strings <- paste0(interesting_local_reps)

    result_matrix[interesting_local_reps_strings, interesting_local_reps_strings] <- result_matrix[interesting_local_reps_strings, interesting_local_reps_strings] + 1
  }

  result_matrix
}


# n - number of clusters
get_local_occurrence_matrix <- function(rafs_results, fs_results, interesting_reps, dist_fun_name, hclust_method, n) {
  run_ids <- names(rafs_results)

  result_matrix <- matrix(0, nrow = length(interesting_reps), ncol = length(interesting_reps))
  rownames(result_matrix) <- colnames(result_matrix) <- interesting_reps

  for (run_id in run_ids) {
    rel_vars <- fs_results[[run_id]]$rel_vars

    hclust_result <- rafs_results[[run_id]][[dist_fun_name]][[hclust_method]]
    memberships <- cutree(hclust_result, k = n)

    is_interesting_rel_var <- rel_vars %in% interesting_reps
    interesting_rel_vars <- rel_vars[is_interesting_rel_var]
    interesting_rel_vars_strings <- paste0(interesting_rel_vars)
    interesting_memberships <- memberships[is_interesting_rel_var]

    for (cluster_idx in 1:n) {
      cluster_interesting_rel_vars_strings <- interesting_rel_vars_strings[interesting_memberships == cluster_idx]
      result_matrix[cluster_interesting_rel_vars_strings, cluster_interesting_rel_vars_strings] <- result_matrix[cluster_interesting_rel_vars_strings, cluster_interesting_rel_vars_strings] + 1
    }
  }

  result_matrix
}


#' @importFrom stats cutree
get_hclust_reps <- function(hclust_result, k, vars_rank) {
  k <- min(k, length(vars_rank))
  cuts <- cutree(hclust_result, k = k)
  reps <- 1:k
  for (i in 1:k) {
    cut_indices <- which(cuts == i)
    max_in_cut <- which.min(vars_rank[cut_indices])
    reps[i] <- cut_indices[max_in_cut]
  }
  reps
}
