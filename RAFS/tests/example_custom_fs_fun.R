# to obtain an example madelon dataset
library(MDFS)
mdfs_omp_set_num_threads(1)  # only to pass CRAN checks
data(madelon)
data <- madelon$data
decision <- madelon$decision

# to get repeatable results
set.seed(12345)


#
# Main flow starts below this comment.
#

library(RAFS)

# enhanced to use FDR with 0.10 level and keep the raw p-values for custom needs
custom_fs_fun <- function(train_data, train_decision, seed) {
  fs_result <- MDFS(
    train_data,
    train_decision,
    dimensions = 1,
    divisions = 1,
    discretizations = 30,
    seed = seed,
    p.adjust.method = "fdr",
    level = 0.10
  )

  return(list(
    p_vals = fs_result$p.value,
    rel_vars = fs_result$relevant.variables,
    rel_vars_rank = order(fs_result$p.value[fs_result$relevant.variables])
  ))
}

# using 2 CV loops (seeds) only to speed things up
rafs_results <- run_rafs(data, decision, seeds = sample.int(32767, 2), fs_fun = custom_fs_fun)

# finding top 5 representatives
rafs_reps_popcnts <- get_rafs_reps_popcnts(rafs_results, n_clusters_range = 5)
rafs_top_reps_5 <- get_rafs_top_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)
