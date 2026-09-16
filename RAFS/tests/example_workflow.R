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

# using 2 CV loops (seeds) only to speed things up
rafs_results <- run_rafs(data, decision, seeds = sample.int(32767, 2))

# finding representatives
rafs_reps_popcnts <- get_rafs_reps_popcnts(rafs_results, n_clusters_range = 2:5)

rafs_top_reps_5 <- get_rafs_top_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)

rafs_all_reps_5 <- get_rafs_all_reps_from_popcnts(rafs_reps_popcnts$stig_single, 5)

# findings representative tuples
rafs_rep_tuples_popcnts <- get_rafs_rep_tuples_popcnts(rafs_results, n_clusters_range = 2:5)

rafs_top_tuple_5 <- get_rafs_top_rep_tuples_from_popcnts(rafs_rep_tuples_popcnts$stig_single, 5)

# learning more about the dataset structure

# how many times did the representatives build together the tuple? (i.e., be in different clusters AND be representatives at the same time)
rafs_rep_tuples_matrix <- get_rafs_rep_tuples_matrix(rafs_results, rafs_all_reps_5, n_clusters_range = 2:5)

# how many times did the representatives share the same cluster?
rafs_occurrence_matrix <- get_rafs_occurrence_matrix(rafs_results, rafs_all_reps_5, n_clusters_range = 2:5)
