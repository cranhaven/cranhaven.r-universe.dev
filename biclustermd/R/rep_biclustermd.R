#' Repeat a biclustering to achieve a minimum SSE solution
#'
#' @param data Dataset to bicluster. Must to be a data matrix with only numbers and missing values in the data set. It should have row names and column names.
#' @param nrep The number of times to repeat the biclustering. Default 10.
#' @param parallel Logical indicating if the user would like to utilize the
#'     \code{foreach} parallel backend. Default is FALSE.
#' @param ncores The number of cores to use if parallel computing. Default 2.
#' @param col_clusters The number of clusters to partition the columns into.
#' @param row_clusters The number of clusters to partition the rows into.
#' @param miss_val Value or function to put in empty cells of the prototype matrix.
#'     If a value, a random normal variable with sd = `miss_val_sd` is used each iteration.
#' @param miss_val_sd Standard deviation of the normal distribution `miss_val` follows
#'     if `miss_val` is a number. By default this equals 1.
#' @param similarity The metric used to compare two successive clusterings. Can be
#'     "Rand" (default), "HA" for the Hubert and Arabie adjusted Rand index or "Jaccard".
#'     See \link[phyclust]{RRand} and for details.
#' @param col_min_num Minimum column prototype size in order to be eligible to be chosen when filling an empty row prototype. Default is 5.
#' @param row_min_num Minimum row prototype size in order to be eligible to be chosen when filling an empty row prototype. Default is 5.
#' @param col_num_to_move Number of columns to remove from the sampled prototype to put in the empty column prototype. Default is 1.
#' @param row_num_to_move Number of rows to remove from the sampled prototype to put in the empty row prototype. Default is 1.
#' @param row_shuffles Number of times to shuffle rows in each iteration. Default is 1.
#' @param col_shuffles Number of times to shuffle columns in each iteration. Default is 1.
#' @param max.iter Maximum number of iterations to let the algorithm run for.
#' @export
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @return A list of the minimum SSE biclustering, a vector containing
#'     the final SSE of each repeat, and the time it took the function to run.
#'
#' @seealso \code{\link{biclustermd}}, \code{\link{tune_biclustermd}}
#'
#' @references Li, J., Reisner, J., Pham, H., Olafsson, S., and Vardeman, S. (2019) \emph{Biclustering for Missing Data. Information Sciences, Submitted}
#'
#' @examples
#' data("synthetic")
#'
#' # 20 repeats without parallelization
#' repeat_bc <- rep_biclustermd(synthetic, nrep = 20,
#'                              col_clusters = 3, row_clusters = 2,
#'                              miss_val = mean(synthetic, na.rm = TRUE),
#'                              miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                              col_min_num = 2, row_min_num = 2,
#'                              col_num_to_move = 1, row_num_to_move = 1,
#'                              max.iter = 10)
#' repeat_bc
#' autoplot(repeat_bc$best_bc)
#' plot(repeat_bc$rep_sse, type = 'b', pch = 20)
#' repeat_bc$runtime
#'
#' # 20 repeats with parallelization over 2 cores
#' repeat_bc <- rep_biclustermd(synthetic, nrep = 20, parallel = TRUE, ncores = 2,
#'                              col_clusters = 3, row_clusters = 2,
#'                              miss_val = mean(synthetic, na.rm = TRUE),
#'                              miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                              col_min_num = 2, row_min_num = 2,
#'                              col_num_to_move = 1, row_num_to_move = 1,
#'                              max.iter = 10)
#' repeat_bc$runtime


rep_biclustermd <- function(data, nrep = 10, parallel = FALSE, ncores = 2,
                            col_clusters = floor(sqrt(ncol(data))),
                            row_clusters = floor(sqrt(nrow(data))),
                            miss_val = mean(data, na.rm = TRUE),
                            miss_val_sd = 1, similarity = "Rand",
                            row_min_num = 5, col_min_num = 5,
                            row_num_to_move = 1, col_num_to_move = 1,
                            row_shuffles = 1, col_shuffles = 1, max.iter = 100) {
  mcall <- as.list(match.call())
  mcall <- mcall[-1]
  mcall <- mcall[!(names(mcall) %in% c("nrep", "parallel", "ncores", "verbose"))]
  mcall <- lapply(mcall, function(z) ifelse(class(z) == "call", eval(z), z))
  mcall$data <- data

  if(!parallel) {

    st <- proc.time()
    best_sse <- .Machine$double.xmax
    sse <- numeric(nrep)
    for(i in 1:nrep) {

      bc <- do.call(biclustermd, mcall)

      sse[i] <- bc$SSE[bc$iteration, 1]

      if(sse[i] < best_sse) {

        best_sse <- sse[i]
        best_bc <- bc

      }
    }
    et <- proc.time()

    list(
      best_bc = best_bc,
      rep_sse = sse,
      runtime = et - st
    )

  } else if(parallel) {

    st <- proc.time()
    cl <- makeCluster(ncores)
    registerDoParallel(cl)

    results <- foreach(i = 1:nrep, .export = 'biclustermd') %dopar% {

      do.call(biclustermd, mcall)

    }

    stopCluster(cl)

    sse <- sapply(results, function(z) z$SSE[z$iteration, 1])
    best_bc <- results[[which.min(sse)]]
    rm(results)
    et <- proc.time()

    list(
      best_bc = best_bc,
      rep_sse = sse,
      runtime = et - st
    )

  }

}
