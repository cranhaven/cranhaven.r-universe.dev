#' @title Bicluster data with non-random missing values
#' @description Bicluster data with non-random missing values
#'
#' @param data Dataset to bicluster. Must to be a data matrix with only numbers
#'     and missing values in the data set. It should have row names and column names.
#' @param row_clusters The number of clusters to partition the rows into. The
#'     default is \code{floor(sqrt(nrow(data)))}.
#' @param col_clusters The number of clusters to partition the columns into. The
#'     default is \code{floor(sqrt(ncol(data)))}.
#' @param miss_val Value or function to put in empty cells of the prototype matrix.
#'     If a value, a random normal variable with sd = `miss_val_sd` is used each
#'     iteration. By default, this equals the mean of \code{data}.
#' @param miss_val_sd Standard deviation of the normal distribution `miss_val` follows
#'     if `miss_val` is a number. By default this equals 1.
#' @param similarity The metric used to compare two successive clusterings. Can be
#'     "Rand" (default), "HA" for the Hubert and Arabie adjusted Rand index or "Jaccard".
#'     See \link[phyclust]{RRand} for details.
#' @param row_min_num Minimum row prototype size in order to be eligible to be
#'     chosen when filling an empty row prototype. Default is \code{floor(nrow(data) / row_clusters)}.
#' @param col_min_num Minimum column prototype size in order to be eligible to be
#'     chosen when filling an empty row prototype. Default is \code{floor(ncol(data) / col_clusters)}.
#' @param row_num_to_move Number of rows to remove from the sampled prototype to
#'     put in the empty row prototype. Default is 1.
#' @param col_num_to_move Number of columns to remove from the sampled prototype to
#'     put in the empty column prototype. Default is 1.
#' @param row_shuffles Number of times to shuffle rows in each iteration. Default is 1.
#' @param col_shuffles Number of times to shuffle columns in each iteration. Default is 1.
#' @param max.iter Maximum number of iterations to let the algorithm run for.
#' @param verbose Logical. If TRUE, will report progress.
#' @export
#' @importFrom phyclust RRand
#' @importFrom stats rnorm
#' @return A list of class \code{biclustermd}:
#'     \item{params }{a list of all arguments passed to the function, including defaults.}
#'     \item{data }{the inputted two way table of data.}
#'     \item{P0 }{the initial column partition matrix.}
#'     \item{Q0 }{the initial row partition matrix.}
#'     \item{InitialSSE }{the SSE of the original partitioning.}
#'     \item{P }{the final column partition matrix.}
#'     \item{Q }{the final row partition matrix.}
#'     \item{SSE }{a matrix of class biclustermd_sse detailing the SSE recorded at the end of each iteration.}
#'     \item{Similarities }{a data frame of class biclustermd_sim detailing the
#'         value of row and column similarity measures recorded at the end of each
#'         iteration. Contains information for all three similarity measures.
#'         This carries an attribute `"used"` which provides the similarity
#'         measure used as the stopping condition for the algorithm.}
#'     \item{iteration }{the number of iterations the algorithm ran for, whether \code{max.iter} was reached or convergence was achieved.}
#'     \item{A }{the final prototype matrix which gives the average of each bicluster.}
#'
#' @seealso \code{\link{rep_biclustermd}}, \code{\link{tune_biclustermd}}
#'
#' @references Li, J., Reisner, J., Pham, H., Olafsson, S., and Vardeman, S. (2020) \emph{Biclustering with Missing Data. Information Sciences, 510, 304–316.}
#'
#' @examples
#' data("synthetic")
#' # default parameters
#' bc <- biclustermd(synthetic)
#' bc
#' autoplot(bc)
#'
#' # providing the true number of row and column clusters
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2)
#' bc
#' autoplot(bc)
#'
#' # an example with the nycflights13::flights dataset
#' library(nycflights13)
#' data("flights")
#'
#' library(dplyr)
#' flights_bcd <- flights %>%
#'   select(month, dest, arr_delay)
#'
#' flights_bcd <- flights_bcd %>%
#'   group_by(month, dest) %>%
#'   summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
#'   spread(dest, mean_arr_delay) %>%
#'   as.data.frame()
#'
#' rownames(flights_bcd) <- flights_bcd$month
#' flights_bcd <- as.matrix(flights_bcd[, -1])
#'
#' flights_bc <- biclustermd(data = flights_bcd, col_clusters = 6, row_clusters = 4,
#'                   row_min_num = 3, col_min_num = 5,
#'                   max.iter = 20, verbose = TRUE)
#' flights_bc
#'


biclustermd <- function(data,
                        row_clusters = floor(sqrt(nrow(data))),
                        col_clusters = floor(sqrt(ncol(data))),
                        miss_val = mean(data, na.rm = TRUE),
                        miss_val_sd = 1, similarity = "Rand",
                        row_min_num = floor(nrow(data) / row_clusters),
                        col_min_num = floor(ncol(data) / col_clusters),
                        row_num_to_move = 1, col_num_to_move = 1,
                        row_shuffles = 1, col_shuffles = 1,
                        max.iter = 100, verbose = FALSE) {

  if(!inherits(data, c("matrix", "data.frame"))) {
    stop("`data` must be a matrix or a data.frame.")
  }
  if(inherits(data, "data.frame")) {
    data <- as.matrix(data)
  }
  if(is.null(rownames(data))) {
    stop("`data` does not have row names.")
  }
  if(is.null(colnames(data))) {
    stop("`data` does not have column names.")
  }
  if(is.expression(col_clusters)) {
    condition_call <- substitute(col_clusters)
    if(is.na(eval(condition_call))) {
      stop("`col_clusters` must not be NA.")
    }
    if(length(eval(condition_call)) > 0) {
      stop("`col_clusters` must be a single integer or function which evaluates to such.")
    }
  } else if(length(col_clusters) > 1) {
    stop("`col_clusters` must be a single integer or function which evaluates to such.")
  }
  if(is.expression(row_clusters)) {
    condition_call <- substitute(row_clusters)
    if(is.na(eval(condition_call))) {
      stop("`row_clusters` must not be NA.")
    }
    if(length(eval(condition_call)) > 0) {
      stop("`row_clusters` must be a single integer or function which evaluates to such.")
    }
  } else if(length(row_clusters) > 1) {
    stop("`row_clusters` must be a single integer or function which evaluates to such.")
  }

  if(length(similarity) > 1) {
    warning(
      paste0("Only one similarity metric can be used as a stopping condition. Using the first supplied, ", similarity[1])
    )
    similarity <- similarity[1]
  }

  if(is.expression(miss_val)) {
    condition_call <- substitute(miss_val)
    if(is.na(eval(condition_call))) {
      stop("`miss_val` is NA")
    }
  }


  data <- as.matrix(data)
  m_d <- nrow(data)
  n_d <- ncol(data)

  P0 <- partition_gen(n_d, col_clusters)
  Q0 <- partition_gen(m_d, row_clusters)

  P <- P0
  Q <- Q0

  result_list <- vector("list", 11)
  names(result_list) <- c("params", "data", "P0", "Q0", "InitialSSE", "P", "Q", "SSE", "Similarities", "iteration", "A")
  result_list$params <- mget(names(formals()),sys.frame(sys.nframe()))[-1]
  result_list$P0 <- P0
  result_list$Q0 <- Q0

  InitialSSE <- cluster_iteration_sum_sse(data, P, Q)

  SSE <- matrix(nrow = max.iter, ncol = 2)
  colnames(SSE) <- c("SSE", "Iteration")

  # RIs <- matrix(nrow = max.iter, ncol = 3)
  # colnames(RIs) <- c("P_sim", "Q_sim", "Iteration")

  Similarities <- matrix(nrow = max.iter, ncol = 7)
  colnames(Similarities) <- c("P_rand", "P_ha", "P_jaccard", "Q_rand", "Q_ha", "Q_jaccard", "Iteration")

  n_p <- ncol(P)
  n_q <- ncol(Q)

  s <- 0

  A <- matrix(nrow = n_q, ncol = n_p)
  p1 <- numeric(n_d)
  q1 <- numeric(m_d)

  dat2 <- data
  for (i in 1:m_d){
    for (j in 1:n_d){
      if (!is.na(data[i, j])) {
        dat2[i, j] <- 1
      }
    }
  }

  while(s < max.iter) {
    if(verbose) {
      if(s %% 10 == 0) {
        cat("Iteration ", s, "\r")
      }
    }


    P_old <- P
    Q_old <- Q


    for(k in 1:row_shuffles) {
      for (j in 1:n_q){
        q_ind <- which(Q[, j] == 1)

        for (i in 1:n_p){
          p_ind <- which(P[, i] == 1)

          x_ij_prime <- data[q_ind, p_ind]

          A[j, i] <- sum(x_ij_prime, na.rm = TRUE) /
            (length(x_ij_prime) - sum(is.na(x_ij_prime)))

          if(is.na(A[j, i])) {
            if(is.numeric(miss_val)) {
              A[j, i] <- rnorm(1, miss_val, miss_val_sd)
            } else {
              condition_call <- substitute(miss_val)
              A[j, i] <- eval(condition_call)
            }
          }
        }
      }


      col_cluster_mean <- matrix(0, nrow = m_d, ncol = n_p)  # is M_{in}^R in paper
      distq <- matrix(nrow = m_d, ncol = n_q)  # is d_{im}^R in paper
      dq <- matrix(0, nrow = n_q, ncol = n_p)  # is value in parens in d_{im}^R in paper
      for(i in 1:m_d) {
        col_cluster_cnt <- apply(dat2[i,] * P, 2, sum, na.rm = T)
        col_cluster_mean[i,] <- apply(data[i,] * P, 2, sum, na.rm = T) / col_cluster_cnt
        col_cluster_mean[i, which(col_cluster_mean[i,] == 0 | is.na(col_cluster_mean[i,]))] <- miss_val
        for(j in 1:n_q) {
          dq[j,] <- A[j,] - col_cluster_mean[i,]
          distq[i, j] <- sum((dq[j,] ^ 2) * col_cluster_cnt, na.rm = TRUE)
        }
      }

      q1 <- unlist(lapply(1:m_d, function(x) which.min(distq[x,])), use.names = FALSE)

      Q <- partition_gen_by_p(m_d, n_q, q1)

      Q <- fill_empties_Q(data, Q, row_min_num, row_num_to_move)
    }

    for(k in 1:col_shuffles) {
      for (j in 1:n_q) {
        q_ind <- which(Q[, j] == 1)
        for (i in 1:n_p) {
          p_ind <- which(P[, i] == 1)

          x_ij_prime <- data[q_ind, p_ind]

          A[j, i] <- sum(x_ij_prime, na.rm = TRUE) /
            (length(x_ij_prime) - sum(is.na(x_ij_prime)))

          if(is.na(A[j, i])) {
            if(is.numeric(miss_val)) {
              A[j, i] <- rnorm(1, miss_val, miss_val_sd)
            } else {
              condition_call <- substitute(miss_val)
              A[j, i] <- eval(condition_call)
            }
          }
        }
      }

      row_cluster_mean <- matrix(0, nrow = n_d, ncol = n_q)  # is M_{jm}^R in paper
      distp <- matrix(nrow = n_d, ncol = n_p)  # is d_{jn}^R in paper
      dp <- matrix(0, nrow = n_p, ncol = n_q)  # is value in parens in d_{jn}^R in paper
      for (i in 1:n_d){
        row_cluster_cnt <- apply(dat2[, i] * Q, 2, sum, na.rm = T)
        row_cluster_mean[i, ] <- apply(data[, i] * Q, 2, sum, na.rm = T) / row_cluster_cnt
        row_cluster_mean[i, which(row_cluster_mean[i, ] == 0 | is.na(row_cluster_mean[i, ]))] <- miss_val
        for (j in 1:n_p){
          dp[j,] <- A[, j] - row_cluster_mean[i, ]
          distp[i, j] <- sum((dp[j, ] ^ 2) * row_cluster_cnt)
        }
      }

      p1 <- unlist(lapply(1:n_d, function(x) which.min(distp[x,])), use.names = FALSE)

      P <- partition_gen_by_p(n_d, n_p, p1)

      P <- fill_empties_P(data, P, col_min_num, col_num_to_move)
    }

    s <- s + 1

    SSE[s, 1] <- cluster_iteration_sum_sse(data, P, Q)
    SSE[s, 2] <- s - 1

    P_old_vec <- part_matrix_to_vector(P_old) + 1
    P_new_vec <- part_matrix_to_vector(P) + 1
    Q_old_vec <- part_matrix_to_vector(Q_old) + 1
    Q_new_vec <- part_matrix_to_vector(Q) + 1

    # PRI <- RRand(P_old_vec, P_new_vec)[[1]]
    # P_sim <- adjustedRand(P_old_vec, P_new_vec, randMethod = similarity)
    # QRI <- RRand(Q_old_vec, Q_new_vec)[[1]]
    # Q_sim <- adjustedRand(Q_old_vec, Q_new_vec, randMethod = similarity)

    if(similarity == 'Rand') {
      P_sim <- RRand(P_old_vec, P_new_vec)[[1]]
      Q_sim <- RRand(Q_old_vec, Q_new_vec)[[1]]
    } else if(similarity == 'HA') {
      P_sim <- RRand(P_old_vec, P_new_vec)[[2]]
      Q_sim <- RRand(Q_old_vec, Q_new_vec)[[2]]
    } else if(similarity == 'Jaccard') {
      # P_sim <- cluster_similarity(P_old_vec, P_new_vec, similarity = 'jaccard')
      # Q_sim <- cluster_similarity(Q_old_vec, Q_new_vec, similarity = 'jaccard')
      P_sim <- jaccard_similarity(P_old_vec, P_new_vec)
      Q_sim <- jaccard_similarity(Q_old_vec, Q_new_vec)
    }

    # RIs[s, 1] <- P_sim
    # RIs[s, 2] <- Q_sim
    # RIs[s, 3] <- s - 1
    Similarities[s, 1:2] <- unlist(RRand(P_old_vec, P_new_vec)[1:2])
    # Similarities[s, 2] <- RRand(P_old_vec, P_new_vec)[[2]]
    # Similarities[s, 3] <- cluster_similarity(P_old_vec, P_new_vec, similarity = 'jaccard')
    Similarities[s, 3] <- jaccard_similarity(P_old_vec, P_new_vec)
    Similarities[s, 4:5] <- unlist(RRand(Q_old_vec, Q_new_vec)[1:2])
    # Similarities[s, 5] <- RRand(Q_old_vec, Q_new_vec)[[2]]
    # Similarities[s, 6] <- cluster_similarity(Q_old_vec, Q_new_vec, similarity = 'jaccard')
    Similarities[s, 6] <- jaccard_similarity(Q_old_vec, Q_new_vec)

    # Similarities[s, 1:3] <- adjustedRand(P_old_vec, P_new_vec, randMethod = c("Rand", "HA", "Jaccard"))
    # Similarities[s, 4:6] <- adjustedRand(Q_old_vec, Q_new_vec, randMethod = c("Rand", "HA", "Jaccard"))
    Similarities[s, 7] <- s - 1

    if((P_sim == 1) && (Q_sim == 1)) {
      result_list$data <- data
      result_list$P <- P
      result_list$Q <- Q
      result_list$InitialSSE <- InitialSSE
      result_list$SSE <- SSE
      result_list$Similarities <- data.frame(Similarities)
      result_list$iteration <- s
      result_list$A <- A

      result_list$SSE <- result_list$SSE[1:s,]
      result_list$Similarities <- result_list$Similarities[1:s,]

      class(result_list$SSE) <- c("biclustermd_sse", "matrix")
      class(result_list$Similarities) <- c("biclustermd_sim", "data.frame")
      attr(result_list$Similarities, "used") <- similarity[1]
      class(result_list) <- c("biclustermd", "list")

      result_list
      break
    }


  }

  result_list$data <- data
  result_list$P <- P
  result_list$Q <- Q
  result_list$InitialSSE <- InitialSSE
  result_list$SSE <- SSE
  result_list$Similarities <- data.frame(Similarities)

  result_list$SSE <- result_list$SSE[1:s,]
  result_list$Similarities <- result_list$Similarities[1:s,]

  result_list$iteration <- s
  result_list$A <- A

  class(result_list$SSE) <- c("biclustermd_sse", "matrix")
  class(result_list$Similarities) <- c("biclustermd_sim", "data.frame")
  attr(result_list$Similarities, "used") <- similarity[1]
  class(result_list) <- c("biclustermd", "list")

  result_list

}



