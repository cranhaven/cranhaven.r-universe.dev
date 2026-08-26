library(Matrix)
library(testthat)
library(stringdist)


.hamming_modif_cutoff <- function(str1, str2, cutoff) {
  if (abs(nchar(str1) - nchar(str2)) > cutoff) {
    return(FALSE)
  } else {
    min_len <- min(nchar(str1), nchar(str2))
    max_len <- max(nchar(str1), nchar(str2))
    str1_ <- substring(str1, 1, min_len)
    str2_ <- substring(str2, 1, min_len)
    dist <- stringdist::stringdist(str1_, str2_, "hamming")
    dist <- dist + (max_len - min_len)
    return(dist <= cutoff)
  }
}

.levenshtein_cutoff <- function(str1, str2, cutoff) {
  if (abs(nchar(str1) - nchar(str2)) > cutoff) {
    return(FALSE)
  } else {
    dist <- stringdist::stringdist(str1, str2, "lv")
    return(dist <= cutoff)
  }
}

.get_dist_func <- function(metric) {
  if (metric == "Hamming") {
    return(.hamming_modif_cutoff)
  } else if (metric == "Levenshtein") {
    return(.levenshtein_cutoff)
  }
}

.drop_triv_deg <- function(adj_matrix, strings) {
  col_sums <- Matrix::colSums(adj_matrix)
  non_triv_ids <- which(col_sums > 1)
  non_triv_ids <- unname(non_triv_ids)
  adj_matrix <- adj_matrix[non_triv_ids, non_triv_ids]
  dimnames(adj_matrix)[[1]] <- non_triv_ids
  dimnames(adj_matrix)[[2]] <- strings[non_triv_ids]
  return(adj_matrix)
}

.sim_join_manually <- function(
  strings, cutoff, metric, drop_deg_one_bul, out_mode
) {
  dist_func <- .get_dist_func(metric)
  n <- length(strings)
  if (out_mode == "adj_matrix")
    adj_matrix <- sparseMatrix(i = (1:n), j = (1:n), x = rep(1, n))
  else
    adj_pairs <- c()
  for (i in 1:(n - 1)) {
    string_i <- strings[i]
    if (out_mode == "adj_pairs")
      adj_pairs <- rbind(adj_pairs, c(i, i))
    for (j in (i + 1):n) {
      if (dist_func(string_i, strings[j], cutoff)) {
        if (out_mode == "adj_matrix") {
          adj_matrix[i, j] <- 1
          adj_matrix[j, i] <- 1
        } else {
          adj_pairs <- rbind(adj_pairs, c(i, j))
          adj_pairs <- rbind(adj_pairs, c(j, i))
        }
      }
    }
  }
  if (out_mode == "adj_matrix") {
    dimnames(adj_matrix)[[1]] <- (1:n)
    dimnames(adj_matrix)[[2]] <- strings
    if (drop_deg_one_bul) {
      adj_matrix <- .drop_triv_deg(adj_matrix, strings)
    }
    return(adj_matrix)
  } else {
    adj_pairs <- rbind(adj_pairs, c(n, n))
    return(matrix(adj_pairs, ncol = 2))
  }
}


.gen_all_bin_strings <- function(length) {
  combinations <- expand.grid(rep(list(c("a", "b")), length))
  strings <- apply(combinations, 1, paste, collapse = "")
  return(strings)
}
all_bin_strings <- unlist(lapply(1:5, .gen_all_bin_strings))

adhoc_list <- c(
  "abc", "abx", "xyz", "xya", "dear", "bear",
  "cat", "bat", "water", "kitten", "sitten", "flaw", "flaws",
  "hello", "hell", "world", "words", "apple", "ample",
  "banana", "bananas", "dog", "dot", "bird", "birth",
  "moon", "moons", "tree", "trie", "house", "horse",
  "light", "sight", "water", "water", "waste", "stone", "stove",
  "plane", "plant", "cloud", "clown", "rain", "train",
  "car", "bar", "far", "fear", "near", "dear", "bear",
  "transcendentalism", "transcndentalism", "plane",
  "transocendentalipm", "anscendentalism", "moon", "plan", "aaaaaa", "bbbbb"
)

test_data <- list(
  all_bin_strings = all_bin_strings,
  adhoc_list = adhoc_list
)

test_that("testing similarityJoin", {
  for (cutoff in c(0, 1, 2)) {
    for (metric in c("Hamming", "Levenshtein")) {
      for (method in c("pattern", "semi_pattern", "partition_pattern")) {
        for (drop_deg_one_bul in c(TRUE, FALSE)) {
          for (string_list_name in names(test_data)) {
            string_list <- test_data[[string_list_name]]
            for (test_mode in c("adj_matrix", "adj_pairs")) {
              if (test_mode == "adj_pairs" && drop_deg_one_bul == TRUE) {
                next
              }
              manual_result <- .sim_join_manually(
                string_list, cutoff, metric, drop_deg_one_bul, test_mode
              )
              auto_result <- similarityJoin(
                string_list, cutoff, metric, method, drop_deg_one_bul,
                output_format = test_mode
              )
              if (test_mode == "adj_pairs") {
                manual_result <- manual_result[order(
                  manual_result[, 1], manual_result[, 2]), ]
                auto_result <- auto_result[order(
                  auto_result[, 1], auto_result[, 2]), ]
              }
              expect_equal(
                manual_result, auto_result,
                info = paste("Failed for cutoff =", cutoff,
                             "\nmetric =", metric,
                             "\nmethod =", method,
                             "\ndrop_deg_one =", drop_deg_one_bul,
                             "\ntest_mode =", test_mode,
                             "\ntest_data =", string_list_name,
                             "\n")
              )
            }
          }
        }
      }
    }
  }
})