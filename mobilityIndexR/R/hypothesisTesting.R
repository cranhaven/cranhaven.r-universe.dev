#' @title Hypothesis Test for Two Mobility Datasets
#'
#' @description Calculates hypothesis tests of mobility indices from two datasets.
#' Specifically, for datasets A and B, this function performs one-sided nonparametric hypothesis tests
#' that the index value for A is greater than the corresponding index value for B.
#' Supports Prais-Bibby, Absolute Movement, Origin Specific, and Weighted Group Mobility indices and relative, mixed,
#' and absolute types of rankings in the calculation these indices.
#'
#' @param dat_A a dataframe with an "id" column
#' @param dat_B a dataframe with an "id" column
#' @param cols_A a list of character strings denoting the first and second column
#' to be used in the index calculations for dat_A
#' @param cols_B a list of character strings denoting the first and second column
#' to be used in the index calculations for dat_B
#' @param type a character string indicating the type of ranking;
#' accepts 'relative', 'mixed', and 'absolute'
#' @param indices a vector of character strings indicating which mobility indices are desired;
#' currently support 'prais_bibby', 'average_movement', 'wgm', and 'origin_specific'.
#' The default value is 'all'.
#' @param num_ranks an integer specifying the number of ranks for a relative or mixed ranking
#' @param exclude_value a single numeric value that is excluded in calculating the transition matrix;
#' see the rerank_exclude_value parameter to specify how the exclude value is handled
#' @param bounds a sequence of numeric bounds for defining absolute ranks
#' @param rerank_exclude_value a character string indicating how the exclude value is handled when present; accepts
#' 'as_new_rank', 'as_existing_rank', and 'exclude'
#' @param strict logical. If TRUE, indices are calculated from the given values. If FALSE,
#' indices are calculated by jittering the values to ensure uniqueness of bounds of ranks.
#' Only used with relative and mixed types. The default value is TRUE.
#' @param bootstrap_iter the number of bootstrap iterations used to estimate hypothesis tests.
#' The default value is 100.
#'
#' @return Returns a named vector containing the estimated probabilities that index value for dataset A is greater than the corresponding index value for dataset B
#' @export
#'
#' @examples
#' getHypothesisTest(dat_A = incomeMobility,
#'                   dat_B = incomeMobility,
#'                   cols_A = c("t0", "t3"),
#'                   cols_B = c("t5", "t8"),
#'                   type = "relative",
#'                   num_ranks = 5)
getHypothesisTest <- function(dat_A, dat_B, cols_A, cols_B, type, indices = "all", num_ranks,
                              exclude_value, bounds, rerank_exclude_value = FALSE, strict = TRUE,
                              bootstrap_iter = 100){
  bootstrap_samplesA <- makeBootstrapSamples(dat = dat_A, col_x = cols_A[1], col_y = cols_A[2], type = type, indices = indices, num_ranks = num_ranks, exclude_value = exclude_value, bounds = bounds, strict = strict, rerank_exclude_value = rerank_exclude_value, bootstrap_iter = bootstrap_iter)
  bootstrap_samplesB <- makeBootstrapSamples(dat = dat_B, col_x = cols_B[1], col_y = cols_B[2], type = type, indices = indices, num_ranks = num_ranks, exclude_value = exclude_value, bounds = bounds, strict = strict, rerank_exclude_value = rerank_exclude_value, bootstrap_iter = bootstrap_iter)
  stopifnot(colnames(bootstrap_samplesA) == colnames(bootstrap_samplesB))
  delta <- data.frame(1:bootstrap_iter)
  output <- list()
  for (index in colnames(bootstrap_samplesA)){
    delta[[index]] <- bootstrap_samplesA[[index]] - bootstrap_samplesB[[index]]
    output[index] <- nrow(delta[delta[[index]] > 0,])/nrow(delta)
  }
  return(output)
}
