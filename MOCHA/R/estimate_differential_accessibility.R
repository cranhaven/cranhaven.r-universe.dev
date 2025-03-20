#' @title \code{estimate_differential_accessibility}
#'
#' @description \code{estimate_differential_accessibility} internal function for computing differential accessibility on a set of tiles
#'
#'
#' @param tile_values vector of intensity values extracted from the sampleTileMatrix
#' @param group: vector of group indices (0,1)
#'
#' @return a table indicating the results for differential accessibility
#'         which includes the following pieces of information
#'         - Tile = Tile ID
#'         - P_value = P-value from two-part wilcoxon test
#'         - TestStatistic = statistic from two-part wilcoxon test
#'         - HL_EffectSize = hodges_lehmann effect size,
#'         - Case_mu = Median of nonzero values in case condition,
#'         - Case_rho = proportion of zeros in case condition ,
#'         - Control_mu = Median of nonzero values in
#'          control condition
#'         - Control_rho = proportion of zeros in control condition ,
#'
#'
#' @details The technical details of the algorithm are found in XX.
#'
#' @references XX
#'
#' @noRd

estimate_differential_accessibility <- function(tile_values, group,
                                                providePermutation = FALSE) {
  data_vec <- as.numeric(tile_values)

  ## conduct two part test
  two_part_results <- TwoPart(data_vec, group = group, test = "wilcoxon", point.mass = 0)

  ## filter non-zero values for group1
  nonzero_dx <- data_vec[group == 1]
  nonzero_dx <- nonzero_dx[nonzero_dx != 0]

  ## filter non-zero values for group0
  nonzero_control <- data_vec[group == 0]
  nonzero_control <- nonzero_control[nonzero_control != 0]

  # df <- data.frame(
  #   Vals = data_vec,
  #   Group = group,
  #   Group_label = ifelse(group == 1, "case", "Control")
  # )

  ## create all pairwise combinations
  pairwise_matrix <- data.table::as.data.table(expand.grid(nonzero_dx, nonzero_control))
  pairwise_matrix$diff <- pairwise_matrix[, 1] - pairwise_matrix[, 2]

  hodges_lehmann <- stats::median(pairwise_matrix$diff)
  meanDiff <- calculateMeanDiff(data_vec, group)
  ## Create Final Results Matrix
  res <- data.frame(
    P_value = two_part_results$pvalue,
    TestStatistic = two_part_results$statistic,
    Log2FC_C = hodges_lehmann,
    MeanDiff = meanDiff,
    Case_mu = stats::median(nonzero_dx),
    Case_rho = mean(data_vec[group == 1] == 0),
    Control_mu = stats::median(nonzero_control),
    Control_rho = mean(data_vec[group == 0] == 0)
  )

  if (providePermutation) {
    nPerm <- 10

    permuted_pvals <- t(data.frame(sapply(1:nPerm, function(x) {

      ## permutation test
      n_a <- sum(group)
      n_b <- length(group) - n_a

      ## shuffle observations
      sampled_data <- sample(data_vec, size = n_a + n_b, replace = FALSE)

      ## calculate hypothesis test
      ## based on shuffled values
      TwoPart(sampled_data,
        group = group, test = "wilcoxon",
        point.mass = 0
      )$pvalue
    })))
    colnames(permuted_pvals) <- paste("Permute", 1:nPerm, sep = "_")
    res <- cbind(res, (permuted_pvals))
  }

  res
}

calculateMeanDiff <- function(tile_values, group) {
  a <- log2(tile_values[which(group == 1)] + 1)
  b <- log2(tile_values[which(group == 0)] + 1)

  mean_diff <- mean(a) - mean(b)
  mean_diff
}
