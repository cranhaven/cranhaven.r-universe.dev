#' Get PTR estimates for output of the core pipeline on a subset of data
#'
#' @param p is the pipeline named list
#' @return a dataframe
#' \itemize{
#'   \item sample: sample
#'   \item est_ptr: PTR estimate
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
est_ptrs_subset <- function(p) {
  PC1 <- contig <- NULL
  if (length(p) == 1) {
    warning("PTR estimation on one subset was unsuccessful")
    return(NULL)
  }

  sample_correct_y_PC1 <- merge(reshape2::dcast(subset(p$correct_ys, select = c("sample", "contig", "correctY")), contig ~ sample, value.var = "correctY"), p$pc1)

  lm_model_co <- apply(subset(sample_correct_y_PC1, select = -c(contig, PC1)), 2, lm_column, y = sample_correct_y_PC1$PC1)
  cor_model <- apply(subset(sample_correct_y_PC1, select = -c(contig, PC1)), 2, function(x) cor.test(sample_correct_y_PC1$PC1, x)$estimate)

  est_ptrs <- data.frame("est_ptr" = 2^abs(lm_model_co[1, ] * (p$pc1_range[1] - p$pc1_range[2])), "coefficient" = lm_model_co[1, ], "pValue" = lm_model_co[2, ], "cor" = cor_model)
  est_ptrs$sample <- rownames(est_ptrs)

  merge(est_ptrs, aggregate(correctY ~ sample, p$correct_ys, FUN = "median"), by = "sample")
}

#' Compares contig subset x against contig subset y
#'
#' @param est_ptrs_x PTR estimates from contig subset x
#' @param est_ptrs_y PTR estimates from contig subset y
#' @param pipeline_x pipeline for contig subset x
#' @param pipeline_y pipeline for contig subset y
#' @param cor_cutoff the correlation cutoff
#' @param max_cor the max correlation
#' @return a named list including the est_ptr dataframe and a max_cor value
#' \itemize{
#'   \item sample: sample
#'   \item est_ptr: PTR estimate
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
#' max_cor: the max correlation achieved
compare_contig_subsets <- function(est_ptrs_x, est_ptrs_y, pipeline_x, pipeline_y, cor_cutoff, max_cor) {
  pipeline_fail_Q <- (length(pipeline_y) == 1) || (length(pipeline_x) == 1)
  too_much_overlap_Q <- length(pipeline_x$pc1$contig) - length(intersect(pipeline_x$pc1$contig, pipeline_y$pc1$contig)) < 3 || length(pipeline_y$pc1$contig) - length(intersect(pipeline_x$pc1$contig, pipeline_y$pc1$contig)) < 3

  if (pipeline_fail_Q || too_much_overlap_Q) {
    return(list(est_ptr = NULL, max_cor = max_cor))
  }

  minor_sample1 <- cor_diff(est_ptrs_x)
  minor_sample2 <- cor_diff(est_ptrs_y)

  # The rest of these filtering clauses are just weird, probably shouldn't be hardcoded cutoffs like this
  if ((length(minor_sample1) > 0 & length(minor_sample2) > 0)) { #| (max(est_ptrs_x$est_ptr) < 1.8 & max(est_ptrs_y$est_ptr) < 1.8) | (max(est_ptrs_x$est_ptr) / min(est_ptrs_x$est_ptr) > 5 & max(est_ptrs_y$est_ptr) / min(est_ptrs_y$est_ptr) > 5)) {
    return(list(est_ptr = NULL, max_cor = max_cor))
  }

  est_ptrs_x_y <- merge(est_ptrs_x, est_ptrs_y, by = "sample")

  if (nrow(est_ptrs_x_y) > 0.9 * max(c(nrow(est_ptrs_x), nrow(est_ptrs_y)))) {
    cor_xy <- cor(est_ptrs_x_y$est_ptr.x, est_ptrs_x_y$est_ptr.y)

    if (is.na(cor_xy)) {
      return(list(est_ptr = NULL, max_cor = max_cor))
    }

    if (cor_xy > max_cor) {
      max_cor <- cor_xy
    }

    if (cor_xy > cor_cutoff) {
      est_ptrs_x_y$est_ptr <- apply(subset(est_ptrs_x_y, select = c("est_ptr.x", "est_ptr.y")), 1, mean)
      est_ptrs_x_y$coefficient <- apply(subset(est_ptrs_x_y, select = c("coefficient.x", "coefficient.y")), 1, function(x) mean(abs(x)))
      est_ptrs_x_y$pValue <- apply(subset(est_ptrs_x_y, select = c("pValue.x", "pValue.y")), 1, max)
      est_ptrs_x_y$cor <- apply(subset(est_ptrs_x_y, select = c("cor.x", "cor.y")), 1, function(x) mean(abs(x)))
      est_ptrs_x_y$correctY <- apply(subset(est_ptrs_x_y, select = c("correctY.x", "correctY.y")), 1, mean)

      return(list(est_ptr = subset(est_ptrs_x_y, select = c("sample", "est_ptr", "coefficient", "pValue", "cor", "correctY")), max_cor = max_cor))
    }
  }

  list(est_ptr = NULL, max_cor = max_cor)
}

#' Compares sample subset x against sample subset y
#'
#' @param est_ptrs_x PTR estimates from sample subset x
#' @param est_ptrs_y PTR estimates from sample subset y
#' @param pipeline_x pipeline for sample subset x
#' @param pipeline_y pipeline for sample subset y
#' @param cor_cutoff the correlation cutoff
#' @param max_cor the max correlation
#' @return a named list including the est_ptr dataframe and a max_cor value
#' \itemize{
#'   \item sample: sample
#'   \item est_ptr: PTR estimate
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
compare_sample_subsets <- function(est_ptrs_x, est_ptrs_y, pipeline_x, pipeline_y, cor_cutoff, max_cor) {
  pipeline_fail_Q <- (length(pipeline_x) == 1) || (length(pipeline_y) == 1)

  if (pipeline_fail_Q) {
    return(list(est_ptr = NULL, max_cor = max_cor))
  }

  sample_intersection <- intersect(est_ptrs_x$sample, est_ptrs_y$sample)
  est_ptrs_int <- merge(est_ptrs_x[est_ptrs_x$sample %in% sample_intersection, ], est_ptrs_y[est_ptrs_y$sample %in% sample_intersection, ], by = "sample")

  minor_sample1 <- cor_diff(est_ptrs_x)
  minor_sample2 <- cor_diff(est_ptrs_y)

  if ((length(minor_sample1) > 0 & length(minor_sample2) > 0)) {
    return(list(est_ptr = NULL, max_cor = max_cor))
  }

  cor_xy <- cor(est_ptrs_int$est_ptr.x, est_ptrs_int$est_ptr.y)

  if (is.na(cor_xy)) {
    return(list(est_ptr = NULL, max_cor = max_cor))
  }

  if (cor_xy > max_cor) {
    max_cor <- cor_xy
  }

  if (cor_xy > cor_cutoff) {
    rownames(est_ptrs_int) <- est_ptrs_int$sample
    only_est_ptrs_int <- subset(est_ptrs_int, select = c("est_ptr.x", "est_ptr.y"))
    est_ptrs_int_pca <- prcomp(only_est_ptrs_int)

    est_ptrs_x$test_ptr <- (est_ptrs_x$est_ptr - mean(est_ptrs_int$est_ptr.x)) / est_ptrs_int_pca$rotation[1, 1]
    est_ptrs_y$test_ptr <- (est_ptrs_y$est_ptr - mean(est_ptrs_int$est_ptr.y)) / est_ptrs_int_pca$rotation[2, 1]
    est_ptrs_y$test_ptr2 <- est_ptrs_y$test_ptr * est_ptrs_int_pca$rotation[1, 1] + mean(est_ptrs_int$est_ptr.x)
    est_ptrs_x$test_ptr2 <- est_ptrs_x$test_ptr * est_ptrs_int_pca$rotation[2, 1] + mean(est_ptrs_int$est_ptr.y)

    if (test_reasonable(est_ptrs_x$test_ptr2, est_ptrs_y$est_ptr) > test_reasonable(est_ptrs_y$test_ptr2, est_ptrs_x$est_ptr) & test_reasonable(est_ptrs_x$test_ptr2, est_ptrs_y$est_ptr) > 0.2) {
      return(list(est_ptr = df_transfer(est_ptrs_y, est_ptrs_x), max_cor = max_cor))
    }
    if (test_reasonable(est_ptrs_x$test_ptr2, est_ptrs_y$est_ptr) < test_reasonable(est_ptrs_y$test_ptr2, est_ptrs_x$est_ptr) & test_reasonable(est_ptrs_y$test_ptr2, est_ptrs_x$est_ptr) > 0.2) {
      return(list(est_ptr = df_transfer(est_ptrs_x, est_ptrs_y), max_cor = max_cor))
    }
  }

  list(est_ptr = NULL, max_cor = max_cor)
}
