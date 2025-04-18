#' A convenient function for KS test of uniform distribution
#' @param x a vector without NA
#' @return the p value of KS test
ks <- function(x) {
  ks_result <- ks.test(x, "punif", min(x, na.rm = TRUE), max(x, na.rm = TRUE))

  ks_result$p.value
}

#' A function to remove outlier contigs using KS test
#' @param sort_values a vector of sorted values
#' @return a vector with all values following a uniform distribution
select_by_ks_test <- function(sort_values) {
  len <- length(sort_values)
  if (len < 10) {
    return(c(0, 0, FALSE))
  }

  ks_result <- ks(sort_values)

  if (sort_values[2] - sort_values[1] > sort_values[len] - sort_values[len - 1]) {
    if (ks_result < 0.05) {
      return(select_by_ks_test(sort_values[2:len]))
    } else {
      ks_next <- ks(sort_values[2:len])
      if (ks_next > ks_result + 0.1) {
        return(select_by_ks_test(sort_values[2:len]))
      } else {
        return(c(sort_values[1], sort_values[len], TRUE))
      }
    }
  } else {
    if (ks_result < 0.05) {
      return(select_by_ks_test(sort_values[1:len - 1]))
    } else {
      ks_next <- ks(sort_values[2:len])
      if (ks_next > ks_result + 0.1) {
        return(select_by_ks_test(sort_values[1:len - 1]))
      } else {
        return(c(sort_values[1], sort_values[len], TRUE))
      }
    }
  }
}

#' A convenient function for ordinary linear regression on two vectors
#' @param x first vector
#' @param y second vector
#' @return the coefficient and p value of linear regression
#'
#' @importFrom stats anova lm
lm_column <- function(x, y) {
  lm_model <- lm(x ~ y)
  anova_model <- anova(lm_model)
  c(lm_model$coefficients[2], anova_model$`Pr(>F)`)
}

#' A function for sample filtration
#' Input requirements: 1. have values in more than half of the contigs 2. average log2(cov) > 0 in all these contigs
#' @param Z a matrix
#' @param avg_cutoff threshold of average
#' @param cutoff_ratio threshold of ratio
#' @return the coefficient and p value of linear regression
filter_sample <- function(Z, avg_cutoff, cutoff_ratio) {
  z_summary <- aggregate(correctY ~ (sample), Z, FUN = function(x) c(sum(x), length(x)))

  contig_level <- length(unique(Z$contig))
  samples_filtered <- z_summary[z_summary$correctY[, 1] >= avg_cutoff * contig_level & z_summary$correctY[, 2] >= cutoff_ratio * contig_level, ]$sample

  samples_filtered
}

#' Determine the majority orientation of the input PTR estimates correlations
#' @param Z a vector of values
#' @return a minor subset, where each value has the same orientation
cor_diff <- function(Z) {
  pos_cor <- Z[Z$cor > 0, ]$sample
  neg_cor <- Z[Z$cor < 0, ]$sample
  if (length(pos_cor) > length(neg_cor)) {
    return(neg_cor)
  } else {
    return(pos_cor)
  }
}

#' A function for reshape to facilitate PCA, removing all contigs with missing values for designated samples
#' @param samples_filtered a vector of samples
#' @param Z a matrix of coverage
#' @return a reshaped matrix of coverage
reshape_filtered <- function(samples_filtered, Z) {
  contig <- correctY <- NULL

  z_filtered <- subset(Z, sample %in% samples_filtered, select = c(sample, contig, correctY))
  z_filtered_wide <- reshape2::dcast(subset(z_filtered, select = c("sample", "contig", "correctY")), contig ~ sample, value.var = "correctY")

  z_filtered_wide_na <- apply(subset(z_filtered_wide, select = -c(contig)), 1, function(x) length(x[is.na(x)]))
  names(z_filtered_wide_na) <- z_filtered_wide$contig

  z_filtered_wide_na_nz <- subset(subset(z_filtered_wide, contig %in% names(z_filtered_wide_na[z_filtered_wide_na == 0])), select = -c(contig))

  row.names(z_filtered_wide_na_nz) <- names(z_filtered_wide_na[z_filtered_wide_na == 0])

  z_filtered_wide_na_nz
}

#' A function for data frame integration
#' @param x first data frame
#' @param y second data frame
#' @param i 'sample' column
#' @return a data frame with the other column as mean or max of that in the original two
consist_transfer <- function(x, y, i) {
  z <- merge(x, y, by = "sample", all.x = TRUE, all.y = TRUE)
  if (i == 1) {
    est_ptr <- apply(subset(z, select = 2:3), 1, function(x) mean(abs(x), na.rm = TRUE))
  } else if (i == 2) {
    est_ptr <- apply(subset(z, select = 2:3), 1, function(x) max(abs(x), na.rm = TRUE))
  }
  names(est_ptr) <- z$sample

  est_ptr
}

#' A function for data frame transfer
#' @param x first data frame with six columns
#' @param y second data frame with six columns
#' @return a data frame with the same six columns but integrated info
df_transfer <- function(x, y) {
  est_ptr <- test_ptr2 <- coefficient <- pValue <- correctY <- NULL

  xy <- data.frame(
    "sample" = sort(union(x$sample, y$sample), method = "shell"),
    "est_ptr" = consist_transfer(subset(x, select = c(sample, est_ptr)), subset(y, select = c(sample, test_ptr2)), 1),
    "coefficient" = consist_transfer(subset(x, select = c(sample, coefficient)), subset(y, select = c(sample, coefficient)), 1),
    "pValue" = consist_transfer(subset(x, select = c(sample, pValue)), subset(y, select = c(sample, pValue)), 2),
    "cor" = consist_transfer(subset(x, select = c(sample, cor)), subset(y, select = c(sample, cor)), 1),
    "correctY" = consist_transfer(subset(x, select = c(sample, correctY)), subset(y, select = c(sample, correctY)), 1)
  )

  xy
}

#' A function to test whether the result is reasonable
#' @param a first vector of values
#' @param b second vector of values
#' @return the test result
test_reasonable <- function(a, b) {
  c <- c(a, b)
  if (min(c) >= 1) {
    return(min(c) / max(c))
  } else {
    return(0 - length(c[c < 1]))
  }
}

#' A function to return the first dimension of PCA on an input matrix
#' @param X a matrix to undergo PCA
#' @return first dimension of the PCA results
#'
#' @importFrom stats prcomp
contig_pca <- function(X) {
  contig_pca <- prcomp(X) # take first component (PC1)

  data.frame("contig" = rownames(contig_pca$x), "PC1" = contig_pca$x[, "PC1"])
}
