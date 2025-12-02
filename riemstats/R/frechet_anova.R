#' Frechet ANOVA Test Statistic
#'
#' Computes the Frechet ANOVA test statistic and its p-value for a given super-sample object.
#'
#' @param super_sample An object of class CSuperSample (see riemtan package)
#'
#' @return A list with two elements:
#' \describe{
#'   \item{statistic}{The Frechet ANOVA test statistic}
#'   \item{p_value}{The p-value from the chi-squared test}
#' }
#' @export
frechet_anova <- function(super_sample) {
  if (!inherits(super_sample, "CSuperSample")) {
    stop("Argument 'super_sample' must be an object of class 'CSuperSample'.")
  }

  # Check for minimum number of groups
  if (length(super_sample$list_of_samples) < 2) {
    stop("CSuperSample must contain at least 2 groups for ANOVA analysis")
  }

  k <- super_sample$list_of_samples |> length()
  n <- super_sample$sample_size

  group_stats <- super_sample$list_of_samples |>
    purrr::map(\(sample) {
      sample$compute_dists()
      sample$distances
    }) |>
    purrr::map(\(vec_of_dists) {
      group_v <- mean(vec_of_dists^2)
      group_sig_2_raw <- mean(vec_of_dists^4) - mean(vec_of_dists^2)^2

      # Cap group_sig_2 to prevent numerical instability from near-zero variances
      # Minimum variance is set to 10% of (mean squared distance)^2
      epsilon <- 0.1
      group_sig_2 <- max(group_sig_2_raw, epsilon * group_v^2)

      list(
        group_sig_2 = group_sig_2,
        group_v = group_v,
        group_sample_size = length(vec_of_dists)
      )
    })

  group_sig_2s <- group_stats |> purrr::map_dbl(\(stats) stats$group_sig_2)
  group_vs <- group_stats |> purrr::map_dbl(\(stats) stats$group_v)

  super_sample$gather()
  super_sample$full_sample$compute_dists()
  pooled_v <- mean(super_sample$full_sample$distances^2)

  group_rel_sizes <- group_stats |>
    purrr::map_dbl(\(stats) stats$group_sample_size) |>
    (\(x) x / super_sample$full_sample$sample_size)()

  F_stat <- pooled_v - sum(group_rel_sizes * group_vs)

  # Vectorized computation of Un
  idx_mat <- which(upper.tri(matrix(0, k, k)), arr.ind = TRUE)
  j_idx <- idx_mat[, 1]
  l_idx <- idx_mat[, 2]
  Un <- sum(
    (group_rel_sizes[j_idx] * group_rel_sizes[l_idx]) /
      (group_sig_2s[j_idx] * group_sig_2s[l_idx]) *
      (group_vs[j_idx] - group_vs[l_idx])^2
  )

  term1 <- (n * Un) / sum(group_rel_sizes / group_sig_2s)
  term2 <- (n * (F_stat^2)) / sum((group_rel_sizes^2) * group_sig_2s)
  thestat <- term1 + term2

  # compute p-value
  p_value <- stats::pchisq(thestat, df = (k - 1), lower.tail = FALSE)

  list(
    statistic = thestat,
    p_value = p_value
  )
}
