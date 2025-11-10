#' Permutation test of pairwise correlations
#'
#' Permute data a given number (n) of times, collecting pairwise correlations
#' and testing them for significance. See [plot_correlation_magnitudes()] and
#' [plot_correlation_counts()] for plotting functions which take the output of
#' this function.
#'
#' @param pca_data dataframe or matrix containing only continuous variables.
#'   (as accepted by the `prcomp` function.)
#' @param n the number of times (integer) to permute that data. **Warning:**
#'   high values will take a long time to compute. Default: 100.
#' @param cor.method method to use for correlations (default = "pearson").
#'   Alternative is "spearman" (see `?cor.test`).
#' @returns object of class `correlation_test`, with attributes:
#' * `$permuted_correlations` A tibble of length n of pairs from the original
#' data, their correlations, and the significance of each correlation (as
#' p-values).
#' * `$actual_correlations` the correlations of each pair of variables
#' in the original data and their significance (as p-values).
#' * `$iterations` the number of permutations carried out.
#' * `$cor_method` the form of correlation used.
#' @importFrom dplyr mutate filter across pull n
#' @importFrom purrr map2 map_dbl
#' @importFrom stats cor.test
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr expand
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom stringr str_c
#' @examples
#'   # get a small sample of random intercepts.
#'   pca_data <- onze_intercepts |>
#'     dplyr::select(-speaker) |>
#'     dplyr::slice_sample(n=10)
#'
#'   # apply correlation test with 5 permutations.
#'   # actual use requires at least 100.
#'   cor_test <- correlation_test(pca_data, n = 5, cor.method = 'pearson')
#'   # Return summary of significant correlations
#'   summary(cor_test)
#'
#'   # use spearman correlation instead.
#'   cor_test_spear <- correlation_test(pca_data, n = 10, cor.method = 'spearman')
#' @export
correlation_test <- function(pca_data, n = 100, cor.method = 'pearson') {

  stopifnot(
    "n must be a positive integer." = (n%%1 == 0 & n >= 1)
  )

  original_pairs <- pca_data |>
    get_pairs() |>
    get_correlations(pca_data = pca_data, cor.method = cor.method)


  permed_pairs <- map(
    1:n,
    ~ pca_data |>
      # permute data
      mutate(
        across(
          .cols = everything(),
          .fns = ~ base::sample(.x, length(.x), replace = FALSE)
        )
      ) %>%
      get_correlations(
        pair_names = get_pairs(.),
        pca_data = .,
        cor.method = cor.method
      )
  )

  permed_pairs <- permed_pairs |>
    bind_rows(.id = "iteration")

  correlation_test <- list(
    permuted_correlations = permed_pairs,
    original_correlations = original_pairs,
    iterations = n,
    cor_method = cor.method
  )

  class(correlation_test) <- "correlation_test"

  correlation_test

}

# Helper function to generate tibble with each pair from the original data
# frame. Each pair appears only once (i.e. these are unordered pairs).
get_pairs <- function(pca_data) {
  original_pairwise <- as_tibble(base::names(pca_data)) |>
    expand(.data$value, value1 = .data$value) |>
    filter(.data$value < .data$value1)
}

# Helper function to collect correlations for each pair and their p-values.
get_correlations <- function(pair_names, pca_data, cor.method) {

  out_data <- pair_names |>
    mutate(
      htest = map2(
        .data$value,
        .data$value1,
        ~ cor.test(
          pca_data |>
            pull(.x),
          pca_data |>
            pull(.y),
          method=cor.method,
          exact=FALSE # Only active is spearman chosen.
        )
      ),
      cor = map_dbl(.data$htest, ~ .x[['estimate']][[1]]),
      cor_p = map_dbl(.data$htest, ~.x[['p.value']])
    ) |>
    select(-"htest")

  out_data

}

#' Summary function for correlation test object. Set alpha to change
#' significance level.
#'
#' Set alpha to change significance level and n_cors to change number of pairwise
#' correlations given.
#' @importFrom dplyr filter across pull n group_by desc summarise arrange
#' @importFrom magrittr %>%
#' @importFrom purrr map2 map_dbl
#' @importFrom stats cor.test
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr expand
#' @importFrom tidyselect everything
#' @importFrom glue glue
#' @importFrom stringr str_c
#' @param object object of class `correlation test`,
#' @param alpha significance level for counting correlation as significant.
#' @param n_cors number of pairwise correlations to list.
#' @param ...	 additional arguments affecting the summary produced.
#' @return a `glue` object.
#' @export
summary.correlation_test <- function(object, alpha = 0.05, n_cors = 5, ...) {

  original_cor_count <- object$original_correlations |>
    filter(.data$cor_p < alpha) |>
    nrow()

  perm_sum <- object$permuted_correlations |>
    filter(.data$cor_p < alpha) |>
    group_by(.data$iteration) |>
    summarise(
      n = n()
    ) |>
    pull(.data$n) |>
    base::summary()

  top_n <- object$original_correlations |>
    arrange(desc(base::abs(.data$cor))) |>
    utils::head(n_cors)


  out_string <- glue(
    "Correlation test results.\n",
    "Count of significant pairwise correlations in original data ",
    "at alpha = {alpha}: ",
    "{original_cor_count}\n",
    "Mean significant pairwise correlations in permuted data (n = {object$iterations}) at alpha = ",
    "{alpha}: ",
    "{perm_sum[['Mean']]}\n",
    "Min = {perm_sum[['Min.']]}, Max = {perm_sum[['Max.']]}.\n\n",
    "Top {n_cors} pairwise correlations in original data:\n",
    "{str_c(top_n$value, ', ', top_n$value1, ': ', base::signif(top_n$cor, digits = 2), collapse = '\n')}"
  )

  out_string

}
