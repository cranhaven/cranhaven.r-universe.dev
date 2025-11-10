#' Run permutation test on PCA analysis.
#'
#' `r lifecycle::badge('superseded')` Permute data fed to PCA a given number of times, collecting the number of
#' significant pairwise correlations in the permuted data and the variances
#' explained for a given number of PCs.
#'
#' This function is now superseded. Use [correlation_test()] for pairwise
#' correlations and [pca_test()] for variance explained and loadings.
#'
#' @param pca_data data fed to the `prcomp` function. Remove non-continuous variables.
#' @param pc_n the number of PCs to collect variance explained from.
#' @param n the number of times to permute that data. **Warning:** high values
#'   will take a long time to compute.
#' @param scale whether the PCA variables should be scaled (default = TRUE).
#' @param cor.method method to use for correlations (default = "pearson").
#'   Alternative is "spearman".
#' @returns object of class `permutation_test`
#' * `$permuted_variances` n x pc_no matrix of variances explained by first
#' pc_no PCs in n permutations of original data.
#' * `$permuted_correlations` list of length n of significant pairwise
#' correlations in n permutations of the data (<= 0.05).
#' * `$actual_variances` pc_n x 2 tibble of variances explained by first pc_n
#' PCs with original data.
#' * `$actual_correlations` the number of significant pairwise correlations (<=
#' 0.05) in the original data.
#' @importFrom dplyr mutate filter across pull
#' @importFrom magrittr %>%
#' @importFrom purrr map2
#' @importFrom stats cor.test
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr expand
#' @importFrom tidyselect everything
#' @examples
#' permutation_test(
#'   onze_intercepts |> dplyr::select(-speaker),
#'   pc_n = 5,
#'   n = 10,
#'   scale = TRUE,
#'   cor.method = 'pearson'
#'  )
#'
#' @export
permutation_test <- function(
  pca_data, pc_n = 5, n=100, scale = TRUE, cor.method = 'pearson'
) {

  # Collect real info
  actual_pca <- stats::prcomp(pca_data, scale = scale)
  actual_explained <- tibble(
    PC = c(glue::glue("PC{seq(1:pc_n)}")),
    variance_explained = (
      actual_pca$sdev^2/base::sum(actual_pca$sdev^2)
    )[1:pc_n]
  )


  # Actual corrs. (TODO: REFACTOR THIS)
  pairwise_correlations <- as_tibble(base::names(pca_data)) %>%
    expand(.data$value, value1 = .data$value) %>%
    filter(.data$value < .data$value1)

  pairwise_correlations <- pairwise_correlations %>%
    mutate(
      cor_p = map2(
        .data$value,
        .data$value1,
        ~ cor.test(
          pca_data %>%
            pull(.x),
          pca_data %>%
            pull(.y),
          method=cor.method,
          exact=FALSE # Only active if spearman chosen.
        )[["p.value"]]
      )
    )

  actual_sig_cors <- pairwise_correlations %>%
    filter(
      .data$cor_p <= 0.05
    ) %>%
    base::nrow()

  # Loop goes here (Accumulator)
  variances_explained <- base::matrix(
    ncol=pc_n,
    nrow=n,
    dimnames = list(
      seq(1:n),
      c(glue::glue("PC{seq(1:pc_n)}"))
    )
  )
  sig_correlations <- rep(0, n)
  for (i in 1:n) {
    permuted <- pca_data %>%
      mutate(
        across(
          .cols = everything(),
          .fns = ~ sample(.x, length(.x))
        )
      )

    # Correlations (REFACTOR - SEE ABOVE!)
    pairwise_correlations <- as_tibble(base::names(permuted)) %>%
      expand(.data$value, value1 = .data$value) %>%
      filter(.data$value < .data$value1)

    pairwise_correlations <- pairwise_correlations %>%
      mutate(
        cor_p = map2(
          .data$value,
          .data$value1,
          ~ cor.test(
            permuted %>%
              pull(.x),
            permuted %>%
              pull(.y),
            method=cor.method,
            exact=FALSE # Only active is spearman chosen.
          )[["p.value"]]
        )
      )

    sig_correlations[i] <- pairwise_correlations %>%
      filter(
        .data$cor_p <= 0.05
      ) %>%
      nrow()

    permuted_pca <- stats::prcomp(permuted, scale = scale)

    variances_explained[i, ] <- (
      permuted_pca$sdev^2 / base::sum(permuted_pca$sdev^2)
    )[1:pc_n]

  }

  permutation_results <- list(
    permuted_variances = variances_explained,
    permuted_correlations = sig_correlations,
    actual_variances = actual_explained,
    actual_correlations = actual_sig_cors
  )

  class(permutation_results) <- "permutation_results"

  permutation_results

}
