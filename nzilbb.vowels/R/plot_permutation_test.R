#' Create plot from [permutation_test()].
#'
#' `r lifecycle::badge('superseded')` Plots results of a permutation test
#' carried out with the [permutation_test()] function. Now use either
#' [correlation_test()] or [pca_test()] and the associated plotting functions.
#'
#' @param permutation_results object of class `permutation_results`.
#' @param violin Determines whether the variances explained are depicted by
#'   distinct violin plots for each PC or by connected lines. the advantage of
#'   lines is that they correctly indicate that values for each PC depend on one
#'   another within a given permutation. That is, if an earlier PC soaks up a
#'   lot of the variation in a data set, then there is less variation left to
#'   explain by subsequent PCs. Default value is `FALSE`.
#' @return `ggplot` object.
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_point geom_violin geom_line
#'   scale_alpha_manual aes labs
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains
#' @importFrom magrittr %>%
#' @importFrom patchwork plot_annotation
#' @examples
#' onze_perm <- permutation_test(
#'   onze_intercepts |> dplyr::select(-speaker),
#'   pc_n = 5,
#'   n = 10,
#'   scale = TRUE,
#'   cor.method = 'pearson'
#'  )
#' plot_permutation_test(onze_perm)
#' @export
plot_permutation_test <- function(permutation_results, violin = FALSE) {

  variance_explained <- permutation_results$permuted_variances %>%
    as_tibble(.name_repair = 'minimal', rownames = "permutation") %>%
    pivot_longer(
      cols = contains('PC'),
      names_to = "PC",
      values_to = "variance_explained"
    ) %>%
    mutate(
      PC = factor(.data$PC, levels = unique(.data$PC))
    )

  if (violin == TRUE) {
    permuted_element <- geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
    actual_element <- geom_point(
      data=permutation_results$actual_variances,
      color="red",
      show.legend = TRUE
    )
    plot_caption <- paste0(
      "Violin plots indicate distribution of results from ",
      nrow(permutation_results$permuted_variances),
      " permutations. \nRed dots indicate values obtained from original data."
    )
  } else {
    permuted_element <- geom_line(colour = "blue", alpha = 0.2)
    actual_element <- geom_line(
      data=permutation_results$actual_variances %>%
        mutate(
          permutation = 'actual'
        ),
      color="red",
      show.legend = TRUE
    )
    plot_caption <- paste0(
      "Violin plots and blue lines indicate distribution of results from ",
      nrow(permutation_results$permuted_variances),
      " permutations. \nRed dots and lines indicate values obtained from original data."
    )
  }

  variance_plot <- variance_explained %>%
    ggplot(
      aes(
        x = .data$PC,
        y = variance_explained * 100, # Convert to percentage
        group = .data$permutation
      )
    ) +
    permuted_element +
    actual_element +
    labs(
      title = paste0(
        "Variance Explained by First ",
        base::nrow(permutation_results$actual_variances),
        " PCs"
      ),
      y = "Variance Explained"
    )

  correlation_plot <- permutation_results$permuted_correlations %>%
    as_tibble() %>%
    ggplot(
      aes(
        x = "",
        y = .data$value
      )
    ) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_point(
      aes(
        x= "",
        y = .data$value
      ),
      data = as_tibble(permutation_results$actual_correlations),
      color = "red"
    ) +
    labs(
      title = "Significant Pairwise Correlations",
      x = NULL,
      y = "Count of Significant Pairwise Correlations"
    )

  correlation_plot + variance_plot + plot_annotation(
    title = "Permutation Test Results",
    subtitle = "Comparison of Permuted and Original Data",
    caption = plot_caption,
    tag_levels = "A"
  )
}
