#' Plot Scores from Significant PCs Against PCA Input
#'
#' It is sometimes useful to see the relationship between PCs and the raw values
#' of the input data fed into PCA. This function takes the results of running
#' `pca_test`, the scores for each speaker from the pca object, and the raw data
#' fed into the PCA analysis. In the usual model-to-pca analysis pipeline, the
#' resulting plot depicts by-speaker random intercepts for each vowel and an
#' indication of which variables are significantly loaded onto the PCs. It
#' allows the researcher to visualise the strength of the relationship between
#' intercepts and PC scores.
#' @param pca_object Output of `prcomp`.
#' @param pca_data Data fed into `prcomp`. This should not include speaker identifiers.
#' @param pca_test Output of `pca_test`
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols mutate left_join if_else select filter if_else
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_rect facet_grid
#'   theme element_text
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @examples
#' pca_data <- onze_intercepts |> dplyr::select(-speaker)
#' onze_pca <- prcomp(pca_data, scale = TRUE)
#' onze_pca_test <- pca_test(pca_data, n = 5) # Increase n to at least 100 in practice.
#' plot_pc_input(onze_pca, pca_data, onze_pca_test)
#'
#' @return a `ggplot` object.
#' @export
plot_pc_input <- function(pca_object, pca_data, pca_test) {

  pca_data_scores <- pca_data |>
    bind_cols(as_tibble(pca_object$x))

  plot_data <- pca_data_scores |>
    pivot_longer(
      cols = contains("PC"),
      names_to = "PC",
      values_to = "PC_score"
    ) |>
    pivot_longer(
      cols = names(pca_data),
      names_to = "variable",
      values_to = "value"
    ) |>
    separate(
      col = .data$variable,
      into = c("var_1", "var_2"),
      sep = "_",
      remove = FALSE
    ) |>
    mutate(
      vowel = if_else(
        str_detect(.data$var_1, "F[0-9]+"),
        .data$var_2,
        .data$var_1
      ),
      formant_type = if_else(
        str_detect(.data$var_1, "F[0-9]+"),
        .data$var_1,
        .data$var_2
      )
    ) |>
    left_join(
      pca_test$loadings |>
        select(.data$PC, .data$variable, .data$sig_loading),
      by = c("PC", "variable")
    ) |>
    mutate(
      sig_loading = if_else(.data$sig_loading, "Significant", "Not significant")
    ) |>
    filter(
      .data$PC %in% (pca_test$variance |>
        filter(.data$sig_PC) |>
        pull(.data$PC))
    )

  plot_data |>
    ggplot(
      aes(
        x = .data$PC_score,
        y = .data$value,
        colour = .data$sig_loading
      )
    ) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", colour = "black") +
    # geom_rect(aes(xmin = -7, xmax = 7, ymin = -1.2, ymax = 1.2), alpha = 0) +
    facet_grid(PC+formant_type ~ vowel) +
    labs(
      title = "PC Score and PCA Input Values",
      colour = "Loading",
      y = "Input value"
    ) +
    theme(
        strip.text.x = element_text(size = 8)
    )
}
