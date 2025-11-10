#' Plot PC loadings in vowel space
#'
#' Plot loadings from a PCA analysis carried out on vocalic data. Vowel
#' positions mean values are at the mean with arrows indicating loadings.
#' Loadings are
#' multiplied by the standard deviation, by vowel, of the initial input data.
#' This is OK for getting a quick, intuitive, interpretation of what the PCs
#' mean in the vowel space. When using a model-to-PCA pipeline, it is not
#' recommended to use these plots directly in publications as the models should
#' more reliably control variation in vocalic readings than taking the standard
#' mean and standard deviation.
#'
#' @param vowel_data A dataframe whose first four columns are speaker ids,
#' vowel ids, F1 values, and F2 values.
#' @param pca_obj The result of a call to `prcomp()`, `princomp()` or `pca_test()`.
#' @param pc_no An integer, indicating which PC to plot (default is PC1).
#' @param is_sig A boolean, indicating whether only 'significant' loadings,
#'   according to `pca_test` should be plotted (only works with objects of class
#'   `pca_test_results`).
#'
#' @importFrom dplyr group_by summarise across rename left_join select mutate
#'  ungroup
#' @importFrom tidyselect matches any_of
#' @importFrom tidyr pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 geom_segment geom_label scale_x_reverse scale_y_reverse
#'  coord_fixed theme arrow expansion labs
#' @importFrom grid unit
#' @importFrom rlang .data `:=`
#'
#' @return a `ggplot` object.
#' @export
#'
#' @examples
#'   onze_pca <- prcomp(onze_intercepts |> dplyr::select(-speaker), scale=TRUE)
#'   # Default is to plot PC1
#'   plot_pc_vs(onze_vowels, onze_pca)
#'   # Or plot another PC with `pc_no`
#'   plot_pc_vs(onze_vowels, onze_pca, pc_no = 3)
plot_pc_vs <- function(vowel_data, pca_obj, pc_no = 1, is_sig = FALSE) {

  stopifnot("`pc_no` must have a numeric value." = is.numeric(pc_no))
  F1_col_name <- names(vowel_data)[[3]]
  F2_col_name <- names(vowel_data)[[4]]

  if (inherits(pca_obj, "prcomp")) {
    loadings_var <- "rotation"
  } else if (inherits(pca_obj, "princomp")) {
    loadings_var <- "loadings"
  }

  vowel_col_name <- names(vowel_data)[[2]]

  vowel_means <- vowel_data |>
    group_by(.data[[vowel_col_name]]) |>
    summarise(
      across(matches('F[0-9]'), list(mean=mean, sd=stats::sd))
    )

  if (inherits((pca_obj), "pca_test_results")) {

    PC_loadings <- pca_obj$loadings |>
      ungroup() |>
      filter(.data$PC == paste0("PC", pc_no)) |>
      select(
        'variable', 'loading', 'sig_loading'
      )

  } else {

    PC_loadings <- pca_obj[[loadings_var]][, pc_no] |>
      as_tibble(rownames = "variable") |>
      rename(loading = 'value')

  }

  PC_directions <- PC_loadings |>
    mutate(
      !!vowel_col_name := str_extract(
        .data$variable,
        # Assumption behind regex: vowel names are either FX_WELLS SET or
        # WELLS SET_FX. We get strings of A-Za-z which either preceed or follow
        # a '_'.
        "(?:^|_)([A-Za-z\\:]+)(?:$|_)",
        group=1
      ),
      formant = str_extract(.data$variable, "(?:^|_)(F[0-9])(?:$|_)", group=1)
    )

  if (is_sig & inherits(pca_obj, "pca_test_results")) {
    PC_directions <- PC_directions |>
      mutate(
        loading = if_else(.data$sig_loading == FALSE, 0, .data$loading)
      )
  }

  PC_directions <- PC_directions |>
    select(-any_of(c('sig_loading', 'variable'))) |>
    pivot_wider(names_from = "formant", values_from = "loading") |>
    rename(
      F1_PC = 'F1',
      F2_PC = 'F2'
    )

  vowel_means <- vowel_means |>
    left_join(PC_directions, by = vowel_col_name) |>
    mutate(
      F1_max = .data[[paste0(F1_col_name, '_mean')]] +
        .data[[paste0(F1_col_name, '_sd')]] * .data$F1_PC,
      F2_max = .data[[paste0(F2_col_name, '_mean')]] +
        .data[[paste0(F2_col_name, '_sd')]] * .data$F2_PC
    )

  vowel_means |>
    ggplot(
      aes(
        x = .data[[paste0(F2_col_name, '_mean')]],
        y = .data[[paste0(F1_col_name, '_mean')]],
        colour = .data[[vowel_col_name]],
        label = .data[[vowel_col_name]]
      )
    ) +
    geom_segment(
      aes(
        xend = .data$F2_max,
        yend = .data$F1_max
      ),
      arrow = arrow(length = unit(2, "mm")),
      linewidth = 1
    ) +
    geom_label(size=3, alpha=0.8) +
    scale_x_reverse(expand = expansion(mult = 0.1)) +
    scale_y_reverse(expand = expansion(mult = 0.1)) +
    labs(
      title = paste0("PC", pc_no),
      x = F2_col_name,
      y = F1_col_name
    ) +
    theme(legend.position = "none")
}

