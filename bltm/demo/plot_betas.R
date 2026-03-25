#' Plot adjusted betas
#'
#' Plot adjusted betas to easily compare with simulated data
#'
#' @param a result of [`ltm_mcmc`],
#' @param betas which betas (indices) to plot. Defaults to all betas.
#' @param scales should we use free scales? Like in `ggplot2` package.
#' @param real_values simulated data from [`ltm_sim`] to compare, if it exists.
#'
#' @import ggplot2
#'
#' @export
plot_betas <- function(a, betas = "all", scales = "free_y", real_values = NULL) {
  pegar_stat <- function(prob = .5, a, betas = "all") {
    nm <- stringr::str_subset(dimnames(a)[[2]], "beta\\[[^,]+,[0-9]")
    if (betas[1] == "all") {
      betas <- as.numeric(unique(stringr::str_extract(nm, "[0-9]+(?=\\])")))
    }
    a[,nm] %>%
      apply(2, stats::quantile, prob) %>%
      as.numeric() %>%
      purrr::set_names(nm) %>%
      tibble::enframe() %>%
      dplyr::mutate(p = as.numeric(stringr::str_extract(name, "[0-9]+(?=\\])"))) %>%
      dplyr::filter(p %in% betas) %>%
      dplyr::mutate(id = as.numeric(stringr::str_extract(name, "(?<=\\[)[0-9]+")))
  }
  d <- a[,stringr::str_detect(colnames(a), "d\\[")] %>%
    apply(2, mean) %>%
    tibble::enframe() %>%
    dplyr::mutate(p = as.numeric(stringr::str_extract(name, "[0-9]+(?=\\])"))) %>%
    dplyr::filter(p %in% betas) %>%
    dplyr::select(p, d_mean = value)

  d_betas <- purrr::map_dfr(c(.1, .5, .9), pegar_stat, a, betas = betas, .id = "prob") %>%
    tidyr::spread(prob, value, sep = "_") %>%
    dplyr::inner_join(d, "p")

  p <- d_betas %>%
    ggplot(aes(x = id, y = prob_2)) +
    geom_line(linetype = 2) +
    geom_ribbon(aes(ymin = prob_1, ymax = prob_3), alpha = .2) +
    geom_hline(aes(yintercept = c(-1,1) * d_mean), linetype = 2) +
    facet_wrap(~factor(p), scales = scales)

  if (!is.null(real_values)) {
    betas_reais <- real_values$mb %>%
      as.data.frame() %>%
      tibble::as_tibble() %>%
      tibble::rowid_to_column("id") %>%
      tidyr::gather(p, prob_2, -id) %>%
      dplyr::mutate(p = readr::parse_number(p), d_mean = .4) %>%
      dplyr::filter(p %in% betas)
    p <- p +
      geom_line(data = betas_reais) +
      geom_hline(aes(yintercept = c(-1,1) * d_mean), data = betas_reais)
  }
  p
}
