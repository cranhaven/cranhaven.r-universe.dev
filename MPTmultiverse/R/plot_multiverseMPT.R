# plot_results <- function (results, save = TRUE, write.csv = TRUE){
#
#   shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
#
#   prefix <- paste0(gsub("\\.eqn", "", results$model[1]), "_",
#                             gsub("\\.", "_", paste0(results$dataset[1],"_")))
#
#   if (write.csv){
#     readr::write_csv(tidyr::unnest(results, est_group), paste0(prefix,"estimates.csv"))
#     readr::write_csv(tidyr::unnest(results, gof), paste0(prefix,"gof.csv"))
#   }
#
#
#   dd <- ggplot2::position_dodge(w = .75)
#   gg_est1 <- tidyr::unnest(results, est_group) %>%
#     ggplot2::ggplot(ggplot2::aes(y = est, x = parameter,
#                col=interaction(method, pooling,package),
#                shape=interaction(method, pooling, package))) +
#     ggplot2::facet_grid(.~condition) +
#     ggplot2::geom_errorbar(ggplot2::aes(ymin = est-se, ymax = est+se), position = dd,
#                   width = 0.4)+
#     ggplot2::geom_point(position = dd) + ylim(0,1) +
#     ggplot2::scale_shape_manual(values=shapes) +
#     ggplot2::theme_bw()
#   plot(gg_est1)
#   if(save) ggplot2::ggsave(paste0(prefix,"estimates.pdf"), gg_est1, h = 4.5, w = 10)
#
#
#   res_between <-  tidyr::unnest(results, test_between)
#   if (nrow(res_between) > 0){
#
#     if (write.csv) readr::write_csv(tidyr::unnest(results, test_between), paste0(prefix,"test_between.csv"))
#
#     gg_est2 <- ggplot2::ggplot(
#       res_between
#       , ggplot2::aes(
#         y = est_diff
#         , x = parameter
#         , col = interaction(method, pooling, package)
#         , shape = interaction(method, pooling, package)
#       )
#     ) +
#     ggplot2::facet_grid(condition2~condition1) +
#     ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_0.025, ymax = ci_0.975),
#                   position = dd, width = 0.5) +
#     ggplot2::geom_point(position = dd) + ylim(-1,1) +
#     ggplot2::scale_shape_manual(values=shapes) +
#     ggplot2::theme_bw() +
#     ggplot2::geom_hline(yintercept = 0, lty = 2)
#
#     if(save) ggplot2::ggsave(paste0(prefix,"test_between.pdf"), gg_est2, h = 4.5, w = 8)
#   }
#
#
#   gg_gof1 <-  tidyr::unnest(results, gof) %>%
#     # filter(focus == "mean") %>%
#     ggplot2::ggplot(ggplot2::aes(y = p,
#                x = interaction(method, pooling, package))) +
#     ggplot2::geom_point() + ylim(0, 1) +
#     ggplot2::geom_hline(yintercept = .05, lty = 2)+
#     ggplot2::theme_bw() + ggplot2::coord_flip() +
#     ggplot2::facet_wrap(~focus) +
#     ggplot2::ggtitle("Goodness of fit")
#   plot(gg_gof1)
#   if(save) ggplot2::ggsave(paste0(prefix,"gof.pdf"), gg_gof1, h = 4, w = 6)
#
#
#   if (nrow(res_between) > 0){
#     if (write.csv) readr::write_csv(tidyr::unnest(results, gof_group), paste0(prefix,"gof_group.csv"))
#
#     gg_gof2 <- tidyr::unnest(results, gof_group) %>%
#       ggplot(ggplot2::aes(y = p,
#                  x = interaction(method, pooling, package),
#                  col = condition)) +
#       ggplot2::geom_point() + ylim(0, 1) +
#       ggplot2::geom_hline(yintercept = .05, lty = 2)+
#       ggplot2::theme_bw() +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~focus) +
#       ggplot2::ggtitle("Goodness of fit")
#     plot(gg_gof2)
#     if(save) ggplot2::ggsave(paste0(prefix,"gof_group.pdf"), gg_gof2, h = 4, w = 8)
#   }
# }


#' Plot multiverseMPT
#'
#' Plot the results from a multiverse MPT analysis.
#'
#' @param x An object of class \code{multiverseMPT}.
#' @param which Character. Which information should be plotted? Possible
#' values are
#' \code{"est"} for parameter estimates,
#' \code{"test_between"} for between-subjects comparisions,
#' \code{"gof1"} for overall goodness-of-fit statistics, and
#' \code{"gof2"} for group-wise goodness-of-fit statistics.
#' @param save Logical. Indicates whether the plot should also be saved as a .pdf file.
#' @param ... ignored.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom graphics plot
#' @export

plot.multiverseMPT <- function(x, which = "est", save = FALSE, ...){

  args <- list(...)

  if(is.null(args$shapes)) {
    shapes <- seq_len(nrow(x))
  } else {
    shapes <- args$shapes
  }

  prefix <- paste0(gsub("\\.eqn", "", x$model[1]), "_",
                   gsub("\\.", "_", paste0(x$dataset[1],"_")))

  if("est" %in% which) {
    gg_est1 <- plot_est(x, shapes = shapes)
    if(save) ggplot2::ggsave(paste0(prefix, "estimates.pdf"), gg_est1, h = 4.5, w = 10)
    return(gg_est1)
  }

  if("test_between" %in% which){
    gg_est2 <- plot_test_between(x, shapes = shapes)
    if(save) ggplot2::ggsave(paste0(prefix,"test_between.pdf"), gg_est2, h = 4.5, w = 8)
    return(gg_est2)
  }

  if("gof1" %in% which) {
    gg_gof1 <- plot_gof1(x)
    if(save) ggplot2::ggsave(paste0(prefix,"gof.pdf"), gg_gof1, h = 4, w = 6)
    return(gg_gof1)
  }

  if("gof2" %in% which) {
    gg_gof2 <-plot_gof2(x)
    if(save) ggplot2::ggsave(paste0(prefix, "gof_group.pdf"), gg_gof2, h = 4, w = 8)
    return(gg_gof2)
  }
  if("gof" %in% which) {
    gg_gof <- plot_gof(x)
    if(save) ggplot2::ggsave(paste0(prefix, "gof_group.pdf"), gg_gof, h = 4, w = 7)
    return(gg_gof)
  }
}



#' @keywords internal

plot_est <- function(x, shapes, ...) {

  args <- list(...)

  dd <- ggplot2::position_dodge(width = .75)

  x %>%
    tidyr::unnest(.data$est_group) %>%
    dplyr::mutate(approach = interaction(.data$method, .data$pooling, .data$package, sep = " ")) %>%
    ggplot2::ggplot() +
    ggplot2::aes_(y = ~ est, x = ~ parameter, col = ~ approach, shape = ~ approach) +
    ggplot2::facet_grid(facets = ". ~ condition") +
    ggplot2::geom_errorbar(
      ggplot2::aes_(ymin = ~ci_0.025, ymax = ~ci_0.975)
      , position = dd
      , width = 0.4
    ) +
    ggplot2::geom_point(position = dd) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    # ggplot2::ylim(0, 1) +
    ggplot2::scale_shape_manual(values = shapes)
}



#' @keywords internal

plot_test_between <- function(x, shapes, ...) {

  test_between <- x %>%
    tidyr::unnest(.data$test_between)

  if(nrow(test_between) == 0) {
    warning(
      "No between-subjects conditions present in your results. Therefore, no between-subjects comparisons were plotted."
      , call. = FALSE
    )
    return(ggplot2::ggplot())
  }

  dd <- ggplot2::position_dodge(width = .75)

  test_between %>%
    dplyr::mutate(approach = interaction(.data$method, .data$pooling, .data$package, sep = " ")) %>%
    ggplot2::ggplot() +
    ggplot2::aes_(y = ~ est_diff, x = ~ parameter, col = ~ approach, shape = ~ approach) +
    ggplot2::facet_grid("condition2 ~ condition1") +
    ggplot2::geom_errorbar(
      ggplot2::aes_(ymin = ~ ci_0.025, ymax = ~ ci_0.975)
      , position = dd
      , width = 0.4
    ) +
    ggplot2::geom_point(position = dd) + ggplot2::ylim(-1, 1) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::geom_hline(yintercept = 0, lty = 2)
}



#' @keywords internal

plot_gof1 <- function(x, ...) {

  x %>%
    tidyr::unnest(.data$gof) %>%
    dplyr::mutate(approach = interaction(.data$method, .data$pooling, .data$package, sep = " ")) %>%
    ggplot2::ggplot() +
    ggplot2::aes_(y = ~ p, x = ~ approach) +
    ggplot2::geom_point() +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_hline(yintercept = .05, lty = 2)+
    ggplot2::coord_flip() +
    ggplot2::facet_wrap( ~ focus) +
    ggplot2::ggtitle("Goodness of fit")
}



#' @keywords internal

plot_gof2 <- function(x, ...) {

  gof_group <- x %>%
    tidyr::unnest(.data$gof_group)

  if(nrow(gof_group) == 0) stop("No between-subjects conditions present in your results.")

  gof_group %>%
    dplyr::mutate(approach = interaction(.data$method, .data$pooling, .data$package, sep = " ")) %>%
    ggplot2::ggplot() +
    ggplot2::aes_(y = ~ p, x = ~ approach, col = ~ condition) +
    ggplot2::geom_point() +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_hline(yintercept = .05, lty = 2)+
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ focus) +
    ggplot2::ggtitle("Goodness of fit")
}



#' @keywords internal

plot_gof <- function(x, ...) {
  gof_group <- x %>%
    tidyr::unnest(.data$gof_group)

  n_conditions <- length(unique(gof_group$condition))

  if(n_conditions > 1) {
    plot_gof2(x)
  } else {
    plot_gof1(x)
  }
}



