#' @title
#' A helper function for cerf_gp object
#'
#' @description
#' A helper function to plot cerf_gp object using ggplot2 package.
#'
#' @param object A cerf_gp object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @export
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#'
autoplot.cerf_gp <- function(object, ...) {


  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  # extract data
  tmp_data <- data.frame(w_vals = object$posterior$w,
                         mean_vals = object$posterior$mean,
                         sd_vals = object$posterior$sd)

  g1 <- ggplot2::ggplot(tmp_data) +
       ggplot2::geom_ribbon(ggplot2::aes(.data$w_vals,
                                y = .data$mean_vals,
                                ymin = .data$mean_vals - 1.96 * .data$sd_vals,
                                ymax = .data$mean_vals + 1.96 * .data$sd_vals),
                                fill = "blue", alpha = 0.25) +
        ggplot2::geom_line(ggplot2::aes(.data$w_vals, .data$mean_vals),
                           color = "blue", size = 1) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle("Estimated CERF (gp)") +
        ggplot2::labs(subtitle = "+ credible band (1.96sd)") +
        ggplot2::xlab("Exposure level") +
        ggplot2::ylab("Population average counterfactual outcome")


  balance <- data.frame(original = object$cb_org,
                        adjusted = object$cb)

  balance$covar_label <- row.names(balance)

  # sort data.frame based on original data correlation values
  balance <- balance[order(balance$original), ]
  covar_label <- balance$covar_label
  row.names(balance) <- NULL
  n_cov <- length(balance$original)
  m_balance <- reshape(balance,
                       direction = "long",
                       varying = 1:2,
                       v.names = "value",
                       times = c("original", "adjusted"),
                       idvar = "covar_label")

  rownames(m_balance) <- NULL
  m_balance$covariates <- rep(seq(1, n_cov, 1), 2)
  colnames(m_balance)[colnames(m_balance) == "time"] <- "Data"

  default_gg_title <- "Covariate balance"
  default_gg_labs <- list(x = "Absolute weighted correlation", y = "Covariates")

  color_var <- c("#1E88E5", "#FFC107")

  g2 <- ggplot2::ggplot(data = m_balance,
                       ggplot2::aes(x = .data$value,
                                    y = .data$covariates,
                                    color = .data$Data)) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_y_discrete(limit = factor(1:n_cov), labels = covar_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(x = default_gg_labs$x,
                  y = default_gg_labs$y) +
    ggplot2::ggtitle(default_gg_title) +
    ggplot2::scale_color_manual(values = color_var)

  p <- cowplot::plot_grid(g1, g2, ncol = 2)

  return(p)
}


#' @title
#' Extend generic plot functions for cerf_gp class
#'
#' @description
#' A wrapper function to extend generic plot functions for cerf_gp class.
#'
#' @param x  A cerf_gp object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side
#' effects.
#'
#' @export
#'
plot.cerf_gp <- function(x, ...) {
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}

#' @title
#' A helper function for cerf_nngp object
#'
#' @description
#' A helper function to plot cerf_nngp object using ggplot2 package.
#'
#' @param object A cerf_nngp object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @export
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#'
autoplot.cerf_nngp <- function(object, ...) {

  gg_labs <- NULL
  gg_title <- "Exposure Rate Function"

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  # extract data
  tmp_data <- data.frame(w_vals = object$posterior$w,
                         mean_vals = object$posterior$mean,
                         sd_vals = object$posterior$sd)

  g1 <- ggplot2::ggplot(tmp_data) +
        ggplot2::geom_ribbon(ggplot2::aes(.data$w_vals,
                                y = .data$mean_vals,
                                ymin = .data$mean_vals - 1.96 * .data$sd_vals,
                                ymax = .data$mean_vals + 1.96 * .data$sd_vals),
                                fill = "#FC4E07", alpha = 0.25) +
        ggplot2::geom_line(ggplot2::aes(.data$w_vals, .data$mean_vals),
                           color = "#FC4E07",
                           size = 1) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle("Estimated CERF (nngp)") +
        ggplot2::labs(subtitle = "+ credible band (1.96sd)") +
        ggplot2::xlab("Exposure level") +
        ggplot2::ylab("Population average counterfactual outcome")

  balance <- data.frame(original = object$cb_org,
                        adjusted = object$cb)

  balance$covar_label <- row.names(balance)

  # sort data.frame based on original data correlation values
  balance <- balance[order(balance$original), ]
  covar_label <- balance$covar_label
  row.names(balance) <- NULL
  n_cov <- length(balance$original)
  m_balance <- reshape(balance,
                       direction = "long",
                       varying = 1:2,
                       v.names = "value",
                       times = c("original", "adjusted"),
                       idvar = "covar_label")

  rownames(m_balance) <- NULL
  m_balance$covariates <- rep(seq(1, n_cov, 1), 2)
  colnames(m_balance)[colnames(m_balance) == "time"] <- "Data"

  default_gg_title <- "Covariate balance"
  default_gg_labs <- list(x = "Absolute weighted correlation", y = "Covariates")

  color_var <- c("#1E88E5", "#FFC107")

  g2 <- ggplot2::ggplot(data = m_balance,
                        ggplot2::aes(x = .data$value,
                                     y = .data$covariates,
                                     color = .data$Data)) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_y_discrete(limit = factor(1:n_cov), labels = covar_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(x = default_gg_labs$x,
                  y = default_gg_labs$y) +
    ggplot2::ggtitle(default_gg_title) +
    ggplot2::scale_color_manual(values = color_var)

  p <- cowplot::plot_grid(g1, g2, ncol = 2)

  return(p)
}


#' @title
#' Extend generic plot functions for cerf_nngp class
#'
#' @description
#' A wrapper function to extend generic plot functions for cerf_nngp class.
#'
#' @param x  A cerf_nngp object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side
#' effects.
#'
#' @export
#'
plot.cerf_nngp <- function(x, ...) {
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}
