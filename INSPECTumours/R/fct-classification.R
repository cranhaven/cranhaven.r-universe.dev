#' Function to plot classification over growth rate
#' @param df data frame
#' @param col_palette character palette
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot geom_boxplot facet_wrap labs theme theme_minimal
#' aes geom_point position_jitterdodge scale_color_manual element_text guides
#' guide_axis
#' @importFrom rlang .data
plot_class_gr <- function(df, col_palette) {

  p <- ggplot(df, aes(x = .data$treatment,
                      y = .data$gr,
                      label = .data$animal_id)) +
    facet_wrap(~ study, scales = "free_x") +
    geom_boxplot(fill = "gray90",
                 outlier.shape = NA) +
    geom_point(aes(color = .data$classification),
               position = position_jitterdodge(dodge.width = 0.5)) +
    scale_color_manual(values = col_palette, limits = force) +
    labs(x = "Treatment",
         y = "Growth rate",
         title = "Classification over growth rate") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14)) +
    guides(x = guide_axis(angle = 45))
  return(p)
}

#' Function to hide outliers in boxplots with jitterdodge
#' as suggested
#' @param x plotly object
#'
#' @return plotly object without boxplot outliers
hide_outliers <- function(x) {
  if (x$hoverinfo == "y") {
    x$marker <- list(opacity = 0)
    x$hoverinfo <- NA
  }
  return(x)
}

#' Function to plot classification over tumour volume
#' @param df data frame
#' @param col_palette named vector
#' @param title_name character
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot facet_wrap geom_point geom_line geom_ribbon
#' aes labs scale_color_manual theme_minimal theme element_text
#' @importFrom rlang .data
plot_class_tv <- function(df, col_palette, title_name) {
  p <-
    ggplot(df, aes(
      x = .data$day,
      y = .data$log_tv,
      label = .data$animal_id
    )) +
    facet_wrap(~ treatment, scales = "free",  ncol = 2) +
    geom_point(aes(color = .data$classification)) +
    geom_line(aes(color = .data$classification,
                  group = .data$animal_id)) +
    geom_line(aes(y = .data$estimate),
              size = 1) +
    geom_ribbon(aes(
      y = .data$estimate,
      ymin = .data$lower,
      ymax = .data$upper
    ),
    alpha = 0.2) +
    labs(title = title_name,
         y = "Log10(tumour volume)", x = "Day") +
    scale_color_manual(values = col_palette, limits = force) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14))

  return(p)

}



#' Function to plot waterfall
#' @param df data frame
#' @param col_palette character palette
#' @param study_name string: to show on title
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual labs theme_minimal
#' theme element_text margin
#' @importFrom rlang .data
plot_waterfall <- function(df, col_palette, study_name) {
  p <- ggplot(df) +
    geom_bar(
      aes(
        x = .data$animal_id,
        y = .data$percent_change_from_control_mean,
        fill = .data$classification,
        color = .data$treatment
      ),
      alpha = 0.7,
      size = 1,
      stat = "identity"
    ) +
    scale_fill_manual(values = col_palette, limits = force) +
    labs(
      y = "Change from control mean (%)",
      x = "Treatment - Animal ID",
      title = paste("Waterfall plot for tumour percentage change -",
                    study_name)
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(),
      axis.text.x = element_text(angle = 90, vjust = 0.7),
      plot.title = element_text(size = 14)
    )
  return(p)
}
