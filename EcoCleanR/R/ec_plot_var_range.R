#' Plot cleaned data overlay overall occurrence data to demonstrate accepted ranges of spatial and non-spatial attributes
#'
#' @param data data table which even has outlier data points
#' @param summary_df summmary output of final cleaned data, after executing function ec_var_summary
#' @param latitude default set to "decimalLatitude"
#' @param longitude default set to "decimalLongitude"
#' @param env_layers list of environmental variables
#'
#' @return A plot which shows spatial and environmental variables with the acceptable range for species habitability
#' @import ggplot2
#' @import patchwork
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr all_of
#' @export
#'
#' @examples
#' data <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9, -116.5),
#'   decimalLatitude = c(32.9, 33.5, 31.9, 32.4),
#'   temperature_mean = c(12, 13, 14, 11),
#'   temperature_min = c(9, 6, 10, 10),
#'   temperature_max = c(14, 16, 18, 17),
#'   flag_outlier = c(0, 0.5, 1, 0.7)
#' ) # this data table has data points which was considered as outliers
#'
#' data_x <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.5),
#'   decimalLatitude = c(32.9, 33.5, 32.4),
#'   temperature_mean = c(12, 13, 11),
#'   temperature_min = c(9, 6, 10),
#'   temperature_max = c(14, 16, 17),
#'   flag_outlier = c(0, 0.5, 0.7)
#' )
#' # cleaned data base after removing outliers >x probability.
#' # in this example, removed data points >0.7 probability to be
#' # considering outliers
#'
#'
#' env_layers <- c("temperature_mean", "temperature_min", "temperature_max")
#' summary_df <- ec_var_summary(data_x,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   env_layers
#' )
#' # this is the final cleaned data table which
#' # will be used to derive summary of acceptable niche
#'
#' ec_plot_var_range(data,
#'   summary_df,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   env_layers
#' )
#'
ec_plot_var_range <- function(data,
                              summary_df,
                              latitude = "decimalLatitude",
                              longitude = "decimalLongitude",
                              env_layers) {
  range_data <- summary_df %>%
    mutate(ymin = .data$Min, ymax = .data$Max)
  mydata_long <- data %>%
    tidyr::pivot_longer(
      cols = tidyr::all_of(c(latitude, longitude, env_layers)),
      names_to = "variable",
      values_to = "value"
    )

  P1 <- ggplot() +
    geom_errorbar(
      data = range_data %>% filter(.data$variable == latitude),
      aes(x = .data$variable, ymin = .data$ymin, ymax = .data$ymax),
      width = 0.15, color = "blue", linewidth = 1.0
    ) +
    geom_point(
      data = mydata_long %>% filter(.data$variable == latitude),
      aes(x = .data$variable, y = .data$value),
      size = 2, shape = 1, color = "black", stroke = 1
    ) +
    ggtitle("Latitude") +
    theme_classic() +
    theme(axis.text = element_text(size = 14))

  P2 <- ggplot() +
    geom_errorbar(
      data = range_data %>% filter(.data$variable == longitude),
      aes(x = .data$variable, ymin = .data$ymin, ymax = .data$ymax),
      width = 0.15, color = "blue", linewidth = 1.0
    ) +
    geom_point(
      data = mydata_long %>% filter(.data$variable == longitude),
      aes(x = .data$variable, y = .data$value),
      size = 2, shape = 1, color = "black", stroke = 1
    ) +
    ggtitle("Longitude") +
    theme_classic() +
    theme(axis.text = element_text(size = 14))
  P3 <- ggplot() +
    geom_errorbar(
      data = range_data %>% filter(.data$variable %in% env_layers),
      aes(x = .data$variable, ymin = .data$ymin, ymax = .data$ymax),
      width = 0.15, color = "blue", linewidth = 1.0
    ) +
    geom_point(
      data = mydata_long %>% filter(.data$variable %in% env_layers),
      aes(x = .data$variable, y = .data$value),
      size = 2, shape = 1, color = "black", stroke = 1, na.rm = TRUE
    ) +
    ggtitle("Environmental Variables") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14)
    )


  return(P1 + P2 + P3 + plot_layout(widths = c(0.6, 0.6, 1.8)))
}
