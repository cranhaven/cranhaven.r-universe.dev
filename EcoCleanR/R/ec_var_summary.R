#' A Summary Table of Final Cleaned Spatial and Environmental Variables
#'
#' @param data data table after cleaning the records
#' @param latitude default set to "decimalLatitude"
#' @param longitude default set to "decimalLongitude"
#' @param env_layers an array of col names of enviornmental layers
#' @return A summary table with the mean, min and max values of final cleaned spatial and environmental variables
#'
#' @export
#' @examples
#' data <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9, -116.5),
#'   decimalLatitude = c(32.9, 33.5, 31.9, 32.4),
#'   BO_sstmean = c(12, 13, 14, 11),
#'   BO_sstmin = c(9, 6, 10, 10),
#'   BO_sstmax = c(14, 16, 18, 17)
#' )
#' env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
#' ec_var_summary(data,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   env_layers
#' )
#'
ec_var_summary <- function(data, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers) {
  vars_to_summarize <- c(latitude, longitude, env_layers)

  # Subset the data
  data_subset <- data[, vars_to_summarize]

  summary_table <- data.frame(
    Mean = round(colMeans(data_subset, na.rm = TRUE), 2),
    Min = round(apply(data_subset, 2, min, na.rm = TRUE), 2),
    Max = round(apply(data_subset, 2, max, na.rm = TRUE), 2)
  )

  summary_table$variable <- rownames(summary_table)
  rownames(summary_table) <- seq_len(nrow(summary_table))
  summary_table <- summary_table[, c("variable", "Max", "Min", "Mean")]

  return(summary_table)
}
