#' Plots record count by Scientific Name on a bar graph.
#'
#' \code{plot_records} returns a bar graph showing the number of records for each distinct
#' scientific name in the dataset.
#'
#' This is a function to visualize data by Scientific Name from FishNet2 search query.
#' A dataframe is input from a standard FishNet2 search query.
#'
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics plot
#' @param df A dataframe in FishNet2 standard format (by using read.csv())
#' @param top_ten Top ten species occurrence counts
#' @param color True if each bar should have a distinct color, FALSE for grey bars. Default: TRUE
#' @return Plot of record count by Scientific Name on a bar graph
#'
#' @examples
#' plot_records(louisiana)

plot_records <- function(df, top_ten = TRUE, color = TRUE) {
  if (color == TRUE){
    res <- plot(df$ScientificName, xlab = "Scientific Name", ylab = "Number of records", main = "Records by Scientific Name", col = rainbow(nlevels(df$ScientificName)))
  } else {
    res <- plot(df$ScientificName, xlab = "Scientific Name", ylab = "Number of records", main = "Records by Scientific Name")
  }
  return(res)
}
