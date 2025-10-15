#' @title Missing Values Plot
#' @description
#'  This function generates plots to visualize missing values in a data frame. It includes two types of plots:
#'  - A percentage plot: Displays the percentage of missing values for each variable, allowing quick identification
#'     of variables with high missingness.
#'  - A row plot: Illustrates the distribution of missing values across rows, providing insights into patterns of missingness.
#'
#' @param df The input data frame.
#' @param percentage A logical argument (default: TRUE) to generate a percentage plot.
#' @param row A logical argument (default: TRUE) to generate a row plot.
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 

#' @return A list of plots, including a percentage plot and/or a row plot.
#'
#' @examples
#' \donttest{
#' data("airquality")
#' missing_values_plot(df = airquality, percentage = TRUE, row = TRUE)
#' }
#' @importFrom htmltools tagList
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom grDevices colors
#' @export

missing_values_plot <- function(df, percentage = TRUE, row = TRUE, html=FALSE) {
  variable = missing_percentage = key = id = isna = NULL
  if (all(!is.na(df))) {
    return("Data contains no missing values.")
  }
  
  if (percentage) {
    missing_df <- data.frame(
      variable = names(df),
      missing_percentage = colMeans(is.na(df)) * 100
    )
    
    percentage_plot <- ggplot(missing_df, aes(x = reorder(variable, -missing_percentage), y = missing_percentage)) +
      geom_bar(stat = "identity", alpha = 0.8, fill = "tomato") +
      labs(title = "Percentage of Missing Values in Each Variable", x = "Variable", y = "Missing Percentage") +
      coord_flip() +
      theme_minimal()
  }
  
  if (row) {
    long_data <- data.frame(
      id = rep(seq_len(nrow(df)), time = ncol(df)),
      key = rep(colnames(df), each = nrow(df)),
      val = unlist(df),
      isna = ifelse(is.na(unlist(df)), "Missing", "Present")
    )
    
    row_plot <- ggplot(long_data, aes(x = as.factor(key), y = id, fill = isna)) +
      geom_raster(alpha = 0.8) +
      scale_fill_manual(name = "", values = c('tomato', 'steelblue')) +
      labs(x = "Variable", y = "Row Number", title = "Missing values in rows") +
      coord_flip() +
      theme_minimal()
  }

  if (percentage && row) {
    plot_list <- list(ggplotly(percentage_plot), ggplotly(row_plot))
    result <-plot_list
  } else if (percentage) {
    result <- ggplotly(percentage_plot)
  } else if (row) {
    result <- ggplotly(row_plot)
  }
  if (html) result <- htmltools::tagList(result)
  return(result)
  
}

