#'Repartition of Financial Funds Allocation: Investment Amounts Barplot
#'
#'The \code{plot_funds_bar} function creates a barplot to visually represent the distribution of financial funds allocation across different investment channels.
#'
#'@param data   Dataset of class 'data.frame'. Specify the dataset from which to take information.
#'@param var_col integer value. Specify the number of the columns associated with the variable to visualize.
#'
#'@returns An object of class \code{gg} and \code{ggplot} representing the barplot
#'
#'@author Lorena Ricciotti
#'
#'@examples
#'data(OCpoint)
#'plot_funds_bar(OCpoint, var_col = c(10:15))
#'#Barplot visualizing the total amount allocated by each fund.
#'
#'@export
plot_funds_bar <- function(data, var_col) {

  # Total amount for each resource
  sum_funds <- data %>%
    dplyr::mutate_at(dplyr::vars(var_col), ~ tidyr::replace_na(., 0)) %>%
    dplyr::summarise(dplyr::across(var_col, sum))

  sum_funds <- as.data.frame(t(sum_funds))

  # Rename columns
  colnames(sum_funds) <-  "Ammontare Speso"
  sum_funds <- sum_funds %>%
    tibble::rownames_to_column(var = "Fonte")


  sum_funds <- sum_funds[order(sum_funds$`Ammontare Speso`, decreasing = TRUE),]

  # Barplot
  bar_plot <- ggplot2::ggplot(data = sum_funds, ggplot2::aes(x = stats::reorder(.data$Fonte, -.data$`Ammontare Speso`), y = .data$`Ammontare Speso`)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = .data$Fonte), width = 0.7) +
    ggplot2::labs(fill = "Resources", title = "Total Amount \n for each Financial Resource", x = "Financial Resources", y = "Total Amount (mln)") +
    ggplot2::scale_fill_viridis_d()+
    ggplot2::scale_y_continuous(labels = scales::number_format(scale = 1e-6)) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


  return(bar_plot)
}
