#' @import ggplot2 ggrepel hrbrthemes tidyverse dplyr tidyr tibble
#' @export
HeatMap <- function(data_frequency_list, orders = seq(0.50, 3, by = 0.01), selection = 1:length(data_frequency_list), plot_order = selection, RowNames = names(data_frequency_list)[plot_order], title = "HeatMap", x_ticks = round(stats::quantile(orders, c(0,0.25, 0.5, 0.75, 1)), 2), plot_margin = ggplot2::margin(0.5,0.2,0.2,1, "cm"), text_face = 1, fill_colors = c("blue4", "white", "red3"), title_text_size = 25, label_text_size = 25){
  generalized.entropy <- function(sample_freq, order){
    results <- vector()
    n <- sum(sample_freq)
    phats <- sample_freq/n
    phats <- phats[phats>0]
    for (i in 1:length(order)) {
      pmhats <- phats^order[i]/sum(phats^order[i])
      results[i] <- sum(-pmhats*log(pmhats))
    }
    return(results)
  }

  df <- t(data.frame(lapply(data_frequency_list[plot_order], function(x){generalized.entropy(x, orders)})))
  colnames(df) <- orders
  name <- "suppress error note"
  rowname <- "suppress error note"
  value <- "suppress error note"
  p <- as.data.frame(df) |>
    rownames_to_column() |>
    pivot_longer(-rowname) |>
    mutate(rowname = factor(rowname, rownames(df))) |>
    ggplot(aes(factor(name, unique(name)), rowname, fill = value)) +
    ggtitle(title) +
    scale_x_discrete(name = "Orders", labels = ~., breaks = ~ x_ticks) + theme(plot.title = element_text(hjust = 0.5, size = title_text_size), text = element_text(size=label_text_size)) +
    geom_tile() +
    scale_fill_gradientn(colours = fill_colors) +
    scale_y_discrete(name = NULL, position = "right") +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          text = element_text(face = text_face)) +
    theme(plot.margin = plot_margin)
  return(p)
}



