#' @title Histograms
#' @description Create histograms for all quantitative variables in a data frame.
#' @param data data frame
#' @param fill fill color for histogram bars
#' @param color border color for histogram bars
#' @param bins number of bins (bars) for the histograms
#' @return a ggplot graph
#' @details
#' The \code{histograms} function will only plot quantitative variables from
#' a data frame. Categorical variables are ignored.
#' @examples
#' histograms(cars74)
#' histograms(cars74, bins=15, fill="darkred")
#' @rdname histograms
#' @import tidyr
#' @import ggplot2
#' @export
histograms <- function(data, fill="deepskyblue2", color="white", bins=30){
  index <- sapply(data, is.numeric)
  qdata <- data[index]
  qdata_long <- tidyr::gather(qdata)
  ggplot2::ggplot(data=qdata_long, aes(x=.data[["value"]])) +
    ggplot2::geom_histogram(fill=fill, bins=bins, color=color) +
    ggplot2::facet_wrap(~key, scale="free") +
    ggplot2::theme_bw() +
    labs(title="Histograms")

}
