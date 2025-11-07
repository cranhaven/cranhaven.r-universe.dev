#' @title Density plots
#' @description Create desnsity plots for all quantitative variables in a data frame.
#' @param data data frame
#' @param fill fill color for density plots
#' @param adjust a factor multiplied by the smoothing bandwidth. See details.
#' @return a ggplot graph
#' @details
#' The \code{densities} function will only plot quantitative variables from
#' a data frame. Categorical variables are ignored.
#'
#' The \code{adjust} parameter mulitplies the smoothing parameter. For example
#' \code{adjust = 2} will make the density plots twice as smooth.
#' The \code{adjust = 1/2} will make the density plots half as smooth (i.e., twice as spiky).
#' @examples
#' densities(cars74)
#'
#' densities(cars74, adjust=2)
#'
#' densities(cars74, adjust=1/2)
#' @rdname densities
#' @import tidyr
#' @import ggplot2
#' @export
densities <- function(data, fill="deepskyblue2", adjust=1){
  index <- sapply(data, is.numeric)
  qdata <- data[index]
  qdata_long <- tidyr::gather(qdata)
  ggplot2::ggplot(data=qdata_long, aes(x=.data[["value"]])) +
    ggplot2::geom_density(fill=fill, adjust=adjust) +
    ggplot2::facet_wrap(~key, scale="free") +
    ggplot2::theme_bw() +
    labs(title="Density Plots")

}
