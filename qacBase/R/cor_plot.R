#' @title Correlation matrix plot
#' @description Create a correlation matrix for all quantitative variables in a data frame.
#' @param data data frame
#' @param method a character string indicating which correlation
#' coefficient is to be computed. One of "pearson" (default), "kendall",
#' or "spearman".
#' @param sort logical. If \code{TRUE}, reorder variables to place variables
#' with similar correlation patterns together.
#' @param number_text_size size for correlation coefficient labels (default=3).
#' @param axis_text_size size for axis labels (default=12).
#' @param legend logical, if TRUE the legend is displayed.
#' (default=FALSE)
#' @return a ggplot graph
#' @details
#' The \code{cor_plot} function will only select quantitative variables from
#' a data frame. Categorical variables are ignored.
#' The correlation matrix is presented as a lower triangle matrix.
#' Missing values are deleted in listwise fashion.
#'
#' @note
#' This function is a wrapper for the \code{\link[ggcorrplot:ggcorrplot]{ggcorrplot}} function.
#' @examples
#' cor_plot(cars74)
#' cor_plot(cars74, sort=TRUE)
#' @rdname cor_plot
#' @import ggcorrplot
#' @import ggplot2
#' @export
cor_plot <- function(data, method=c("pearson", "kendall", "spearman"),
                     sort=FALSE,
                     axis_text_size=12,
                     number_text_size=3,
                     legend=FALSE){
  method <- match.arg(method)
  index <- sapply(data, is.numeric)
  qdata <- data[index]
  qdata <- na.omit(qdata)
  # bind global variables to keep check from warning

  r <- stats::cor(qdata, method=method)
  p <- ggcorrplot(r,
                  hc.order = sort,
                  colors = c("red", "white", "blue"),
                  type = "lower",
                  lab = TRUE,
                  lab_size=number_text_size,
                  show.legend=legend)
  n <- format(nrow(qdata), big.mark=",")
  if (method == "pearson"){
    subtitle <- paste0("Pearson correlations (n = ",n, ")")
  }
  if (method == "spearman"){
    subtitle <- paste0("Spearman rank order correlations (n = ",n, ")")
  }
  if (method == "kendall"){
    subtitle <- paste0("Kendall rank order correlations (n = ",n, ")")
  }

  p <- p + labs(title = "Correlation Matrix",
                subtitle = subtitle) +
    theme(axis.text.x=element_text(size=axis_text_size),
          axis.text.y=element_text(size=axis_text_size),
          plot.subtitle = element_text(size=8,
                                       face="plain"))

  return(p)
}
