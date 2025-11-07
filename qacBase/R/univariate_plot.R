#' @title Univariate plot
#'
#' @description
#' Generates a descriptive graph for a quantitative variable.
#'
#' @details
#' \code{univariate_plot} generates a plot containing three graphs:
#' a histogram (with an optional density curve), a horizontal
#' jittered point plot, and a horizontal box plot. The \code{subtitle}
#' contains descriptive statistics, including the mean, standard
#' deviation, median, minimum, maximum, and skew.
#'
#' @note
#' The graphs are created with \link{ggplot2} and then assembled into
#' a single plot through the \link{patchwork} package. Missing values
#' are deleted.
#'
#' @param data a data frame.
#' @param x a variable name (without quotes).
#' @param bins number of histogram bins.
#' @param fill fill color for the histogram and boxplot.
#' @param pointcolor point color for the jitter plot.
#' @param density logical. Plot a filled density curve over the
#' the histogram. (default=TRUE)
#' @param densitycolor fill color for density curve.
#' @param alpha Alpha transparency (0-1) for the density curve and
#' jittered points.
#' @param seed pseudorandom number seed for jittered plot.
#' @import ggplot2
#' @import patchwork
#' @return a ggplot2 graph
#' @export
#' @examples
#' univariate_plot(mtcars, mpg)
#' univariate_plot(cardata, city_mpg, fill="lightsteelblue",
#'                 pointcolor="lightsteelblue", densitycolor="lightpink",
#'                 alpha=.6)
univariate_plot <- function(data, x, bins=30,
                           fill="deepskyblue",
                           pointcolor="black",
                           density=TRUE,
                           densitycolor="grey",
                           alpha=0.2,
                           seed=1234){

  
  if (missing(x)){
    stop("format is univariate_plot(data, x)", call.=FALSE)
  }
  
  x <- as.character(substitute(x))

  ..density.. <- NULL # for CRAN check

  r <- range(data[[x]], na.rm=TRUE)
  minx <- floor(r[1] - (r[2] - r[1])/bins)
  maxx <- ceiling(r[2] + (r[2] - r[1])/bins)
  mean <- mean(data[[x]], na.rm=TRUE)
  sd <- sd(data[[x]], na.rm=TRUE)
  median <- median(data[[x]], na.rm=TRUE)
  IQR <- IQR(data[[x]], na.rm=TRUE)

  # skewness
  v <- stats::na.omit(data[[x]])
  n <- length(v)
  v <- v - mean(v)
  v <- sqrt(n) * sum(v^3)/(sum(v^2)^(3/2))
  skew <- v * ((1 - 1/n))^(3/2)

  title <- paste("Univariate plots for", x)
  subtitle <- paste("n=", n,
                    " mean =", round(mean, 2),
                    " sd =", round(sd, 2),
                    " median =", round(median, 2),
                    " min =", round(r[1], 2),
                    " max =", round(r[2], 2),
                    " skew =", round(skew, 2))

  p1 <- ggplot(data, aes(x=.data[[x]]) ) +
    geom_histogram(aes(y = ..density..),
                   bins=bins,
                   fill=fill,
                   color="black",
                   na.rm=TRUE) +
    geom_vline(xintercept=mean,
               linetype="dashed",
               color="grey40") +
    scale_x_continuous(limits=c(minx, maxx)) +
    labs(title=title,
         subtitle=subtitle) +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          plot.subtitle = element_text(size=8,
                                       face="plain"))

  if (density){
    p1 <- p1 + geom_density(alpha=alpha, fill=densitycolor)
  }

  df <- data.frame(x=mean, y=0)
  p2 <- ggplot(data, (aes(x=.data[[x]]))) +
    geom_boxplot(fill=fill) +
    geom_point(data=df,
               mapping=aes(x=x, y=.data[["y"]]),
               shape="plus",
               color="white",
               size=1) +
    scale_x_continuous(limits=c(minx, maxx)) +
    theme_minimal() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

  df <- data.frame(x=data[[x]], y=0)
  set.seed(seed)
  p3 <- ggplot(df, (aes(x=x, y=.data[["y"]]))) +
    geom_jitter(alpha=alpha, color=pointcolor) +
    scale_x_continuous(limits=c(minx, maxx)) +
    theme_minimal() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

  p1 + p3 + p2 + plot_layout(nrow=3, heights=c(10,2, 1))

}
