#' @title Scatterplot
#'
#' @description
#' Create a scatter plot between two quantitative variables.
#'
#' @param data data frame
#' @param x quantitative predictor variable
#' @param y quantitative response variable
#' @param outlier number. Observations with studentized residuals
#' larger than this value are flagged. If set to 0, observations
#' are not flagged.
#' @param alpha Transparency of data points. A numeric value between 0
#' (completely transparent) and 1 (completely opaque).
#' @param digits Number of significant digits in displayed statistics.
#' @param title Optional title.
#' @param margin Marginal plots. If specified, parameter can be
#' \code{histogram}, \code{boxplot}, \code{violin},
#' or \code{density}. Will add these
#' features to the top and right margin of the graph.
#' @param stats logical. If \code{TRUE}, the slope,
#' correlation, and correlation squared (expressed as a percentage) for the
#' regression line are printed on the subtitle line.
#' @param point_color Color used for points.
#' @param outlier_color Color used to identify outliers (see the \code{outlier}
#' parameter.
#' @param line_color Color for regression line.
#' @param margin_color Fill color for margin boxplots, density plots, or
#' histograms.
#'
#' @details
#' The \code{scatter} function generates a scatterplot between two quantitative
#' variables, along with a line of best fit and a 95\% confidence interval.
#' By default, regression statistics (b, r, r2, p) are printed and
#' outliers (observations with studentized residuals > 3) are flagged.
#' Optionally, variable distributions (histograms, boxplots, violin plots,
#' density plots) can be added to the plot margins.
#'
#' @note
#' Variable names do not have to be quoted.
#'
#' @return a ggplot2 graph
#' @export
#' @import ggplot2
#' @import ggExtra
#' @importFrom stats as.formula coefficients lm na.omit pf residuals rstudent
#' @examples
#' scatter(cars74, hp, mpg)
#' scatter(cars74, wt, hp)
#' p <- scatter(ggplot2::mpg, displ, hwy,
#'         margin="histogram",
#'         title="Engine Displacement vs. Highway Mileage")
#' plot(p)
scatter <- function(data, x, y,
                    outlier=3,
                    alpha=1,
                    digits=3,
                    title,
                    margin="none",
                    stats=TRUE,
                    point_color="deepskyblue2",
                    outlier_color="violetred1",
                    line_color="grey30",
                    margin_color="deepskyblue2"){

  # import parameters
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  f <- as.formula(paste(y, "~", x))

  # title
  if(missing(title)){
    title <- paste("Plot of", x, "by", y)
  }


  # remove missing data
  data <- na.omit(data[c(x, y)])

  # fit model and flag outliers
  fit  <- lm(f, data)
  sfit <- summary(fit)
  b0   <- coefficients(fit)[1]
  b1   <- coefficients(fit)[2]
  Fvalue <- sfit$fstatistic[1]
  dfn  <- sfit$fstatistic[2]
  dfd  <- sfit$fstatistic[3]
  r    <- sqrt(sfit$r.squared)
  p    <- pf(Fvalue, dfn, dfd, lower.tail = FALSE)

  # studentized residuals
  data$stud.residuals <- rstudent(fit)


  # rmse
  # rmse <- sqrt(mean(residuals(fit)^2))

  # flag outliers
  data$outlier <- ifelse(abs(data$stud.residuals) >= outlier,
                         "outlier", "non-outlier")

  # p-value
  p_value = "p > 0.05"
  if (p < .05) p_value = "p < .05"
  if (p < .01) p_value = "p < .01"
  if (p < .001) p_value = "p < .001"


  # create informational inset
  inset <- paste0(
    "slope = ",
    format(b1, big.mark=",", digits=digits),
    # ", RMSE = ",
    # format(rmse, big.mark=",", digits=digits),
    ", r = ",
    round(r, digits),
    " (", round(r*r*100), "%), ",
    p_value)

  # create plot
  p <- ggplot(data=data,
              aes(x=.data[[x]],
                  y=.data[[y]])) +
    geom_point(alpha=alpha,
               aes(color=.data[["outlier"]])) +
    geom_smooth(method="lm",
                formula=y~x,
                color=line_color) +

    scale_color_manual(values=c(point_color, outlier_color)) +
    labs(title = title) +
    theme_bw() +
    theme(legend.position="none",
          plot.subtitle = element_text(size=8,face="plain"),
          plot.caption = element_text(size=8, face="plain"))

  if (stats){
    p <- p + labs(subtitle=inset)
  }


  # outlier caption
  if (any(abs(data$stud.residuals) > outlier & outlier != 0)){
    p <- p + labs(caption=paste("Note: studentized residuals >",
                                outlier, "are highlighted."))
  }
  # add margins
  if(margin!="none"){
    p <- ggExtra::ggMarginal(p, size=8, type=margin,
                             fill=margin_color)
  }

  # return graph
  return(p)
}
