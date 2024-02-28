#' @title  Create Diagnostic Plots
#'
#' @description Generate residual diagnostic plots from a step1_down fit.
#'
#' @details Use the fit object obtained from the step1_down function to plot the
#' residual diagnostic plots, assess the quality of fit and search for anomalies.
#' Plots created are: Residuals Histogram, Observed Vs Predicted results, Residuals
#'  Vs Predicted results and QQplot of Residuals.
#'
#' @param step1_down_object The fit object from the step1_down function (required).
#' @param bins The number of bins in the Histogram plot (default 7).
#'
#' @return A list containing the four ggplot2 plots.
#'
#' @examples
#' #load antigenicity data
#' data(antigenicity)
#'
#' #run step1_down fit
#' fit1 <- step1_down(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", max_time_pred = 3)
#'
#' #plot diagnostic plots to asses the fit
#' step1_plot_diagnostic(fit1)
#'
#' @import ggplot2
#' @importFrom stats dnorm sd
#'
#' @export step1_plot_diagnostic

step1_plot_diagnostic <- function(step1_down_object, bins = 7)
{
  if (is.null(step1_down_object))
    stop("First, run the model")
  dat = step1_down_object$data

  mytheme <- ggplot2::theme(legend.position = "bottom", strip.background = element_rect(fill = "white"),
                            legend.key = element_rect(fill = "white"), legend.key.width = unit(2,"cm"),
                            axis.text = element_text(size = 13), axis.title = element_text(size = 13),
                            strip.text = element_text(size = 13),
                            legend.text = element_text(size = 13),
                            legend.title = element_text(size = 13))

  validation = step1_down_object$user_parameters$validation
  if(!is.null(validation)){
    dat <- dat[dat$validation == "Fit",]
  }

  dat$residuals <- summary(step1_down_object$fit)$residuals

  dat$predicted <- dat$y - dat$residuals

  # Histogram plot

  res_histo = ggplot(dat, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)),
                   breaks = seq(min(dat$residuals), max(dat$residuals), by = (max(dat$residuals) - min(dat$residuals))/bins),
                   colour = "black",
                   fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(dat$residuals), sd = sd(dat$residuals)),
                  xlim = c(min(dat$residuals), max(dat$residuals)),
                  col = "turquoise",
                  linewidth = 1,
                  alpha = 0.6) + ggtitle ("Residuals Histogram") + xlab("Residuals") + ylab("Density") +
    mytheme


  # observed vs predicted

  obs_pred = ggplot() + geom_point(data = dat, mapping = aes(x = predicted, y = y, colour = Celsius)) +
    geom_smooth(data = dat, method ="lm", formula = y ~ x, mapping = aes(x = predicted, y = y)) +
    labs( x = "Predicted response", y = "Observed data")+
    ggtitle ("Observed Vs Predicted") +
    mytheme

  # residuals vs predicted
  res_pred = ggplot() + geom_point(data = dat, mapping = aes(x = predicted, y = residuals, colour = Celsius)) +
    labs( x = "Predicted response", y = "Residuals") +
    geom_hline(yintercept=0, linetype="solid", color = "black")+
    ggtitle ("Residuals Vs Predicted") +
    mytheme

  # QQplot
  qqplot <- ggplot(as.data.frame(dat), aes(sample = residuals)) +
    stat_qq(aes(colour = Celsius)) + stat_qq_line() +
    ggtitle ("Q-Q Plot") + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")+
    mytheme

  results = list(res_histo,obs_pred,res_pred,qqplot)
  names(results) = c("Residuals_Histogram","Observed_V_Predicted","Residuals_V_Predicted","Q_Q_Plot")
  return(results)
}

globalVariables(c('residuals','density','predicted'))



