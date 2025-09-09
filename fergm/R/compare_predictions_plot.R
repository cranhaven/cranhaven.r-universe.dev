#' Compare FERGM to ERGM predictions via plot.
#' This is a plot function to compare the distributions of predictions from \code{ergm} and \code{fergm} objects.
#' @param compare_predictions_out Matrix of correctly predicted ties produced by the \code{compare_predictions} function.
#' @keywords Fit GOF Prediction Plot
#' @return The compare_predictions_plot function returns a ggplot2 plot of the density of the percent of correctly predicted ties simulated by the compare_predictions function.
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @example
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' # Use built in compare_predictions function to compare predictions of ERGM and FERGM,
#' # few replications due to example.
#'  net <- ergm.fit$network
#' predict_out <- compare_predictions(ergm.fit = ergm.fit, fergm.fit = fergm.fit,
#'                                    replications = 10, seed = 123456)
#' # Use the built in compare_predictions_plot function to examine the densities
#' # of correctly predicted ties from the compare_predictions simulations
#' compare_predictions_plot(predict_out)
#'
#' @export

compare_predictions_plot <- function(compare_predictions_out = NULL){
  plot_df <- reshape2::melt(as.data.frame(compare_predictions_out))
  p <- ggplot2::ggplot(data = plot_df, aes(x = plot_df$value, color = plot_df$variable, fill = plot_df$variable)) +
    geom_density(alpha = 0.5) +
    xlab("Percent of Ties Correctly Predicted") +
    ylab("Density") +
    scale_fill_manual(values=c("firebrick4", "dodgerblue4"),
                      name="Model",
                      breaks=c("pct_correct_ergm", "pct_correct_fergm"),
                      labels=c("ERGM", "FERGM")) +
    scale_color_manual(values=c("firebrick4", "dodgerblue4"),
                      name="Model",
                      breaks=c("pct_correct_ergm", "pct_correct_fergm"),
                      labels=c("ERGM", "FERGM")) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))

  return(p)
}
