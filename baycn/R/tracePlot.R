#' tracePlot
#'
#' Creates a trace plot of the log likelihood and graph decimal. The graph
#' decimal is a decimal number calculated from the vector of edge states for the
#' accepted graph.
#'
#' @param x An object of class baycn.
#'
#' @import ggplot2
#'
#' @importFrom egg ggarrange
#'
#' @export
#'
tracePlot <- function (x) {

  # The following lines are to avoid the note 'Undefined global
  # functions or variables: likelihood, decimal'.
  likelihood <- NULL
  decimal <- NULL

  # Number of samples kept
  nSamples <- length(x@likelihood)

  # Convert the likelihood to a data frame to pass to ggplot.
  logLikelihood <- data.frame(likelihood = x@likelihood)

  # Convert the decimal numbers to a data frame to pass to ggplot.
  decimalNum <- data.frame(decimal = x@decimal)

  # Create the trace plot for the likelihood.
  p <- ggplot(logLikelihood, aes(x = 1:nSamples,
                                 y = likelihood)) +
    ggtitle('log likelihood') +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.line = element_line(color = 'black')) +
    geom_line(color = '#5500cc',
              size = 1) +
    labs(x = '') +
    labs(y = '') +
    ylim(min(logLikelihood), max(logLikelihood))

  # Create the trace plot for the decimal number.
  g <- ggplot(decimalNum, aes(x = 1:nSamples,
                              y = decimal)) +
    ggtitle('decimal number') +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.line = element_line(color = 'black')) +
    geom_line(color = '#cc5500',
              size = 1) +
    labs(x = '') +
    labs(y = '') +
    ylim(min(decimalNum), max(decimalNum))

  # print the two plots with one column and two rows.
  egg::ggarrange(p, g, ncol = 1)

}
