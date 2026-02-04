#------------------------------------------------------------------------------
# Plotting routine for the plastic prevalence probability
#
# Developed by: Davi Castro Tavares
# Date: May 2018
# Modified by: Esteban Acevedo-Trejos
# Date: July 2019
#------------------------------------------------------------------------------

#' Plastic prevalence probability plot
#'
#' Plot to show the plastic prevalence probability in seabird's nests as a function of
#' different sample sizes and their corresponding confidence intervals.
#'
#' @param prev_prob_mat numeric matrix, containing plastic prevalence probability
#'   with dimensions (samples_size, bootstrap_replicates).
#' @param sample_sizes numeric vector, containing sequence of sample size used to
#'   estimate the confidence intervals \code{plastic.ci}.
#' @param lower_ci numeric vector, containing values for lower confidence interval
#'   and with the same length as \code{sample_sizes}.
#' @param upper_ci numeric vector, containing values for upper confidence interval
#'   and with the same length as \code{sample_sizes}.
#' @param xlab string, label of x axis.
#' @param ylab string, label of y axis.
#' @param colobs color of observations.
#' @param colci color of confidence intervals.
#' @seealso \code{\link{plastic.ci}}, \code{\link{plastic.prev.prob}}
#' @examples
#' binomtest <- plastic.ci(rbinom(1000,1,0.5), 30, 100)
#' prevalence_plot(binomtest$prevprob,
#'                 binomtest$cidtf$N,
#'                 binomtest$cidtf$lower_ci,
#'                 binomtest$cidtf$upper_ci)
#' @importFrom graphics matplot par points
#' @export
prevalence_plot <- function(prev_prob_mat, sample_sizes, lower_ci, upper_ci,
                            xlab = "Sample size",
                            ylab = "Plastic prevalence probability",
                            colobs = "grey", colci = "#64B5F6"){
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #defining plot (quadrant) parameters.
  par(mar = c(5, 4, 4, 4) + 0.3)
  #defining a sequence from 1 to 300 to the horizontal axis, and calling the simulated prevalence values (object called output).
  matplot(sample_sizes, prev_prob_mat, type = "l", lty = 1, col = colobs,
          yaxp = c(0, 1, 5), cex.lab = 0.9, tck = -0.03, cex.axis = 0.9,
          las = 1, xlab = xlab,
          ylab = ylab)
  #adding lower CIs to the plot.
  points(sample_sizes, lower_ci, type = "p", cex = 0.2, ylim = c(0,1), col = colci)
  #adding upper CIs to the plot.
  points(sample_sizes, upper_ci, type = "p", cex = 0.2, ylim = c(0,1), col = colci)
}
