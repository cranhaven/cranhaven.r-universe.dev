#' Histograms of the Weights, Beta1, and WQS using \code{\link[ggplot2]{ggplot}}
#'
#' @family wqs
#' @keywords wqs
#'
#' @description  Plots a WQS object producing three histograms of the weights, the overall chemical effect, and WQS
#'  across bootstraps. These histograms are returned as \pkg{ggplot2} objects.
#'
#' @details
#' Three histograms are produced using \code{\link[ggplot2]{geom_histogram}} with ten bins.
#'
#' Once a Weighted Quantile Sum (WQS) regression is run, the \strong{hist.weights} is a panel of histograms. These are distributions of the weight estimates to determine which chemicals are important in the mixture. Each weight is between 0 and 1 and sum to 1. The individual bootstrapped weight estimates were used to construct the overall chemical index, WQS.
#'
#' The \strong{hist.beta1} is the distribution of the overall effect of the mixture on the outcome across bootstraps in the training dataset. Due to the constraint in WQS regression, these estimates are either all positive or all negative as dictated by \emph{b1.pos()} argument in \code{estimate.wqs}. The patterns detected here might be helpful in adjusting the signal function, which is controlled by \emph{signal.fn()} argument in \code{estimate.wqs}.
#'
#' @note
#' Defunct argument \code{filename} has been removed. Plots are no longer saved automatically; please save manually using \code{\link[ggplot2]{ggsave}}().

#' @param x  An object of class "wqs", usually as a result of \code{\link{estimate.wqs}}.
#' @param ... Further arguments passed from other methods. Currently has no effect.

#' @return A list of histograms \describe{
#'  \item{hist.weights}{A list of \pkg{ggplot2} histogram of weights across the bootstrap. Each component consists of a histogram with a weight estimate.}
#'  \item{hist.beta1}{A histogram of the overall chemical mixture effect. This parameter is constrained to be positive if the b1.pos argument in estimate.wqs() is TRUE.; otherwise, it is FALSE.}
#'  \item{hist.WQS}{A histogram of the overall chemical sum, WQS. Due to constraints, it is always between 0 and \emph{n.quantiles-1}.}
#'  }
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
# #' @importFrom tidyselect all_of
#' @export
#'
#' @examples
#' # Use simulated dataset and set seed for reproducibility.
#' data(simdata87)
#' set.seed(23456)
#' Wa <- estimate.wqs(y = simdata87$y.scenario, X = simdata87$X.true[, 1:6],
#'   B = 10, family = "binomial")
#' plot(Wa)

plot.wqs <- function(x, ...) {
  W <- x      # Let W be WQS object.

  # Plot 1: histogram of bootstrap weights
  train.wts  <- as.data.frame(W$train.estimates[, -(1:6)])
  # to.plot <- tidyr::gather(train.wts, key = "chemical", value = "estimate", factor_key = TRUE)  #tidyr::gather is now defunct and is replaced with pivot_longer.
  to.plot <- tidyr::pivot_longer(
    train.wts,
    colnames(train.wts),
    names_to = "chemical",
    values_to = "estimate"
  )
    #R CRAN Check found that the column names of to.plot have 'no visible binding for global variable' so I need to attach it. Tried to remove the dataset and send it with a note. Response: Do you really need to use non standard evaluation? We'd propose to use standard evaluation. If not, pelase declare such usage as described in ?globalVariables.
    # changing the aes() doesn't work either:  # aes(tidyselect::all_of("estimate"),
  #     fill = tidyselect::all_of("chemical")
  #     )
  # )

  hist.weights <-
    ggplot(to.plot, aes(to.plot$estimate, fill = to.plot$chemical)) +
    theme_bw() +
    geom_histogram(bins = 10, color = "black")   +
    facet_wrap(~chemical, scales = 'fixed', dir = "h") +
    xlab("weight") +
    ggtitle("Chemical Weight Estimates Histogram") +
    theme(axis.text.x = element_text(size = 7.5)) +  # make x-axis readable
    guides(fill = FALSE)  # remove legend

  # Plot 2: Beta 1.
  beta1 <- data.frame(beta1 = W$train.estimates$beta_1)
  hist.beta1 <- ggplot(data = beta1, aes(beta1)) + theme_bw() +
    geom_histogram(fill = "white", color = "black", bins = 10) +
    xlab("beta1") + ggtitle("Overall Mixture Effect (Beta1)") +
    xlim(0, ceiling(max(beta1))) #+ ylim(0, hist.freq.max)

  # Plot 3: Distribution of WQS --Works but don't know if I want it.
  WQS <- data.frame(WQS = W$WQS)
  hist.WQS <- ggplot(data = WQS, aes(WQS)) + theme_bw() +
    geom_histogram(fill = "white", color = "black", bins = 10) +
    xlab("WQS") + ggtitle("WQS Histogram") +
    xlim(0, ceiling(max(beta1))) #+ ylim(0, hist.freq.max)

  return(list(hist.weights = hist.weights, hist.beta1 = hist.beta1, hist.WQS = hist.WQS))
}



# #' @param filename  DEFUNCT; argument not used; files are no longer saved.

# #' @param ...                  DEFUNCT. Arguments no longer passed to ggsave(). This argument currently has no effect.
#  #'@param hist.weight.max    DEFUNCT.  maximum weight estimate for x axis of histogram of weights.  Should be
#  #'                          same across all chemicals. Defaults to 1 to show the whole range.
#  #'@param no.weight.bins     Number of bins used to create weight histogram. Defaults to 15.
#  #'@param hist.freq.max      DEFUNCT. maximum frequency for histogram of weights. Should be same across all chemicals.