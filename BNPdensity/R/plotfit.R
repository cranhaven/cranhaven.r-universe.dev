#' Plot the density estimate and the 95\% credible interval for noncensored
#' data
#'
#' The density estimate is the mean posterior density computed on the data
#' points.
#' @import graphics
#'
#' @param fit A fitted object of class NRMI1 or NRMI2
#' @return A graph with the density estimate, the 95\% credible interval and a
#' histogram of the data
#' @examples
#'
#' data(acidity)
#' out <- MixNRMI1(acidity, Nit = 50)
#' plot(out)
plotfit_noncensored <- function(fit) {
  m <- ncol(fit$qx)
  nbins <- length(hist(fit$data, plot = FALSE)$breaks) - 1
  ggplot(data.frame(xx = fit$xx, infCI = fit$qx[, 2], supCI = fit$qx[, m], y = fit$qx[, 1]), aes_string(x = "xx")) +
    theme_classic() +
    geom_histogram(
      data = data.frame(x = fit$data), aes_string(x = "x", y = "..density.."),
      fill = grDevices::grey(0.9),
      colour = "black",
      bins = nbins
    ) +
    geom_line(aes_string(y = "y"), size = 1.) +
    geom_line(aes_string(y = "infCI"), colour = "blue", linetype = "dotted") +
    geom_line(aes_string(y = "supCI"), colour = "blue", linetype = "dotted") +
    xlab("Data") +
    ylab("Density")
}

#' Plot the density estimate and the 95\% credible interval for censored data
#'
#' The density estimate is the mean posterior density computed on the data
#' points. It is not possible to display a histogram for censored data.
#'
#'
#' @param fit A fitted object of class NRMI1cens or NRMI2cens
#' @return A graph with the density estimate and the 95\% credible interval
#' @examples
#'
#' data(acidity)
#' out <- MixNRMI1(acidity, Nit = 50)
#' plot(out)
plotfit_censored <- function(fit) {
  m <- ncol(fit$qx)
  ggplot(data.frame(xx = fit$xx, infCI = fit$qx[, 2], supCI = fit$qx[, m], y = fit$qx[, 1]), aes_string(x = "xx")) +
    theme_classic() +
    geom_line(aes_string(y = "y"), size = 1.) +
    geom_line(aes_string(y = "infCI"), colour = "blue", linetype = "dotted") +
    geom_line(aes_string(y = "supCI"), colour = "blue", linetype = "dotted") +
    xlab("Data") +
    ylab("Density")
}

#' #' Plot the density estimate and grey lines to represent the uncertainty on the density estimate
#' #'
#' #' @param fit A fitted object of class NRMI1 or NRMI2
#' #'
#' #' @return A graph with the density estimate, the 95\% credible interval and a histogram of the data.
#' #' @export
#' #'
#' #' @examples
#' plotfit_manylines = function(fit) {
#'   1+1
#' }
