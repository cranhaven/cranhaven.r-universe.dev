.onUnload <- function (libpath) {
  library.dynam.unload("BSL", libpath)
}

#' @importFrom stats runif cov sd pnorm rnorm qnorm density approx dexp rexp median quantile
#' @importFrom graphics par lines abline layout legend
#' @importFrom utils flush.console capture.output head tail
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm rmvnorm
#' @importFrom glasso glasso
#' @importFrom coda effectiveSize
#' @import foreach
#' @importFrom ggplot2 ggplot aes_string geom_density geom_hline geom_vline theme labs element_blank ggplotGrob
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual scale_size_manual
#' @importFrom ggplot2 aes geom_line facet_wrap label_both geom_label element_text
#' @importFrom grid unit unit.c grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom copula normalCopula p2P P2p dCopula
#' @importFrom whitening whiteningMatrix
#' @importFrom Rdpack reprompt
#' @importFrom doRNG registerDoRNG
#' @import methods
NULL

#' @useDynLib BSL, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
