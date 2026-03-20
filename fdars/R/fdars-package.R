#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom grDevices rgb
#' @importFrom graphics abline legend lines mtext par points
#' @importFrom stats approx as.dist coef dnorm hclust lm mad median optimize pnorm qchisq qnorm qt quantile rnorm runif sd var
#' @importFrom utils head
#' @importFrom ggplot2 .data
## usethis namespace: end
NULL

# Global variable declarations for ggplot2 NSE
utils::globalVariables(c(
  "PC1", "PC2", "X", "id", "value", "curve_type", "component", "prop",
  "cumulative", "mei", "mbd", "mbd_theoretical", "mbd_threshold"
))

