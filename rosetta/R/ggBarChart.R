#' Bar chart using ggplot
#'
#' This function provides a simple interface to create a [ggplot2::ggplot()]
#' bar chart.
#'
#' @param vector The vector to display in the bar chart.
#' @param plotTheme The theme to apply.
#' @param \dots And additional arguments are passed to [ggplot2::geom_bar()].
#' @return A [ggplot2::ggplot()] plot is returned.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@behaviorchange.eu>
#' @seealso [ggplot2::geom_bar()]
#' @examples rosetta::ggBarChart(mtcars$cyl);
#'
#' @export ggBarChart
ggBarChart <- function(vector, plotTheme = ggplot2::theme_bw(), ...) {
  varName <- ufs::extractVarName(deparse(substitute(vector)));
  tmpDf <- as.data.frame(stats::na.omit(vector));
  names(tmpDf) <- varName;
  tmpDf[, varName] <- as.factor(tmpDf[, varName]);
  ggplot2::ggplot(tmpDf, ggplot2::aes_string(x=varName)) +
    ggplot2::geom_bar(...) + plotTheme;
}
