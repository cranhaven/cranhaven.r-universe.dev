#' Bar chart using ggplot
#'
#' This function provides a simple interface to create a [ggplot2::ggplot()]
#' bar chart.
#'
#' @param x,y The vectors to display in the scatter plot. Alternatively,
#' `x` can be a data frame; then `y` has to be a vector with (numeric or
#' character) indices, e.g. column names.
#' @param jitter Whether to jitter the points (`TRUE` by default).
#' @param size,alpha,shape,color,fill,stroke Quick way to set the aesthetics.
#' @param plotTheme The theme to apply.
#' @param \dots And additional arguments are passed to [ggplot2::geom_point()].
#'
#' @return A [ggplot2::ggplot()] plot is returned.
#' @seealso [ggplot2::geom_point()]
#' @examples rosetta::ggScatterPlot(mtcars$hp, mtcars$mpg);
#'
#' @export ggScatterPlot
ggScatterPlot <- function(x,
                          y,
                          jitter = TRUE,
                          size = 3,
                          alpha = .66,
                          shape = 16,
                          color = "black",
                          fill = "black",
                          stroke = 1,
                          plotTheme = ggplot2::theme_bw(),
                          ...) {

  if (is.data.frame(x)) {
    if ((length(y)==2) && is.numeric(y)) {
      if (all(y %in% 1:ncol(x))) {
        tmpDf <- x;
        xVarName <- names(tmpDf)[y[1]];
        yVarName <- names(tmpDf)[y[2]];
        x <- tmpDf[, xVarName];
        y <- tmpDf[, yVarName];
      } else {
        stop("You provided a data frame as `x` and two numbers as `y`, but these numbers ("
             , vecTxt(y),
             ") do not correspond to columns that you have in `x` (", ncol(x), ").");
      }
    } else if ((length(y)==2) && is.character(y)) {
      if (all(y %in% names(x))) {
        tmpDf <- x;
        xVarName <- y[1];
        yVarName <- y[2];
        x <- tmpDf[, xVarName];
        y <- tmpDf[, yVarName];
      } else {
        stop("You provided a data frame as `x` and two column names as `y`, but these columns ("
             , vecTxt(y),
             ") do not correspond to columns that you have in `x`.");
      }
    } else {
      stop("Arguments `x` and `y` should be either the vectors to display in the scatter plot, or `x` should be a data frame and `y` a vector with (numeric or character) indices, e.g. column names.");
    }
  } else {
    if (!(is.numeric(x) && is.numeric(y))) {
      stop("If you provide vectors for `x` and `y`, they must both be numeric vectors.");
    }
    if (!(length(x) == length(y))) {
      stop("If you provide vectors for `x` and `y`, they must be the same length.");
    }
    varNameX <- ufs::extractVarName(deparse(substitute(x)));
    varNameY <- ufs::extractVarName(deparse(substitute(y)));
    tmpDf <- data.frame(x, y);
    names(tmpDf) <- c(varNameX, varNameY);;
  }

  tmpDf <- as.data.frame(stats::na.omit(tmpDf));

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x=varNameX,
                                                  y=varNameY)) +
    plotTheme;

  if (jitter) {
    res <-
      res +
      ggplot2::geom_point(position = ggplot2::position_jitter(),
                          size = size,
                          alpha = alpha,
                          stroke = stroke,
                          shape = shape,
                          color = color,
                          fill = fill,
                          ...)
  } else {
    res <-
      res +
      ggplot2::geom_point(size = size,
                          alpha = alpha,
                          stroke = stroke,
                          shape = shape,
                          color = color,
                          fill = fill,
                          ...)
  }

  return(res);

}
