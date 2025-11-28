#' Simple function to create a histogram
#'
#' @param vector A variable or vector.
#' @param bins The number of bins; when 0, either the number of unique
#' values in `vector` or `20`, whichever is lower.
#' @param xLabel,yLabel Labels for x and y axes; variable name is used for
#' x axis if no label is specified.
#' @param theme The ggplot2 theme to use.
#'
#' @return A ggplot2 plot.
#' @export
#'
#' @examples rosetta::histogram(mtcars$mpg);
histogram <- function(vector,
                      bins = NULL,
                      theme = ggplot2::theme_bw(),
                      xLabel = NULL,
                      yLabel = "Count") {
  xName <- deparse(substitute(vector));
  tmpDf <- data.frame(x = vector);
  names(tmpDf) <- xName;
  if (is.null(bins)) {
    bins <-
      min(length(unique(stats::na.omit(vector))), 20);
  }
  if (is.null(xLabel)) {
    xLabel <- xName;
  }

  return(
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = xName)) +
      ggplot2::geom_histogram(bins = bins) +
      ggplot2::labs(x = xLabel,
                    y = yLabel) +
      theme
  );
}
