#' Two-dimensional visualisation of factor analyses
#'
#' This function uses the [diamondPlot()] to visualise the results of
#' a factor analyses. Because the factor loadings computed in factor analysis
#' are point estimates, they may vary from sample to sample. The factor
#' loadings for any given sample are usually not relevant; samples are but
#' means to study populations, and so, researchers are usually interested in
#' population values for the factor loadings. However, tables with lots of
#' loadings can quickly become confusing and intimidating. This function aims
#' to facilitate working with and interpreting factor analysis based on
#' confidence intervals by visualising the factor loadings and their confidence
#' intervals.
#'
#'
#' @param fa The object produced by the [psych::fa()] function from the
#' [psych::psych] package. It is important that the `n.iter` argument
#' of [psych::fa()] was set to a realistic number, because otherwise, no
#' confidence intervals will be available.
#' @param xlab The label for the x axis.
#' @param colors The colors used for the factors. The default uses the discrete
#' `viridis` palette, which is optimized for perceptual uniformity,
#' maintaining its properties when printed in grayscale, and designed for
#' colourblind readers.
#' @param labels The labels to use for the items (on the Y axis).
#' @param showLoadings Whether to show the factor loadings or not.
#' @param heatmap Whether to produce a heatmap or use diamond plots.
#' @param theme The ggplot2 theme to use.
#' @param sortAlphabetically Whether to sort the items alphabetically.
#' @param digits Number of digits to round to.
#' @param labs The labels to pass to ggplot2.
#' @param themeArgs Additional theme arguments to pass to ggplot2.
#' @param \dots Additional arguments will be passed to
#' [ggDiamondLayer()]. This can be used to set, for example, the
#' transparency (alpha value) of the diamonds to a lower value using e.g.
#' `alpha=.5`.
#' @return A [ggplot2::ggplot()] plot with several
#' [ggDiamondLayer()]s is returned.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @seealso [psych::fa()]ss, [meansDiamondPlot()],
#' [meanSDtoDiamondPlot()], [diamondPlot()],
#' [ggDiamondLayer()]
#' @keywords hplot
#' @examples
#'
#' \dontrun{
#' ### (Not run during testing because it takes too long and
#' ###  may generate warnings because of the bootstrapping of
#' ###  the confidence intervals)
#'
#' factorLoadingHeatmap(psych::fa(psych::Bechtoldt,
#'                                nfactors=2,
#'                                n.iter=50,
#'                                n.obs=200));
#'
#' ### And using a lower alpha value for the diamonds to
#' ### make them more transparent
#'
#' factorLoadingHeatmap(psych::fa(psych::Bechtoldt,
#'                                nfactors=2,
#'                                n.iter=50,
#'                                n.obs=200),
#'                      alpha=.5,
#'                      size=1);
#' }
#'
#' @export
factorLoadingHeatmap <- function(fa,
                                 xlab='Factor Loading',
                                 colors = viridisPalette(max(2, fa$factors)),
                                 labels=NULL,
                                 showLoadings = FALSE,
                                 heatmap = FALSE,
                                 theme=ggplot2::theme_minimal(),
                                 sortAlphabetically = FALSE,
                                 digits=2,
                                 labs = list(title = NULL,
                                             x = NULL,
                                             y = NULL),
                                 themeArgs = list(panel.grid = ggplot2::element_blank(),
                                                  legend.position = "none",
                                                  axis.text.x = ggplot2::element_blank()),
                                 ...) {

  ### Create list for CIs per factor
  loadings <- as.data.frame(unclass(fa$loadings));

  dotsList <- as.list(substitute(list(...)));

  if ('alpha' %in% names(dotsList)) {
    alpha <- dotsList$alpha;
  } else {
    alpha <- 1;
  }

  if (is.null(labels)) {
    labels <- rownames(unclass(fa$loadings));
  }

  if (sortAlphabetically) {
    sortOrder <- order(labels);
  } else {
    sortOrder <- seq_along(labels);
  }

  tmpDf <-
    utils::stack(loadings);

  tmpDf$Variable <-
    factor(
      rep(row.names(loadings),
          ncol(loadings)),
      levels = row.names(loadings)[rev(sortOrder)],
      labels = row.names(loadings)[rev(sortOrder)],
      ordered = TRUE
    );

  tmpDf$Factor <-
    factor(
      tmpDf$ind,
      levels = names(loadings),
      labels = names(loadings),
      ordered = TRUE
    );

  tmpDf$loadingLabel <-
    round(tmpDf$values, digits);

  tmpDf$absLoading <-
    abs(tmpDf$values);

  ### Create empty plot
  res <-
    ggplot2::ggplot(
      data = tmpDf,
      mapping = ggplot2::aes_string(
        x = "Factor",
        y = "Variable",
        color = 'absLoading',
        fill = 'absLoading')
    ) +
      ggplot2::scale_color_viridis_c(direction=-1,
                                     limits = c(0, 1)) +
      ggplot2::scale_fill_viridis_c(direction=-1,
                                    limits = c(0, 1)) +
      theme +
    do.call(
      ggplot2::labs,
      labs
    ) +
      do.call(
        ggplot2::theme,
        themeArgs
      );

  if (heatmap) {
    res <-
      res +
      ggplot2::geom_tile();
  } else {
    res <-
      res +
      ggplot2::geom_point(
        mapping = ggplot2::aes_string(size = 'absLoading')
      );
  }

  if (showLoadings) {
    res <-
      res +
      ggplot2::geom_text(
        mapping = ggplot2::aes_string(label = 'loadingLabel')
      );
  }

  return(res);
}
