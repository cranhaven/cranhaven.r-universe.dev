#' scatterMatrix
#'
#' scatterMatrix produces a matrix with jittered scatterplots, histograms, and
#' correlation coefficients.
#'
#' @param dat A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items is NULL.
#' @param items If not NULL, this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param itemLabels Optionally, labels to use for the items (optionally, named,
#' with the names corresponding to the `items`; otherwise, the order of the
#' labels has to match the order of the items)
#' @param plotSize Size of the final plot in millimeters.
#' @param sizeMultiplier Allows more flexible control over the size of the plot
#' elements
#' @param pointSize Size of the points in the scatterplots
#' @param axisLabels Passed to ggpairs function to set axisLabels.
#' @param normalHist Whether to use the default ggpairs histogram on the
#' diagonal of the scattermatrix, or whether to use the [normalHist()] version.
#' @param x The object to print.
#' @param progress Whether to show a progress bar; set to `FALSE` to disable. See
#' [GGally::ggpairs()] help for more information.
#' @param theme The ggplot2 theme to use.
#' @param hideGrid Whether to hide the gridlines in the plot.
#' @param conf.level The confidence level of confidence intervals
#' @param ...  Additional arguments for `scatterMatrix()` are passed on to
#' [normalHist()], and additional arguments for the `print` method are passed
#' on to the default `print` method.
#' @return
#'
#' An object with the input and several output variables. Most notably:
#' \item{output$scatterMatrix}{A scattermatrix with histograms on the diagonal
#' and correlation coefficients in the upper right half.}
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar
#' @rdname scatterMatrix
#' @examples
#'
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#'
#' ### Generate a datafile to use
#' exampleData <- data.frame(item1=rnorm(100));
#' exampleData$item2 <- exampleData$item1+rnorm(100);
#' exampleData$item3 <- exampleData$item1+rnorm(100);
#' exampleData$item4 <- exampleData$item2+rnorm(100);
#' exampleData$item5 <- exampleData$item2+rnorm(100);
#'
#' ### Use all items
#' scatterMatrix(dat=exampleData);
#' }
#'
#' @export
scatterMatrix <- function(dat, items=NULL,
                          itemLabels = NULL,
                          plotSize=180, sizeMultiplier = 1,
                          pointSize = 1,
                          axisLabels = "none", normalHist=TRUE,
                          progress = NULL,
                          theme = ggplot2::theme_minimal(),
                          hideGrid = TRUE,
                          conf.level = .95,
                          ...) {

  suppressMessages(suppressWarnings(
    if (!requireNamespace("GGally")) {
      message("You need the {GGally} package to use this function!\n\n",
              "You can install it with the following command:\n\n",
              "install.packages('GGally');");
      return(invisible(FALSE));
    }
  ));

  if (knitr::is_html_output() || knitr::is_latex_output()) {
    progress <- FALSE;
  }

  if (is.null(items)) {
    items <- names(dat);
  }

  if (is.null(itemLabels)) {
    itemLabels <- items;
  } else {
    if (length(itemLabels) != length(items)) {
      stop("The vector with item labels does not have the same length ",
           "as the vector with items!");
    }
  }

  if (is.null(names(itemLabels))) {
    names(itemLabels) <- items;
  } else {
    if (!(all(names(itemLabels) %in% items))) {
      stop("You passed a named vector with item labels, but not all ",
           "names correspond to items that you passed in `items`!");
    }
  }

  if (!all(items %in% names(dat))) {
    stop("Not all `items` you specified exist as columns in ",
         "the data frame you supplied as `data`!");
  }

  ### Generate object with 3 sub-objects to store input,
  ### intermediate results, and output
  res <- list(input = list(dat=dat,
                           items=items,
                           itemLabels=itemLabels,
                           plotSize=plotSize,
                           sizeMultiplier=sizeMultiplier,
                           axisLabels=axisLabels,
                           conf.level=conf.level),
              intermediate = list(),
              output = list());

  ### Extract dataframe and select only complete cases
  res$intermediate$dat <- dat[stats::complete.cases(dat[, items]), items];

  ### Convert all variables to numeric vectors, if they weren't already
  res$intermediate$dat <- data.frame(lapply(res$intermediate$dat, 'as.numeric'));

  ### The size of each panel in the scattermatrix depends
  ### on the number of items - therefore, we need to adjust
  ### the plot sizes to the number of items.
  res$intermediate$baseSize <- baseSize <-
    (sizeMultiplier * (plotSize / length(items))) / 100;

  res$intermediate$plotSettings <- plotSettings <-
    ggplot2::theme(axis.line = ggplot2::element_line(size = baseSize),
                   panel.grid.major = ggplot2::element_line(size = baseSize/2),
                   line = ggplot2::element_line(size = baseSize/2),
                   axis.ticks = ggplot2::element_line (size=baseSize/2)
    );

  ### Visual representation of bivariate correlations

  ### First generate a normal scattermatrix with histograms
  ### on the diagonal
  res$intermediate$ggpairs.normal <-
    GGally::ggpairs(
      res$intermediate$dat,
      title=paste0(
        "Scatter matrix with ",
        round(100 * conf.level, 2),
        "% confidence intervals"),
      diag=list(continuous="barDiag", discrete="barDiag"),
      upper=list(continuous=betterCor_for_ggally),
      columnLabels = itemLabels,
      switch="y", progress=progress,
      axisLabels=res$input$axisLabels
    );

  ### Then generate one with jittered points
  res$intermediate$ggpairs.jittered <-
    GGally::ggpairs(
      res$intermediate$dat,
      title=paste0(
        "Scatter matrix with ",
        round(100 * conf.level, 2),
        "% confidence intervals"),
      diag=list(continuous="blankDiag"),
      upper=list(continuous=GGally::wrap(betterCor_for_ggally,
                                         conf.level = conf.level)),
      lower=list(continuous=GGally::wrap("points",
                                         position="jitter",
                                         size=pointSize)),
      columnLabels = itemLabels,
      switch="y", progress=progress,
      axisLabels=res$input$axisLabels
    );

  ### Copy the the one with the jittered points
  res$intermediate$ggpairs.combined <- res$intermediate$ggpairs.jittered;

  if (normalHist) {
    ### Create histograms and add them to the combined plot
    res$intermediate$normalHists <- list();
    for (currentVar in 1:length(items)) {
      res$intermediate$normalHists[[items[currentVar]]] <-
        ufs::normalHist(res$intermediate$dat[[items[currentVar]]], ...);
      res$intermediate$ggpairs.combined <-
        GGally::putPlot(res$intermediate$ggpairs.combined,
                        res$intermediate$normalHists[[items[currentVar]]]$plot,
                        currentVar, currentVar);
    }
  } else {
    ### Then place the histograms from the 'normal' one
    ### on the diagonal of the jittered scattermatrix
    for (currentVar in 1:length(items)) {
      res$intermediate$ggpairs.combined <-
        GGally::putPlot(res$intermediate$ggpairs.combined,
                        GGally::getPlot(res$intermediate$ggpairs.normal, currentVar, currentVar),
                        currentVar, currentVar);
    }
  }

  ### Copy combined matrix to the output for final adjustments
  res$output$scatterMatrix <- res$intermediate$ggpairs.combined;

  ### Adjust the size of the plots
  for (currentRowFromTop in 1:length(items)) {
    for (currentColumnFromLeft in 1:length(items)) {
      res$output$scatterMatrix <-
        GGally::putPlot(res$output$scatterMatrix,
                        GGally::getPlot(res$output$scatterMatrix, currentRowFromTop, currentColumnFromLeft) + plotSettings,
                        currentRowFromTop, currentColumnFromLeft);
    }
  }

  res$output$scatterMatrix <-
    res$output$scatterMatrix +
    theme;

  if (hideGrid) {
    res$output$scatterMatrix <-
      res$output$scatterMatrix +
      ggplot2::theme(panel.grid=ggplot2::element_blank());
  }

  ### Set class and return result
  class(res) <- "scatterMatrix";
  return(res);

}

#' @method print scatterMatrix
#' @rdname scatterMatrix
#' @export
print.scatterMatrix <- function(x, ...) {
  ###
  print(x$output$scatterMatrix, ...);
}

betterCor_for_ggally <- function (data, mapping, ...,
                                  conf.level = .95) {
  return(
    GGally::ggally_statistic(
      data = data,
      mapping = mapping,
      align_percent = 0.5,
      display_grid = FALSE,
      title_args = list(...),
      group_args = list(...),
      justify_labels = "right",
      justify_text = "left",
      family = "sans",
      sep = "",
      title = "",
      text_fn =
        function(x, y) {
          if (!is.numeric(x)) {
            x <- ufs::convertToNumeric(x);
          }
          if (!is.numeric(y)) {
            y <- ufs::convertToNumeric(y);
          }
          cor <- cor(x, y);
          corObj <-
            ufs::confIntR(
              cor,
              N = sum(stats::complete.cases(data.frame(x, y))),
              conf.level = conf.level);
          txtRes <-
            paste0(
              "r = ",
              ufs::formatR(cor), "\n",
              ufs::formatCI(corObj, noZero=TRUE)
            );
          return(txtRes);
        }
    )
  );
}
