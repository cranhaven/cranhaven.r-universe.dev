#' Convenience functions for ggplots based on multiple variables
#'
#' These are convenience functions to quickly generate plots for multiple
#' variables, with the variables in the y axis.
#'
#'
#' @aliases ggEasyPlots ggEasyRidge ggEasyBar
#' @param data The dataframe containing the variables.
#' @param items The variable names (if not provided, all variables will be
#' used).
#' @param labels Labels can optionally be provided; if they are, these will be
#' used instead of the variable names.
#' @param sortByMean Whether to sort the variables by mean value.
#' @param xlab,ylab The labels for the x and y axes.
#' @param scale_fill_function The function to pass to [ggplot2::ggplot()] to
#' provide the colors of the bars. If `NULL`, set to
#' `ggplot2::scale_fill_viridis_d(labels = legendValueLabels,
#' guide = ggplot2::guide_legend(title = NULL, nrow=legendRows, byrow=TRUE))`.
#' @param fontColor,fontSize The color and size of the font used to display the
#' labels
#' @param labelMinPercentage The minimum percentage that a category must reach
#' before the label is printed (in whole percentages, i.e., on a scale from 0
#' to 100).
#' @param showInLegend What to show in the legend in addition to the values;
#' nothing ("`none`"), the frequencies ("`freq`"), the percentages
#' ("`perc`"), or both ("`both`"). This is only used if only one
#' variable is shown in the plot; afterwise, after all, the absolute
#' frequencies and percentages differ for each variable.
#' @param legendRows Number or rows in the legend.
#' @param legendValueLabels Labels to use in the legend; must be a vector of
#' the same length as the number of categories in the variables.
#' @param biAxisLabels This can be used to specify labels to use if you want to
#' use labels on both the left and right side. This is mostly useful when
#' plotting single questions or semantic differentials. This must be a list
#' with two character vectors, `leftAnchors` and `rightAnchors`,
#' which must each have the same length as the number of items specified in
#' `items`. See the examples for, well, examples.
#' @return A [ggplot2::ggplot()] plot is returned.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @seealso [ggridges::geom_ridgeline()], [ggplot2::geom_bar()]
#' @keywords hplot
#' @rdname ggEasyPlots
#' @examples
#'
#' ggEasyBar(mtcars, c('gear', 'carb'));
#' ggEasyRidge(mtcars, c('disp', 'hp'));
#'
#' ### When plotting single questions, if you want to show the anchors:
#' ggEasyBar(mtcars, c('gear'),
#'           biAxisLabels=list(leftAnchors="Fewer",
#'                             rightAnchors="More"));
#'
#' ### Or for multiple questions (for e.g. semantic differentials):
#' ggEasyBar(mtcars, c('gear', 'carb'),
#'           biAxisLabels=list(leftAnchors=c("Fewer", "Lesser"),
#'                             rightAnchors=c("More", "Greater")));
#' @export
ggEasyBar <- function(data, items = NULL,
                      labels = NULL, sortByMean = TRUE,
                      xlab = NULL, ylab = NULL,
                      scale_fill_function = NULL,
                      fontColor = "white",
                      fontSize = 2,
                      labelMinPercentage = 1,
                      showInLegend = "both",
                      legendRows=2,
                      legendValueLabels=NULL,
                      biAxisLabels = NULL) {

  if (is.null(items)) {
    items <- names(data);
  }

  if (!all(items %in% names(data))) {
    stop("You specified items that do not exist in the data you provided (specifically, ",
         vecTxtQ(items[!items %in% names(data)]), ").");
  }

  if (sortByMean && length(items) > 1) {
    if (!all(unlist(lapply(data[, items], is.numeric)))) {
      lvls <- lapply(data[, items], levels)
      data[, items] <- massConvertToNumeric(data[, items],
                                            ignoreCharacter=FALSE);
    }
    tmpVarOrder <- order(colMeans(data[, items],
                                  na.rm=TRUE),
                         decreasing=TRUE);
  } else {
    tmpVarOrder <- 1:length(items);
  }

  if (is.null(labels)) {
    labels <- items;
  }

  ### Get frequencies and store them
  tmpDf <-
    lapply(data[, items, drop=FALSE], function(x)
      return(cbind(table(x), table(x) / sum(table(x)))));

  tmpDf <-
    lapply(names(tmpDf),
           function(x) return(data.frame(var = rep(x, nrow(tmpDf[[x]])),
                                         val = rownames(tmpDf[[x]]),
                                         abs = tmpDf[[x]][, 1],
                                         rel = 100 * tmpDf[[x]][, 2])));

  tmpDf <- do.call(rbind, tmpDf);
  ### Convert row names to numeric if need be
  if (!is.numeric(tmpDf$val)) {
    if (all(grepl('^\\d+$', tmpDf$val))) {
      if (is.factor(tmpDf$val)) {
        tmpDf$val <- as.numeric(levels(tmpDf$val))[tmpDf$val];
      } else {
        tmpDf$val <- as.numeric(tmpDf$val);
      }
    }
  }

  if (is.numeric(tmpDf$val) || (all(grepl('^\\d+$', tmpDf$val)))) {
    tmpDf$val <- factor(tmpDf$val,
                        levels = sort(as.numeric(unique(tmpDf$val))),
                        labels = sort(as.numeric(unique(tmpDf$val))),
                        ordered=TRUE);
  } else {
    tmpDf$val <- factor(tmpDf$val,
                        levels = tmpDf$val,
                        labels = tmpDf$val,
                        ordered=TRUE);
  }

  tmpDf$var <- factor(tmpDf$var,
                      levels=items[tmpVarOrder],
                      labels=labels[tmpVarOrder],
                      ordered=TRUE);

  if ((nrow(tmpDf) == 1) && (showInLegend == "both")) {
    tmpDf$val <- paste0(tmpDf$val, " (", tmpDf$abs, "; ", round(tmpDf$rel), "%)");
  } else if ((nrow(tmpDf) == 1) && (showInLegend == "perc")) {
    tmpDf$val <- paste0(tmpDf$val, " (", round(tmpDf$rel), "%)");
  } else if ((nrow(tmpDf) == 1) && (showInLegend == "freq")) {
    tmpDf$val <- paste0(tmpDf$val, " (", tmpDf$abs, ")");
  }

  tmpDf$label <- ifelse(round(tmpDf$rel) >= labelMinPercentage,
                        paste0(tmpDf$abs,
                               "\n(",
                               round(tmpDf$rel),
                               "%)"),
                        "");

  if (is.null(scale_fill_function)) {
    if (is.null(legendValueLabels)) {
      scale_fill_function <-
        ggplot2::scale_fill_viridis_d(guide = ggplot2::guide_legend(title = NULL,
                                                                    nrow=legendRows,
                                                                    byrow=TRUE));
    } else {
      scale_fill_function <-
        ggplot2::scale_fill_viridis_d(labels = legendValueLabels,
                                      guide = ggplot2::guide_legend(title = NULL,
                                                                    nrow=legendRows,
                                                                    byrow=TRUE));
    }
  }

  ### Actual plot
  if (!is.null(biAxisLabels) &&
      (length(biAxisLabels$leftAnchors) == length(items)) &&
      (length(biAxisLabels$rightAnchors) == length(items))) {
    res <-
      ggplot2::ggplot(data = tmpDf,
                      mapping = ggplot2::aes_string(x = rep(1:length(items),
                                                            table(tmpDf$var)),
                                                    y = 'rel',
                                                    fill = 'val',
                                                    label = 'label')) +
      ggplot2::scale_x_continuous(breaks=1:length(items),
                                  labels = biAxisLabels$leftAnchors,
                                  sec.axis=ggplot2::dup_axis(labels=biAxisLabels$rightAnchors));
  } else {
    res <-
      ggplot2::ggplot(data = tmpDf,
                      mapping = ggplot2::aes_string(x = 'var',
                                                    y = 'rel',
                                                    fill = 'val',
                                                    label = 'label'));
  }

  res <- res +
    ggplot2::geom_bar(na.rm=TRUE, stat = 'identity',
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::geom_text(color=fontColor, size = fontSize,
                       position = ggplot2::position_stack(reverse=TRUE,
                                                          vjust = 0.5)) +
    scale_fill_function +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme(legend.position="bottom");

  return(res);
}
