#' descr (or descriptives)
#'
#' This function provides a number of descriptives about your data, similar to
#' what SPSS's DESCRIPTIVES (often called with DESCR) does.
#'
#' Note that R (of course) has many similar functions, such as
#' \code{\link{summary}}, [psych::describe()] in the excellent
#' [psych::psych] package.
#'
#' The Hartigans' Dip Test may be unfamiliar to users; it is a measure of uni-
#' vs. multimodality, computed by the `dip.test()` function from the
#' `{diptest}` package from the. Depending on the sample size, values over
#' .025 can be seen as mildly indicative of multimodality, while values over
#' .05 probably warrant closer inspection (the p-value can be obtained using
#' that `dip.test()` function from `{diptest}`; also see Table 1 of
#' Hartigan & Hartigan (1985) for an indication as to critical values).
#'
#' @aliases descr descriptives
#' @param x The vector/variable for which to return descriptives, or the data
#' frame from which to get those variables/vectors.
#' @param items Optionally, if `x` is a data frame, the variable names for
#' which to produce the descriptives.
#' @param varLabels Optionally, a named vector with 'pretty labels' to show
#' for the variables. This has to be a vector of the same length as `items`,
#' and if it is not a named vector with the names corresponding to the
#' `items`, it has to be in the same order.
#' @param mean,meanCI,median,mode Whether to compute the mean, its
#' confidence interval, the median, and/or the mode (all logical, so `TRUE`
#' or `FALSE`).
#' @param var,sd,se Whether to compute the variance, standard deviation, and
#' standard error (all logical, so `TRUE` or `FALSE`).
#' @param min,max,q1,q3,IQR Whether to compute the minimum, maximum, first and
#' third quartile, and inter-quartile range (all logical, so `TRUE` or `FALSE`).
#' @param skewness,kurtosis,dip Whether to compute the skewness, kurtosis and
#' dip test (all logical, so `TRUE` or `FALSE`).
#' @param totalN,missingN,validN Whether to show the total sample size, the
#' number of missing values, and the number of valid (i.e. non-missing) values
#' (all logical, so `TRUE` or `FALSE`).
#' @param histogram,boxplot Whether to show a histogram and/or boxplot
#' @param digits The number of digits to round the results to when showing
#' them.
#' @param errorOnFactor,convertFactor If `errorOnFactor` is `TRUE`, factors
#' throw an error. If not, if `convertFactor` is TRUE, they will be
#' converted to numeric values using `as.numeric(as.character(x))`, and then
#' the same output will be generated as for numeric variables. If
#' `convertFactor` is false, the frequency table will be produced.
#' @param maxModes Maximum number of modes to display: displays "multi" if more
#' than this number of modes if found.
#' @param t Whether to transpose the dataframes when printing them to the
#' screen (this is easier for users relying on screen readers). **Note: this
#' functionality has not yet been implemented!**
#' @param conf.level Confidence of confidence interval around the mean in the
#' central tendency measures.
#' @param quantileType The type of quantiles to be used to compute the
#' interquartile range (IQR). See \code{\link{quantile}} for more information.
#' @param headingLevel The number of hashes to print in front of the headings
#' when printing while knitting
#' @param x The object to print (i.e. as produced by `descr`).
#' @param forceKnitrOutput Force knitr output.
#' @param maxPlotCols The maximum number of columns when plotting multiple
#' histograms and/or boxplots.
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available.
#' @param show A vector of elements to show in the results, based on the
#' arguments that activate/deactivate the descriptives (from `mean` to
#' `validN`).
#' @param \dots Any additional arguments are passed to the default print method
#' by the print method, and to [rmdpartials::partial()] when knitting an
#' RMarkdown partial.
#' @return A list of dataframes with the requested values.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{summary}}, [psych::describe()
#' @references Hartigan, J. A.; Hartigan, P. M. The Dip Test of Unimodality.
#' Ann. Statist. 13 (1985), no. 1, 70--84. doi:10.1214/aos/1176346577.
#' https://projecteuclid.org/euclid.aos/1176346577.
#' @keywords univariate
#' @rdname rosettaDescr
#' @examples ### Simplest example with default settings
#' descr(mtcars$mpg);
#'
#' ### Also requesting a histogram and boxplot
#' descr(mtcars$mpg, histogram=TRUE, boxplot=TRUE);
#'
#' ### To show the output as Rmd Partial in the viewer
#' rosetta::rosettaDescr_partial(
#'   rosetta::descr(
#'     mtcars$mpg
#'   )
#' );
#'
#' ### Multiple variables, including one factor
#' rosetta::rosettaDescr_partial(
#'   rosetta::descr(
#'     iris
#'   )
#' );
#'
#' @export descr
descr <- function(x,
                  items = names(x),
                  varLabels = NULL,
                  mean = TRUE,
                  meanCI = TRUE,
                  median = TRUE,
                  mode = TRUE,
                  var = TRUE,
                  sd = TRUE,
                  se = FALSE,
                  min = TRUE,
                  max = TRUE,
                  q1 = FALSE,
                  q3 = FALSE,
                  IQR = FALSE,
                  skewness = TRUE,
                  kurtosis = TRUE,
                  dip = TRUE,
                  totalN = TRUE,
                  missingN = TRUE,
                  validN = TRUE,
                  histogram = FALSE,
                  boxplot = FALSE,
                  digits=2,
                  errorOnFactor = FALSE,
                  convertFactor = FALSE,
                  maxModes = 1,
                  maxPlotCols = 4,
                  t=FALSE,
                  headingLevel = 3,
                  conf.level=.95,
                  quantileType = 2) {

  args <- as.list(environment());

  varName <- deparse(substitute(x));

  if (is.vector(x)) {
    items <- varName;
  }

  if (is.null(varLabels)) {
    varLabels <- items;
  }
  if (is.null(names(varLabels))) {
    varLabels <- stats::setNames(varLabels,
                                 items);
  }

  if (is.data.frame(x)) {

    res <-
      lapply(
        items,
        function(item) {

          doCallArgs <- args;
          doCallArgs$x <- x[, item];
          doCallArgs$headingLevel <- headingLevel + 1;
          doCallArgs$varLabels = varLabels[item];

          res <-
            do.call(
              descr,
              doCallArgs
            );

          if (is.data.frame(res)) {
            row.names(res) <- item;
          }

          return(res);
        }
      );
    names(res) <- items;

    attr(res, "originalArgs") <- args;
    attr(res, "maxPlotCols") <- maxPlotCols;
    attr(res, "datasetName") <- varName;
    attr(res, "digits") <- digits;
    attr(res, "show") <- attr(res[[1]], 'show');
    attr(res, "headingLevel") <- headingLevel;

    class(res) <- c("rosettaDescriptives",
                    "rosettaDescr",
                    class(res));

    return(res);

  }

    if (is.factor(x)) {
      if (errorOnFactor) {
        stop("The first argument (called 'x' in this function, you passed '",
             varName, "') is a factor, and you set 'errorOnFactor'",
             "to TRUE, so here is the error you requested.");
      } else if (convertFactor) {
        x <- as.numeric(as.character(x));
      } else {
        res <- freq(x);
        attr(res, "varName") <- varName;
        attr(res, "varLabel") <- varLabels;
        if (histogram) {
          attr(res, "histogram") <-
            ggBarChart(x);
        }
        if (boxplot) {
          attr(res, "boxplot") <-
            ggplot2::ggplot() +
            ggplot2::annotate(
              geom="text",
              x = 1, y = 1, size=4,
              label=paste0("(", varLabels,
                           "\nis a factor;\nno boxplot\navailable)")) +
            ggplot2::theme_void() +
            ggplot2::theme(axis.text = element_blank());
        }
        return(res);
      }
    } else if (!is.numeric(x)) {
      stop("The first argument (called 'x' in this function, you passed '",
           varName, "') is not a numeric vector (it has class '",
           class(x), "').");
    } else {

      nrNA <- sum(is.na(x));

      x <- stats::na.omit(x);

      mode <- modus(x);
      if (is.numeric(maxModes)) {
        mode <- ifelseObj(length(mode) > maxModes, "(multi)", mode);
      }
      if (length(mode) > 1) {
        mode <- vecTxt(mode);
      }

      meanCI <- ufs::formatCI(
        ufs::meanConfInt(
          x,
          conf.level=conf.level
        )$output$ci,
        digits = digits
      );

      q1 <- stats::quantile(x, type=quantileType)[2];
      q3 <- stats::quantile(x, type=quantileType)[4];

      suppressWarnings(dataShape <- ufs::dataShape(x, plots=FALSE));

      res <-
        data.frame(
          mean = mean(x),
          meanCI = meanCI,
          median = stats::median(x),
          mode = mode,
          var = stats::var(x),
          sd = stats::sd(x),
          iqr = q3 - q1,
          se = sqrt(stats::var(x)) / sqrt(length(x)),
          min = min(x),
          q1 = q1,
          q3 = q3,
          max = max(x),
          skewness = dataShape$output$skewness,
          kurtosis = dataShape$output$kurtosis,
          dip = dataShape$output$D.dip.test,
          totalN = length(x) + nrNA,
          missingN = nrNA,
          validN = length(x)
        );

      show <- names(res);
      showColArgs <- intersect(show, names(args));
      show <-
        showColArgs[which(unlist(args[showColArgs]))];

      if (!IQR) {
        show <- setdiff(show, "iqr");
      } else if (!("iqr" %in% show)) {
        show <- c(show, "iqr");
      }

      if (histogram) {
        attr(res, "histogram") <-
          ufs::normalHist(
            x,
            returnPlotOnly = TRUE
          );
      }

      if (boxplot) {
        attr(res, "boxplot") <- ggBoxplot(x);
      }

      row.names(res) <-
        ifelse(
          is.null(varLabels),
          varName,
          varLabels
        );

      attr(res, "maxPlotCols") <- maxPlotCols;
      attr(res, "varName") <- varName;
      attr(res, "varLabel") <- varLabels;
      attr(res, "digits") <- digits;
      attr(res, "conf.level") <- conf.level;
      attr(res, "transpose") <- t;
      attr(res, "show") <- show;
      attr(res, "headingLevel") <- headingLevel;

      class(res) <- c("rosettaDescr", class(res));

      return(res);
    }
  }

###-----------------------------------------------------------------------------

#' @rdname rosettaDescr
#' @export
rosettaDescr_partial <- function(x,
                                 digits = attr(x, "digits"),
                                 show = attr(x, "show"),
                                 headingLevel = attr(x, "headingLevel"),
                                 maxPlotCols = attr(x, "maxPlotCols"),
                                 echoPartial = FALSE,
                                 partialFile = NULL,
                                 quiet=TRUE,
                                 ...) {

  ### Get filename
  if (!is.null(partialFile) && file.exists(partialFile)) {
    rmdPartialFilename <-
      partialFile;
  } else {
    rmdPartialFilename <-
      system.file("partials", "_rosettaDescr_partial.Rmd",
                  package="rosetta");
  }

  rmdpartials::partial(rmdPartialFilename);

}

###-----------------------------------------------------------------------------

#' @rdname rosettaDescr
#' @method knit_print rosettaDescr
#' @importFrom knitr knit_print
#' @export
knit_print.rosettaDescr <- function(x,
                                    digits = attr(x, "digits"),
                                    show = attr(x, "show"),
                                    headingLevel = attr(x, "headingLevel"),
                                    maxPlotCols = attr(x, "maxPlotCols"),
                                    echoPartial = FALSE,
                                    partialFile = NULL,
                                    quiet=TRUE,
                                    ...) {
  rosettaDescr_partial(x = x,
                       headingLevel = headingLevel,
                       quiet = quiet,
                       echoPartial = echoPartial,
                       partialFile = partialFile,
                       digits = digits,
                       ...);
}

###-----------------------------------------------------------------------------

#' @method print rosettaDescr
#' @rdname rosettaDescr
#' @export
print.rosettaDescr <- function(x,
                               digits = attr(x, "digits"),
                               show = attr(x, "show"),
                               maxPlotCols = attr(x, "maxPlotCols"),
                               headingLevel = attr(x, "headingLevel"),
                               forceKnitrOutput = FALSE,
                               ...) {

  if (isTRUE(getOption('knitr.in.progress')) || forceKnitrOutput) {

    rosettaDescr_partial(x = x,
                         digits = digits,
                         headingLevel = headingLevel,
                         show = show,
                         labels = labels,
                         ...);

  } else {

    if ("rosettaDescriptives" %in% class(x)) {

      ### Descriptives for multiple variables

      if (!is.null(attr(x[[1]], 'histogram'))) {
        histograms <-
          lapply(names(x),
                 function(currentDescr) {
                   if (is.null(attr(x[[currentDescr]], "histogram"))) {
                     return(ggplot2::ggplot() + ggplot2::theme_void());
                   }
                   return(
                     attr(x[[currentDescr]], "histogram") +
                       ggplot2::labs(
                         y = NULL,
                         x = NULL,
                         title = currentDescr
                       )
                   );
                 });
        arrangedHistograms <-
          gridExtra::arrangeGrob(
            grobs=histograms,
            ncol=min(maxPlotCols, length(histograms))
          );
        grid::grid.newpage();
        grid::grid.draw(arrangedHistograms);
      }

      if (!is.null(attr(x[[1]], 'boxplot'))) {
        boxplots <-
          lapply(names(x),
                 function(currentDescr) {
                   if (is.null(attr(x[[currentDescr]], "boxplot"))) {
                     return(ggplot2::ggplot() + ggplot2::theme_void());
                   }
                   return(
                     attr(x[[currentDescr]], "boxplot") +
                       ggplot2::labs(
                         y = NULL,
                         x = NULL,
                         title = currentDescr
                       )
                   );
                 });
        arrangedBoxplots <-
          gridExtra::arrangeGrob(
            grobs=boxplots,
            ncol=min(maxPlotCols, length(boxplots))
          );
        grid::grid.newpage();
        grid::grid.draw(arrangedBoxplots);
      }

      numericResults <-
        unlist(
          lapply(
            x,
            function(descrRes) {
              return("rosettaDescr" %in% class(descrRes));
            }
          )
        );

      if (any(numericResults)) {

        res <-
          do.call(
            rbind,
            lapply(
              x[numericResults],
              function(numericDescr)
                return(
                  as.data.frame(numericDescr)
                )
            )
          );

        colsToRound <-
          which(
            unlist(
              lapply(
                names(res),
                function(colName) {
                  return(
                    (!grepl('N$', colName)) &
                      is.numeric(res[, colName])
                  )
                }
              )
            )
          );

        res[, colsToRound] <-
          round(res[, colsToRound],
                digits);

        cat0("\nDescriptives for variables in data frame ",
                  attr(x, "datasetName"), "\n\n");

        print(res[, show]);

      }

      if (any(!numericResults)) {

        for (i in names(x[!numericResults])) {

          if ("freq" %in% class(x[[i]])) {
            cat0("\nFrequencies for ", i, "\n\n");
            print(x[[i]]$dat);
          } else {
            print(x[[i]]);
          }

        }

      }

    } else {

      ### Descriptives for a single variable

      labels <-
        c(mean = "Mean",
          median = "Median",
          mode = "Mode",
          var = "Variance",
          sd = "Standard Deviation",
          se = "Standard Error",
          min = "Minimum",
          max = "Maximum",
          q1 = "First quartile",
          q3 = "Third quartile",
          IQR = "Interquartile range",
          skewness = "Skewness",
          kurtosis = "Kurtosis",
          dip = "Dip test",
          meanCI = paste0(100 * attr(x, 'conf.level'), "% Conf. Interval"),
          totalN = "Total sample size",
          missingN = "Missing values",
          validN = "Valid sample size"
        );

      cat(paste0("Descriptives for ", row.names(x), "\n\n"));

      for (i in intersect(show, names(x))) {
        if (is.character(x[, i])) {
          cat(sprintf(paste0("%20s :  %s\n"), labels[i], x[, i]));
        } else {
          minusSpace <- ifelse(x[, i] < 0, "", " ");
          if (grepl('N$', i)) {
            cat(sprintf(paste0("%20s : %s%s\n"), labels[i], minusSpace, x[, i]));
          } else {
            cat(sprintf(paste0("%20s : %s%.", digits, "f\n"), labels[i], minusSpace, x[, i]));
          }
        }
      }

    }

    return(invisible(x));

  }

}

