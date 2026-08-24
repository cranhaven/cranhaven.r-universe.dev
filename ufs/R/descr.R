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
#' vs. multidimensionality, computed by [diptest::dip.test()] from the
#' `dip.test` package. Depending on the sample size, values over
#' .025 can be seen as mildly indicative of multimodality, while values over
#' .05 probably warrant closer inspection (the p-value can be obtained using
#' [diptest::dip.test()]; also see Table 1 of Hartigan & Hartigan (1985) for
#' an indication as to critical values).
#'
#' @aliases descr descriptives
#' @param x The vector for which to return descriptives.
#' @param digits The number of digits to round the results to when showing
#' them.
#' @param errorOnFactor Whether to show an error when the vector is a factor,
#' or just show the frequencies instead.
#' @param include Which elements to include when showing the results.
#' @param maxModes Maximum number of modes to display: displays "multi" if more
#' than this number of modes if found.
#' @param t Whether to transpose the dataframes when printing them to the
#' screen (this is easier for users relying on screen readers).
#' @param conf.level Confidence of confidence interval around the mean in the
#' central tendency measures.
#' @param quantileType The type of quantiles to be used to compute the
#' interquartile range (IQR). See \code{\link{quantile}} for more information.
#' @param row.names Whether to show row names (`TRUE`) or not (`FALSE`).
#' @param headerPrefix The prefix for the heading; can be used to insert
#' hashes (`#`) to create Markdown headings.
#' @param headerStyle A string to insert before and after the heading (to
#' make stuff bold or italic in Markdown).
#' @param optional Provided for compatibility with the default [as.data.frame()]
#' method - see that help page for details.
#' @param ... Additional arguments are passed to the default `print` and
#' `pander` methods.
#' @return A list of dataframes with the requested values.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @seealso \code{\link{summary}}, [psych::describe()]
#' @references Hartigan, J. A.; Hartigan, P. M. The Dip Test of Unimodality.
#' Ann. Statist. 13 (1985), no. 1, 70--84. doi:10.1214/aos/1176346577.
#' https://projecteuclid.org/euclid.aos/1176346577.
#' @keywords univariate
#' @rdname descriptives
#' @examples
#'
#' descr(mtcars$mpg);
#'
#' @export descr
# #' @export descriptives
descr <-
#  descriptives <-
    function (x, digits=4, errorOnFactor = FALSE,
              include=c("central tendency", "spread",
                        "range", "distribution shape", "sample size"),
              maxModes = 1,
              t=FALSE, conf.level=.95,
              quantileType = 2) {
      UseMethod("descr", x)
    }

# #' @export descr.default
# #' @export descriptives.default
# #' @method generic descr
# #' @method generic descriptives
#' @export
#' @rdname descriptives
descr.default <-
#  descriptives.default <-
    function(x, digits=4, errorOnFactor = FALSE,
             include=c("central tendency", "spread",
                       "range", "distribution shape", "sample size"),
             maxModes = 1,
             t=FALSE, conf.level=.95,
             quantileType = 2) {
  varName <- deparse(substitute(x));
  if (is.factor(x)) {
    if (errorOnFactor) {
      stop("The first argument (called 'x' in this function, you passed '",
           varName, "') is a factor, and you set 'errorOnFactor'",
           "to TRUE, so here is the error you requested.");
    } else {
      return(freq(x));
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

    meanCi <- formatCI(meanConfInt(x, conf.level=conf.level)$output$ci);

    res <- list("central tendency" = data.frame(mean = mean(x),
                                     median = stats::median(x),
                                     mode = mode,
                                     `meanCI` = meanCi),
                spread = data.frame(var = stats::var(x),
                                    sd = stats::sd(x),
                                    iqr = stats::quantile(x, type=quantileType)[4] -
                                      stats::quantile(x, type=quantileType)[2],
                                    se = sqrt(stats::var(x)) / sqrt(length(x))),
                range = data.frame(min = min(x),
                                   q1 = stats::median(x[x < stats::median(x)]),
                                   q3 = stats::median(x[x > stats::median(x)]),
                                   max = max(x)),
                "distribution shape" = data.frame(skewness = suppressWarnings(dataShape(x)$output$skewness),
                                   kurtosis = suppressWarnings(dataShape(x)$output$kurtosis),
                                   dip = diptest::dip.test(x)$statistic[[1]]),
                "sample size" = data.frame(total = length(x) + nrNA,
                                  "NA" = nrNA,
                                  valid = length(x)));
    names(res[['central tendency']])[4] <- paste0(conf.level * 100, '% CI mean');
    row.names(res$spread) <- NULL;
    attr(res, "varName") <- varName;
    attr(res, "digits") <- digits;
    attr(res, "include") <- include;
    attr(res, "transpose") <- t;
    class(res) <- "descr";
    return(res);
  }
}

#' @method print descr
#' @rdname descriptives
#' @export
print.descr <- function(x, digits = attr(x, 'digits'),
                        t = attr(x, 'transpose'),
                        row.names = FALSE, ...) {
  cat("###### Descriptives for", attr(x, "varName"), "\n\n");
  for (current in attr(x, "include")) {
    cat0("Describing the ", current, ":\n");
    if (t) {
      df <- t(x[[current]]);
      colnames(df) <- '';
      print(df, digits=digits, row.names=row.names, ...);
    } else {
      print(x[[current]], digits=digits, row.names=row.names, ...);
    }
    cat("\n");
  }
  if ('shape' %in% names(x)) {
    cat("(You can use the functions 'dataShape' and",
        "'normalityAssessment' to explore the distribution shape",
        "more in depth.)");
  }
  invisible();
}

### Function to smoothly pander descriptives from userfriendlyscience
#' @method pander descr
#' @rdname descriptives
#' @export
pander.descr <- function(x, headerPrefix = "",
                         headerStyle = "**", ...) {
  #pander(cat0(unlist(lapply(x, pander, ...)), sep="\n"));
  pander(cat0(unlist(lapply(1:length(x), function(index) {
    cat0("\n\n", headerPrefix, headerStyle,
         "Describing the ", names(x)[index], ":",
         headerStyle, "\n\n");
    pander(x[[index]]);
  }, ...)), sep="\n"));
}

#' @method as.data.frame descr
#' @rdname descriptives
#' @export
as.data.frame.descr <- function(x, row.names=NULL,
                                optional=FALSE, ...) {

  if (!is.null(attr(x, 'digits'))) {
    res <-
      unlist(lapply(
        x,
        function(valList) {
          return(
            unlist(
              lapply(
                valList,
                function(val) {
                  if (is.numeric(val)) {
                    return(round(val, attr(x, 'digits')));
                  } else {
                    return(val);
                  }
                }
              )
            )
          );
        }
      ));
  } else {
    res <-
      unlist(x);
  }
  res <- res[c(1,2,5,6,7,8,9,10,11,12,13,14,15,16,17,18)];
  res <- as.data.frame(t(matrix(res)),
                       row.names=row.names,
                       optional=optional,
                       ...);
  names(res) <- c('mean', 'median', 'var', 'sd', 'IQR', 'se',
                  'min', 'q1', 'q3', 'max', 'skew', 'kurt', 'dip',
                  'n', 'NA', 'valid');
  return(res);
}

#' @method descr data.frame
#' @rdname descriptives
#' @export
descr.data.frame <-
  descriptives.data.frame <-
    function(x, ...) {
  return(do.call(rbind,
                 lapply(x,
                        function(currentCol) {
                          return(as.data.frame(descr(convertToNumeric(currentCol)),
                                               ...));
                        })));
}


