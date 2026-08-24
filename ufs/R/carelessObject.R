#' Compute diagnostics for careless responding
#'
#' This function is a wrapper for the functions from
#' the `careless` package. Normally, you'd probably
#' call `carelessReport` which calls this function
#' to generate a report of suspect participants.
#'
#' @param data The dataframe.
#' @param items The items to look at.
#' @param flagUnivar How extreme a score has to be for it
#' to be flagged as suspicous univariately.
#' @param flagMultivar This has not been implemented yet.
#' @param irvSplit Whether to split for the IRV, and if so,
#' in how many parts.
#' @param responseTime If not `NULL`, the name of a column
#' containing the participants' response times.
#'
#' @return An object of class `carelessObject`.
#' @export
#'
#' @examples carelessObject(mtcars);
carelessObject <- function(data,
                           items = names(data),
                           flagUnivar = .99,
                           flagMultivar = .95,
                           irvSplit = 4,
                           responseTime = NULL) {

  if (!requireNamespace("careless", quietly = TRUE)) {
    message("Package \"careless\" needed for this function to work. You can install it using:\n\n",
            "  install.packages('careless');\n");
    return(invisible(FALSE));
  }

  res <- list(input = as.list(environment()));

  ### Store results from the `careless` package functions
  res$carelessAnalyses <-
    list(longstring =
           careless::longstring(data[, items]),
         irv = careless::irv(data[, items],
                             split = ifelse(irvSplit>1, TRUE, FALSE),
                             num.split = irvSplit),
         mahalanobis = careless::mahad(data[, items],
                                       plot=FALSE,
                                       flag=FALSE)
    );

  ### Combine into one dataframe
  res$dat <-
    cbind(data.frame(longstring=res$carelessAnalyses$longstring),
          res$carelessAnalyses$irv,
          data.frame(mahalanobis=res$carelessAnalyses$mahalanobis),
          data.frame(responseTime = data[, responseTime]));

  if (!is.null(responseTime)) {
    res$dat <-
      cbind(res$dat,
            data.frame(responseTime = data[, responseTime]));
  }

  ### Add convenient list of all metrics
  res$convenience <- list(metrics = names(res$dat));

  ### Add probability corresponding to each score in each column
  tmpVarNames <-
    names(res$dat);

  tmpDf <-
    as.data.frame(lapply(1:ncol(res$dat),
                         function(i) {
                           ### Get empirical cumulative distribution function
                           tmpEcdf <- stats::ecdf(res$dat[, i]);
                           return(tmpEcdf(res$dat[, i]));
                         }));
  names(tmpDf) <-
    tmpProbVarNames <-
    paste0(tmpVarNames,
           "_prob");
  res$dat <-
    cbind(res$dat,
          tmpDf);

  ### Add logical vector to indicate univariate or multivariate
  ### flags
  tmpDf <-
    as.data.frame(lapply(tmpProbVarNames,
                         function(x) {
                           return(res$dat[, x] > flagUnivar);
                         }));
  names(tmpDf) <- paste0(tmpVarNames,
                         "_flagged");

  res$dat <-
    cbind(res$dat,
          tmpDf);

  class(res) <- 'carelessObject';

  return(res);

}
