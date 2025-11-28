#' Compute means and sums
#'
#' These functions allow easily computing means and sums. Note that if you
#' attach `rosetta` to the search path,
#'
#' @param ... The dataframe or vectors for which to compute the means or sums.
#' When passing a dataframe as unnamed argument (i.e. in the "dots", `...`),
#' the means or sums for all columns in the dataframe will be computed. If you
#' want to select one or more columns, make sure to pass the dataframe as `data`.
#' @param data If a dataframe is passed as `data`, the values passed in
#' the "dots" (`...`) will be taken as column names or indices in that
#' dataframe. This allows easy indexing.
#' @param requiredValidValues The number (if larger than 1) or proportion (if
#' between 0 and 1) of values that have to be valid (i.e. nonmissing) before the
#' mean or sum is returned.
#' @param returnIfInvalid Which value to return for rows not meeting the
#' criterion specified in `requiredValidValues`.
#' @param silent Whether to suppress messages.
#'
#' @return The means or sums.
#' @rdname meansAndSums
#' @export
#'
#' @examples rosetta::means(mtcars$mpg, mtcars$disp, mtcars$wt);
#' rosetta::means(data=mtcars, 'mpg', 'disp', 'wt');
#' rosetta::sums(mtcars$mpg, mtcars$disp, mtcars$wt);
#' rosetta::sums(data=mtcars, 'mpg', 'disp', 'wt');
means <- function(...,
                  data = NULL,
                  requiredValidValues = 0,
                  returnIfInvalid = NA,
                  silent = FALSE) {

  dotList <- list(...);
  if (is.data.frame(data)) {
    if (all(unlist(lapply(dotList, is.character)))) {
      if (all(unlist(dotList) %in% names(data))) {
        dat <- data[, unlist(dotList)];
      } else {
        stop("One or more specified variables/columns does not exist in the ",
             "dataframe you passed as `data`!");
      }
    } else if (all(unlist(lapply(dotList, is.numeric)))) {
      if ((min(unlist(dotList)) > 0) &&
          (max(unlist(dotList)) <= ncol(data))) {
        dat <- data[, dotList];
      } else {
        stop("You specified indices for columns that do not exist in the ",
             "dataframe you passed as `data`!");
      }
    }
  } else if ((length(dotList) == 1) && is.data.frame(dotList[[1]])) {
    dat <- dotList[[1]];
  } else if (length(unique(lapply(dotList, length)))==1) {
    dat <- as.data.frame(dotList);
  } else {
    stop("The vectors you provided do not have equal lengths! Either provide a dataframe or vectors of the same length.");
  }

  if (requiredValidValues == "all") {
    requiredValidValues <- ncol(dat);
  } else if (!is.numeric(requiredValidValues)) {
    stop("Argument 'requiredValidValues' must be numeric or 'all', ",
         "but it is not 'all' and has class ",
         class(requiredValidValues), ".");
  } else if (requiredValidValues == 0) {
    requiredValidValues = 0;
  } else if (requiredValidValues < 1) {
    requiredValidValuesPercentages <- requiredValidValues;
    requiredValidValues <- ceiling(requiredValidValuesPercentages * ncol(dat));
    if (!silent) {
      cat0("Argument 'requiredValidValues' was set to a proportion (",
           requiredValidValuesPercentages, "), so only computing a mean for cases ",
           "where at least that proportion of variables (i.e. ",
           100 * requiredValidValuesPercentages,
           "%, or ", requiredValidValues, " variables) have valid values.\n");
    }
  }
  nrOfValidValues <- rowSums(!is.na(dat)) >= requiredValidValues;

  return(
    ifelse(
      nrOfValidValues,
      rowMeans(dat, na.rm=TRUE),
      returnIfInvalid
    )
  );
}
