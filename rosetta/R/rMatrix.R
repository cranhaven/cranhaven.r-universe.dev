#' Correlation matrix
#'
#' rMatrix provides a correlation matrix with confidence intervals and a
#' p-value adjusted for multiple testing.
#'
#' rMatrix provides a symmetric or asymmetric matrix of correlations, their
#' confidence intervals, and p-values. The p-values can be corrected for
#' multiple testing.
#'
#' @param dat A dataframe containing the relevant variables.
#' @param x Vector of 1+ variable names.
#' @param y Vector of 1+ variable names; if this is left empty, a symmetric
#' matrix is created; if this is filled, the matrix will have the x variables
#' defining the rows and the y variables defining the columns.
#' @param conf.level The confidence of the confidence intervals.
#' @param correction Correction for multiple testing: an element out of the
#' vector c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
#' "none"). NOTE: the p-values are corrected for multiple testing; The
#' confidence intervals are not (yet :-)).
#' @param digits With what precision do you want the results to print.
#' @param pValueDigits Determines the number of digits to use when displaying p
#' values. P-values that are too small will be shown as p<.001 or p<.00001 etc.
#' @param colspace Number of spaces between columns
#' @param rowspace Number of rows between table rows (note: one table row is 2
#' rows).
#' @param colNames colNames can be "numbers" or "names". "Names" cause
#' variables names to be printed in the heading; "numbers" causes the rows to
#' become numbered and the numbers to be printed in the heading.
#' @param ... Additional arguments are ignored.
#'
#' @return An `rMatrix` object that when printed shows the correlation matrix
#'
#' An object with the input and several output variables. Most notably a number
#' of matrices: \item{r}{Pearson r values.} \item{parameter}{Degrees of
#' freedom.} \item{ci.lo}{Lower bound of Pearson r confidence interval.}
#' \item{ci.hi}{Upper bound of Pearson r confidence interval.}
#' \item{p.raw}{Original p-values.} \item{p.adj}{p-values adjusted for multiple
#' testing.}
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#'
#'
#' rMatrix(mtcars, x=c('disp', 'hp', 'drat'))
#'
#'
#' @export rMatrix
rMatrix <- function(dat, x, y=NULL, conf.level = .95, correction = "fdr",
                    digits = 2, pValueDigits=3, colspace=2, rowspace=0,
                    colNames ="numbers") {

  ### Check whether the first vector of vectors has
  if (length(x) < 1) {
    stop(paste0("Error: x vector has 0 elements or less; ",
                "make sure to specify at least one variable name!."));
  }

  ### Check whether we have a second set of variables
  if (!is.null(y)) {
    if (length(y) < 1) {
      stop(paste0("Error: y vector has 0 elements or less; ",
                  "make sure to specify at least one variable name!."));
    }
    symmetric <- FALSE;
  }
  else {
    y <- x;
    symmetric <- TRUE;
  }

  ### Create object to return, and store variable names, confidence of
  ### confidence interval, and digits
  res <- list();
  res$variables.rows <-x;
  res$variables.cols <- y;
  res$ci.confidence <- conf.level;
  res$correction <- correction;
  res$digits <- digits;
  res$pValueDigits <- pValueDigits;
  res$colspace <- colspace;
  res$rowspace <- rowspace;
  res$colNames <- colNames;

  res$r <- matrix(nrow = length(x), ncol = length(y));
  res$parameter <- matrix(nrow = length(x), ncol = length(y));
  res$ci.lo <- matrix(nrow = length(x), ncol = length(y));
  res$ci.hi <- matrix(nrow = length(x), ncol = length(y));
  res$p.raw <- matrix(nrow = length(x), ncol = length(y));
  res$p.adj <- matrix(nrow = length(x), ncol = length(y));

  xCounter <- 1;
  for(curXvar in x) {
    yCounter <- 1;
    for(curYvar in y) {
      curTest <- stats::cor.test(
        dat[,curXvar], dat[,curYvar],
        use="complete.obs",
        conf.level = conf.level);
      res$r[xCounter, yCounter] <- curTest$estimate;
      res$parameter[xCounter, yCounter] <- curTest$parameter;
      res$ci.lo[xCounter, yCounter] <- curTest$conf.int[1];
      res$ci.hi[xCounter, yCounter] <- curTest$conf.int[2];
      res$p.raw[xCounter, yCounter] <- curTest$p.value;
      yCounter <- yCounter + 1;
    }
    xCounter <- xCounter + 1;
  }

  ### If a symmetric table was requested, remove half
  ### the correlations and the diagonal
  if (symmetric) {
    ### Remove lower half of the matrices
    res$r[lower.tri(res$r)] = NA;
    res$parameter[lower.tri(res$parameter)] = NA;
    res$ci.lo[lower.tri(res$ci.lo)] = NA;
    res$ci.hi[lower.tri(res$ci.hi)] = NA;
    res$p.raw[lower.tri(res$p.raw)] = NA;
    ### Remove diagonal in matrices
    for(diagonal in c(1:nrow(res$r))) {
      res$r[diagonal, diagonal] <- NA;
      res$parameter[diagonal, diagonal] <- NA;
      res$ci.lo[diagonal, diagonal] <- NA;
      res$ci.hi[diagonal, diagonal] <- NA;
      res$p.raw[diagonal, diagonal] <- NA;
    }
  }

  ### Correct p-values for multiple testing
  res$p.adj <- matrix(
    stats::p.adjust(
      res$p.raw,
      method=correction),
    nrow(res$p.raw),
    ncol(res$p.raw)
  );

  ### Set row and column names
  rownames(res$r) <- x;         colnames(res$r) <- y;
  rownames(res$parameter) <- x; colnames(res$parameter) <- y;
  rownames(res$ci.lo) <- x;     colnames(res$ci.lo) <- y;
  rownames(res$ci.hi) <- x;     colnames(res$ci.hi) <- y;
  rownames(res$p.raw) <- x;     colnames(res$p.raw) <- y;
  rownames(res$p.adj) <- x;     colnames(res$p.adj) <- y;

  res$r <- t(res$r);
  res$parameter <- t(res$parameter);
  res$ci.lo <- t(res$ci.lo);
  res$ci.hi <- t(res$ci.hi);
  res$p.raw <- t(res$p.raw);
  res$p.adj <- t(res$p.adj);

  res$asMatrix <-
    matrix(
      paste0("r = [", ufs::noZero(round(res$ci.lo, digits)),
             "; ", ufs::noZero(round(res$ci.hi, digits)),
             "]<br/ >r(", res$parameter, ") = ",
             ufs::noZero(round(res$r, digits)), ", ",
             ufs::formatPvalue(res$p.adj)),
      ncol=ncol(res$r));

  res$asMatrix <-
    ifelse(
      is.na(res$r),
      "",
      res$asMatrix
    );

  ### Set class & return result
  class(res) <- "rMatrix";
  return(res);
}

#' @method print rMatrix
#' @export
#' @rdname rMatrix
print.rMatrix <- function (x, digits=x$digits,
                           pValueDigits = x$pValueDigits,
                           colNames = x$colNames, ...) {

  ### We want multiple lines per cell, so we'll need to print manually.
  ### We first print the confidence interval on the first line; then,
  ### on the next line, the point estimate and the p-value (corrected
  ### for multiple testing).
  ###
  ### Compute how wide the columns should be. This depends on
  ### 1) the width of the confidence intervals, and 2) the width
  ### of the variable name in each column
  ### The maximum length of confidence intervals is:
  ### [-.X; -.X]
  ### Where the number of X's is determined by digits.
  ### Thus, 8 + digits * 2 represents the max length of
  ### confidence interval.
  maxConfIntLength <- 8 + digits * 2;

  ### Compute max length of second row
  max2ndRowLength <- 4 + digits + 5 + pValueDigits;

  ### set widest content
  widestContent <- max(maxConfIntLength, max2ndRowLength);

  if (colNames=="numbers") {
    ### The columns contain numbers instead of names;
    ### calculate the max width of these numbers
    numColSize <- nchar(length(x$variables.rows)) + x$colspace;
    colSizes <- rep(numColSize, length(x$variables.cols));
  }
  else {
    ### Otherwise, for each column, store the length of the variable name
    ### of that column.
    colSizes <- nchar(x$variables.cols);
  }

  ### Then, compare these to the maxConfIntLength, and store the
  ### larger of the two
  colSizes <- ifelse(colSizes > widestContent, colSizes, widestContent);

  ### If pval is TRUE, we use the p-value function to format the p-values.
  ### This means that the columns need to be three characters wider, in case
  ### we'll need the scientific notation somewhere.
  #     if(x$pval) {
  #       colSizes <- colSizes + 3;
  #     }
  ### NOTE: obsolete as of 2015-04-15 (version 0.2-3), as formatPvalue is used now

  if (colNames=="numbers") {
    ### Print spaces in first cell of first row as wide as
    ### the widest number
    cat(repeatStr(' ', numColSize + x$colspace));
  }

  ### First print column names. This, however, requires knowing
  ### how long the row names are going to be, so first look for
  ### the longest row name and get its length; add one as
  ### separation between the columns; and then print that
  ### number of spaces.
  leftColSize <- max(nchar(x$variables.rows)) + x$colspace;
  cat(repeatStr(" ", leftColSize));

  ### We'll need to print the column names with a loop (see
  ### explanation below)
  for(j in (1:length(x$variables.cols))) {
    if (colNames=="numbers") {
      ### Print column number
      cat(paste0(j, repeatStr(' ', colSizes[j] - nchar(j) + x$colspace)));
    } else {
      ### Print the column name
      cat(x$variables.cols[j]);
      ### Print trailing spaces (+x$colspace to have space between columns)
      cat(repeatStr(" ", colSizes[j] - nchar(x$variables.cols[j]) + x$colspace));
    }
  }

  ### Print newline character
  cat("\n");

  ### Now we'll start printing the rows, starting with the variable
  ### name and the confidence interval.
  for(i in (1:length(x$variables.rows))) {

    if (colNames=="numbers") {
      ### Print row number
      cat(paste0(repeatStr(' ', numColSize - nchar(i)), i, repeatStr(' ', x$colspace)));
    }

    ### Print variable name for this row
    cat(x$variables.rows[i]);

    ### Print spaces needed to line up second column
    cat(repeatStr(" ", leftColSize - nchar(x$variables.rows[i])));

    ### Now we need two loops (one for each line) to create the cells.
    ### Normally, we could provide paste0 (or paste) with a vector,
    ### and it would concatenate the elements for us, but in this case,
    ### every column can have a different width, so we need a different
    ### number of leading spaces.

    ### First, the confidence intervals
    for(j in (1:length(x$variables.cols))) {
      ### If the point estimate is NA, don't display anything
      if (is.na(x$r[i,j])) {
        cat(repeatStr(" ", colSizes[j] + x$colspace));
      }
      else {
        ### Create confidence interval for this column
        confInt <- paste0("[", formatR(x$ci.lo[i,j], digits), "; ", formatR(x$ci.hi[i,j], digits), "]");
        ### Print confidence interval
        cat(confInt);
        ### Print trailing spaces (+ x$colspace to have space between columns)
        cat(repeatStr(" ", colSizes[j] - nchar(confInt) + x$colspace));
      }
    }

    ### Print newline character
    cat("\n");

    if (colNames=="numbers") {
      ### Print spaces of width of longest row number
      cat(paste0(repeatStr(' ', numColSize + x$colspace)));
    }

    ### Start in second column
    cat(repeatStr(" ", leftColSize));

    ### Then, the point estimate and p-value
    for(j in (1:length(x$variables.cols))) {
      ### If the point estimate is NA, don't display anything
      if (is.na(x$r[i,j])) {
        cat(repeatStr(" ", colSizes[j] + x$colspace));
      }
      else {
        ### Create r & p
        content <- paste0("r=", formatR(x$r[i,j], digits), ", ",
                          formatPvalue(x$p.adj[i,j], digits=pValueDigits,
                                       spaces=FALSE));
        ### Print point estimate and p-value
        cat(content);
        ### Print trailing spaces (+x$colspace to have space between columns)
        cat(repeatStr(" ", colSizes[j] - nchar(content) + x$colspace));
      }
    }

    ### Print newline character
    cat("\n");

    ### x$rowspace indicated how many empty rows should be printed between
    ### every table row
    if (x$rowspace > 1) {
      for(i in c(1:x$rowspace)) {
        cat("\n");
      }
    }

  }

  print(
    kableExtra::kable_styling(
      kableExtra::kbl(
        x$asMatrix,
        escape = FALSE
      )
    )
  );

  return(invisible(x));
}

