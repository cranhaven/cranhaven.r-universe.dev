#' Split a dataset into two parallel halves
#'
#' @param dat The dataframe
#' @param convertToNumeric Whether to first convert all columns to numeric
#' @param digits The number of digits to round to
#' @param x The object to print
#' @param ... Ignored.
#'
#' @return A `parallelSubscales` object that contains the new data frames,
#' and when printed shows the descriptives; or, for the print function, `x`,
#' invisibly.
#'
#' @rdname parallelSubscales
#' @export
parallelSubscales <- function(dat, convertToNumeric = TRUE) {
  res <- list(input = list(dat = dat),
              intermediate = list(),
              output = list());

  if (!is.list(dat)) {
    stop("Argument 'dat' must be a dataframe or a list of dataframes!");
  }

  if (is.data.frame(dat)) {
    dat2 <- list();
    dat2[[deparse(substitute(dat))]] <- dat;
    dat <- dat2;
    rm(dat2);
  }

  res$intermediate$dat <- dat;

  ### Cycle through dataframes and average items over
  ### datapoints/observations/participants/records/rows
  ### and compute variances
  res$intermediate$itemMeans <- list();
  res$intermediate$itemVariances <- list();
  for (currentDf in 1:length(dat)) {
    res$intermediate$itemMeans[[currentDf]] <-
      colMeans(dat[[currentDf]], na.rm=TRUE);
    res$intermediate$itemVariances[[currentDf]] <-
      diag(stats::var(dat[[currentDf]], na.rm=TRUE));
  }

  ### Make two dataframes out of these vectors
  res$intermediate$dat.itemMeans <- as.data.frame(res$intermediate$itemMeans);
  res$intermediate$dat.itemVariances <- as.data.frame(res$intermediate$itemVariances);

  ### Compute average over parallel tests
  res$intermediate$meanItemMeans <- rowMeans(res$intermediate$dat.itemMeans);
  res$intermediate$meanItemVariances <- rowMeans(res$intermediate$dat.itemVariances);

  ### Sort item means and variance
  res$intermediate$meanItemMeans.sorted <- sort(res$intermediate$meanItemMeans);
  res$intermediate$meanItemVariances.sorted <- sort(res$intermediate$meanItemVariances);

  ### Select items for test a1 and test a2
  res$output$byMeans <- list();
  res$output$byVariances <- list();

  res$output$byMeans$a1 <-
    names(res$intermediate$meanItemMeans.sorted)[is.odd(1:length(res$intermediate$meanItemMeans.sorted))];
  res$output$byMeans$a2 <-
    names(res$intermediate$meanItemMeans.sorted)[is.even(1:length(res$intermediate$meanItemMeans.sorted))];
  res$output$byVariances$a1 <-
    names(res$intermediate$meanItemVariances.sorted)[is.odd(1:length(res$intermediate$meanItemVariances.sorted))];
  res$output$byVariances$a2 <-
    names(res$intermediate$meanItemVariances.sorted)[is.even(1:length(res$intermediate$meanItemVariances.sorted))];

  ### Construct selection vectors
  res$output$byMeans$in_a1 <- names(res$intermediate$meanItemMeans) %in% res$output$byMeans$a1;
  res$output$byMeans$in_a2 <- names(res$intermediate$meanItemVariances) %in% res$output$byMeans$a2;
  res$output$byVariances$in_a1 <- names(res$intermediate$meanItemMeans) %in% res$output$byVariances$a1;
  res$output$byVariances$in_a2 <- names(res$intermediate$meanItemVariances) %in% res$output$byVariances$a2;

  ### Store means for each subscale
  res$output$byMeans$a1.mean <- mean(res$intermediate$meanItemMeans[res$output$byMeans$in_a1]);
  res$output$byMeans$a2.mean <- mean(res$intermediate$meanItemMeans[res$output$byMeans$in_a2]);
  res$output$byVariances$a1.mean <- mean(res$intermediate$meanItemMeans[res$output$byVariances$in_a1]);
  res$output$byVariances$a2.mean <- mean(res$intermediate$meanItemMeans[res$output$byVariances$in_a2]);

  ### Store variances for each subscale
  res$output$byMeans$a1.variance <- sum(res$intermediate$meanItemVariances[res$output$byMeans$in_a1]);
  res$output$byMeans$a2.variance <- sum(res$intermediate$meanItemVariances[res$output$byMeans$in_a2]);
  res$output$byVariances$a1.variance <- sum(res$intermediate$meanItemVariances[res$output$byVariances$in_a1]);
  res$output$byVariances$a2.variance <- sum(res$intermediate$meanItemVariances[res$output$byVariances$in_a2]);

  class(res) <- "parallelSubscales";
  return(res);

}

#' @rdname parallelSubscales
#' @method print parallelSubscales
#' @export
print.parallelSubscales <- function(x, digits=2, ...) {
  cat("--- When splitting by means:\n");
  cat(paste0("Mean subscale a1 = ",round(x$output$byMeans$a1.mean, digits=digits),
             ", variance = ", round(x$output$byMeans$a1.variance, digits=digits),
             " (", paste(x$output$byMeans$a1, collapse=", "), ")\n"));
  cat(paste0("Mean subscale a2 = ", round(x$output$byMeans$a2.mean, digits=digits),
             ", variance = ", round(x$output$byMeans$a2.variance, digits=digits),
             " (", paste(x$output$byMeans$a2, collapse=", "), ")\n"));
  cat("\n");
  cat("--- When splitting by variances:\n");
  cat(paste0("Mean subscale a1 = ", round(x$output$byVariances$a1.mean, digits=digits),
             ", variance = ", round(x$output$byVariances$a1.variance, digits=digits),
             " (", paste(x$output$byVariances$a1, collapse=", "), ")\n"));
  cat(paste0("Mean subscale a2 = ", round(x$output$byVariances$a2.mean, digits=digits),
             ", variance = ", round(x$output$byVariances$a2.variance, digits=digits),
             " (", paste(x$output$byVariances$a2, collapse=", "), ")\n"));
  return(invisible(x));
}
