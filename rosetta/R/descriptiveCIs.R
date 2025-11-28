#' Descriptives with confidence intervals
#'
#' @param data The data frame holding the data, or a vector.
#' @param items If supplying a data frame as `data`, the names of the
#' columns to process.
#' @param itemLabels Optionally, labels to use for the items (optionally, named,
#' with the names corresponding to the `items`; otherwise, the order of the
#' labels has to match the order of the items)
#' @param conf.level The confidence level of the confidence intervals.
#' @param digits The number of digits to print in the result.
#' @param x The object to print (i.e. the object returned by `descriptiveCIs`).
#' @param digits The number of digits to round the output to.
#' @param forceKnitrOutput Whether to force `knitr` output even when
#' not knitting.
#' @param ... Any additional arguments are passed on to [knitr::kable()] or to
#' [base::print()].
#'
#' @return A data frame with class `rosettaDescriptiveCIs` prepended to allow
#' printing neatly while knitting to Markdown.
#' @rdname rosettaDescriptiveCIs
#' @export
#'
#' @examples descriptiveCIs(mtcars);
descriptiveCIs <- function(data,
                           items = NULL,
                           itemLabels = NULL,
                           conf.level = .95,
                           digits = 2) {

  if (!is.data.frame(data)) {
    varName <- deparse(substitute(data));
    data <- data.frame(x = data);
    names(data) <- varName;
  }

  if (is.null(items)) {
    items <- names(data);
  }

  if (is.null(itemLabels)) {
    itemLabels <- items;
  } else {
    if (length(itemLabels) != length(items)) {
      stop("The vector with item labels (", vecTxtQ(itemLabels),
           ") does not have the same length ",
           "as the vector with items(", vecTxtQ(items),
           ")!");
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

  if (!all(items %in% names(data))) {
    stop("Not all `items` you specified exist as columns in ",
         "the data frame you supplied as `data`!");
  }

  data <- data[, items, drop=FALSE];

  data <- ufs::massConvertToNumeric(data);

  res <-
    do.call(
      rbind,
      lapply(
        items,
        function(item) {
          meanCI <- ufs::meanConfInt(data[, item])$output$ci;
          sdCI <- confIntSD(data[, item]);
          return(
            data.frame(
              mean.ci.lo = meanCI[1],
              mean.point = mean(data[, item], na.rm=TRUE),
              mean.ci.hi = meanCI[2],
              sd.ci.lo = sdCI[1],
              sd.point = stats::sd(data[, item], na.rm=TRUE),
              sd.ci.hi = sdCI[2]
            )
          );
        }
      )
    );
  row.names(res) <- itemLabels;

  attr(res, "digits") <-
    digits;
  attr(res, "conf.level") <-
    conf.level;

  class(res) <-
    c("rosettaDescriptiveCIs",
      class(res));

  return(res);

}

#' @export
#' @rdname rosettaDescriptiveCIs
print.rosettaDescriptiveCIs <- function(x,
                                        digits = attr(x, "digits"),
                                        forceKnitrOutput = FALSE,
                                        ...) {

  conf.level <- attr(x, "conf.level");

  if (isTRUE(getOption('knitr.in.progress')) || forceKnitrOutput) {

    ufs::kblXtra(
      x,
      digits = digits,
      col.names =
        c(paste0("Mean, ", round(100*conf.level, 2), "% CI lower bound:"),
          "Mean, point<br />estimate:",
          paste0("Mean, ", round(100*conf.level, 2), "% CI upper bound:"),
          paste0("SD, ", round(100*conf.level, 2), "% CI lower bound:"),
          "SD, point<br />estimate:",
          paste0("SD, ", round(100*conf.level, 2), "% CI upper bound:")),
      ...
    );

  } else {
    class(x) <- "data.frame";
    print(round(x, digits),...);
    cat(paste0("\nNote: these are ", round(100*conf.level, 2), "% confidence intervals."));
  }
  return(invisible(x));
}

