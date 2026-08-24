#' A report to help diagnosing careless responders
#'
#' This function wraps functions from the `careless` package
#' to help inspect and diagnose careless participants. It is
#' optimized for using in R Markdown files.
#'
#' @param data The dataframe.
#' @param items The items to look at.
#' @param nFlags How many indicators need to be flagged for
#' a participant to be considered suspect.
#' @param flagUnivar How extreme a score has to be for it
#' to be flagged as suspicous univariately.
#' @param flagMultivar This has not been implemented yet.
#' @param irvSplit Whether to split for the IRV, and if so,
#' in how many parts.
#' @param datasetName The name of the dataset to display (to override,
#' if desired).
#' @param headingLevel The level of the heading in Markdown (the
#' number of `#`s to include before the heading).
#' @param headingSuffix The suffix to include; by default, set
#' such that the individual participants IRP plots are placed
#' in separate tabs.
#' @param digits The number of digits to round to.
#' @param missingSymbol How to represent missing values.
#' @param responseTime If not `NULL`, the name of a column
#' containing the participants' response times.
#'
#' @return NULL, invisibly; and prints the report.
#' @export
#'
#' @examples ### Get the BFI data taken from the `psych` package
#' dat <- ufs::bfi;
#'
#' ### Get the variable names for the regular items
#' bfiVars <-
#'   setdiff(names(dat),
#'           c("gender", "education", "age"));
#'
#' ### Inspect suspect participants, very conservatively to
#' ### limit the output (these are 2800 participants).
#' carelessReport(data = dat,
#'                items = bfiVars,
#'                nFlags = 5);
carelessReport <- function(data,
                           items = names(data),
                           nFlags = 1,
                           flagUnivar = .99,
                           flagMultivar = .95,
                           irvSplit = 4,
                           headingLevel = 3,
                           datasetName = NULL,
                           responseTime = NULL,
                           headingSuffix = " {.tabset}",
                           digits = 2,
                           missingSymbol = "Missing") {

  if (!requireNamespace("careless", quietly = TRUE)) {

    message("\nPackage \"careless\" needed for this function to work. ",
            "You can install it using:\n\n",
            "  install.packages('careless');\n");

    return(invisible(FALSE));

  } else {

    ### Get original dataset name
    if (is.null(datasetName)) {
      datasetName <-
        deparse(substitute(data));
    }

    carelessObject <- carelessObject(data = data,
                                     items = items,
                                     flagUnivar = flagUnivar,
                                     flagMultivar = flagMultivar,
                                     irvSplit = irvSplit,
                                     responseTime = responseTime);

    suspectParticipants <- suspectParticipants(carelessObject,
                                               digits = digits,
                                               nFlags = nFlags,
                                               missingSymbol = missingSymbol);

    cat0("\n\n", repStr("#", headingLevel),
         " Careless Responding Report",
         headingSuffix, "\n\n");

    ### Select variables to show in table
    varsForTable <- grep("_chr", names(suspectParticipants));
    reportTableColNames <- names(suspectParticipants)[varsForTable];

    ### Produce column names
    carelessDict <- opts$get("carelessDict");
    for (processDict in 1:length(carelessDict)) {
      reportTableColNames <-
        gsub(carelessDict[[processDict]][1],
             carelessDict[[processDict]][2],
             reportTableColNames);
    }

    originalKnitrKableNA <- getOption("knitr.kable.NA");
    on.exit(options(knitr.kable.NA = originalKnitrKableNA));
    options(knitr.kable.NA = "");

    print(
      knitr::kable(
        suspectParticipants[, varsForTable],
        col.names = reportTableColNames
      )
    );

    cat0("\n\nFor the Individual Response Pattern plot for each ",
         "participant, see the following headings, which may ",
         "appear as tabs above this section.\n\n");

    for (currentRow in row.names(suspectParticipants)) {
      cat("\n\n");
      cat0("\n\n", repStr("#", headingLevel+1),
           " IRP-plot for participant ",
           currentRow, "\n\n");
      cat("\n\n");
      print(irpplot(data = data,
                    row = currentRow,
                    columns = items,
                    dataName = datasetName));
      cat("\n\n");
    }

    return(invisible(NULL));
  }

}
