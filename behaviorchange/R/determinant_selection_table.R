#' @rdname potential_for_change
#'
#' @param determinantLabels,targetLabel Optionally, labels to use for the
#' (sub-)determinants and the target. The `determinantLabels` must have the
#' same order as the `determinants` vector.
#' @param sortBy The column to sort the results by; if not `NULL`, a number
#' from 1-6 that corresponds to the six columns of the Determinant Selection
#' Table.
#' @param sortByAbs Whether to sort by raw values (`FALSE`) or their
#' absolute value (`TRUE`).
#' @param decreasing Whether to sort in decreasing (`TRUE`) or increasing
#' (`FALSE`) order.
#' @param digits The number of digits to round to.
#' @param potentialScale The scale with minimum and maximum possible values
#' for the Potential for Change index. If `NULL`, the minimum is set to `0`
#' and the maximum is set to the highest observed value.
#' @param headingLevel The level of the heading if using an RMarkdown partial.
#' @param output Whether to only output to the viewer (if possible;
#' `output='viewer'`), or only to the console (`output='console'`), or to
#' both (`output=c('viewer', 'console')`). Note that displaying in the viewer
#' requires the `htmltools` package.
#' @param headingLevel The number of hashes to print in front of the headings
#' when printing while knitting
#' @param forceKnitrOutput Force knitr output.
#' @param quiet Passed on to [knitr::knit()] whether it should e
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available.
#' @param ... Any additional arguments are passed to the default print method
#' by the print method, and to [rmdpartials::partial()] when knitting a
#' RMarkdown partial.
#'
#' @export
determinant_selection_table <-  function(data,
                                         determinants,
                                         target,
                                         determinantLabels = NULL,
                                         targetLabel = NULL,
                                         sortBy = NULL,
                                         sortByAbs = TRUE,
                                         decreasing = TRUE,
                                         digits = 3,
                                         increasesAreImprovements = TRUE,
                                         minimum = base::min,
                                         maximum = base::max,
                                         center = base::mean,
                                         weight = stats::cor,
                                         type = NULL,
                                         minimumArgs = list(na.rm=TRUE),
                                         maximumArgs = list(na.rm=TRUE),
                                         centerArgs = list(na.rm=TRUE),
                                         weightArgs = list(use="complete.obs"),
                                         potentialScale = NULL,
                                         headingLevel = 3,
                                         output = behaviorchange::opts$get('tableOutput')) {

  if (missing(data) || missing(determinants) || missing(target)) {
    stop("You must provide `data` and specify which columns with ",
         "`determinants` and the `target` to use!");
  }

  ### Start by calling the potential for change function, which will catch
  ### wrong arguments and throw errors etc.
  P_delta <-
    potential_for_change_index(
      data = data,
      determinants = determinants,
      target = target,
      increasesAreImprovements = increasesAreImprovements,
      sampleLevel = TRUE,
      minimum = minimum,
      maximum = maximum,
      center = center,
      weight = weight,
      minimumArgs = minimumArgs,
      maximumArgs = maximumArgs,
      centerArgs = centerArgs,
      weightArgs = weightArgs
    );

  if (is.null(determinantLabels)) {
    determinantLabels <- determinants;
  }

  if (is.null(targetLabel)) {
    targetLabel <- target;
  }

  ### Get lower and upper bounds, centers, and weights

  nCols <- length(determinants);

  ### If single number or functions are provided, replicate them to have
  ### the same length as the number of columns for slightly more ease
  ### later on. Note that we wrap them in lists first to enable us to use
  ### `rep` on functions, should those be passed.

  if (length(increasesAreImprovements) == 1) {
    increasesAreImprovements <- rep(increasesAreImprovements, nCols);
  } else if (length(increasesAreImprovements) != nCols) {
    stop("As `increasesAreImprovements`, pass either `TRUE` or `FALSE`, ",
         "or a logical vector with the same length as `x` has columns.");
  }
  if (length(minimum) == 1) {
    minimum <- rep(list(minimum), nCols);
  } else if (length(minimum) != nCols) {
    stop("As `minimum`, pass either a function or a number (or for ",
         "advanced users, a vector of numbers or a list of functions).");
  }
  if (length(maximum) == 1) {
    maximum <- rep(list(maximum), nCols);
  } else if (length(maximum) != nCols) {
    stop("As `maximum`, pass either a function or a number (or for ",
         "advanced users, a vector of numbers or a list of functions).");
  }
  if (length(center) == 1) {
    center <- rep(list(center), nCols);
  } else if (length(center) != nCols) {
    stop("As `center`, pass either a function or a number (or for ",
         "advanced users, a vector of numbers or a list of functions).");
  }

  ### Loop through data frame
  res <-
    lapply(
      1:nCols,
      function(i) {
        if (is.function(minimum[[i]])) {
          lowerBound <-
            do.call(
              minimum[[i]],
              args = c(list(data[, determinants[i]]),
                       minimumArgs)
            );
        } else {
          lowerBound <- minimum[[i]];
        }
        if (is.function(maximum[[i]])) {
          upperBound <-
            do.call(
              maximum[[i]],
              args = c(list(data[, determinants[i]]),
                       maximumArgs)
            );
        } else {
          upperBound <- maximum[[i]];
        }
        statusQuo <-
          do.call(
            center[[i]],
            args = c(list(data[, determinants[i]]),
                     centerArgs)
          );
        currentWeight <-
          do.call(
            weight,
            c(list(data[, determinants[i]],
                   data[, target]),
              weightArgs)
          );
        return(
          data.frame(
            lowerBound = lowerBound,
            upperBound = upperBound,
            statusQuo = statusQuo,
            weight = currentWeight
          )
        );
      }
    );

  res <-
    do.call(
      rbind,
      res
    );

  res$determinant <- determinantLabels;

  res$P_delta <- P_delta;

  res <-
    res[,
        c("determinant",
          "lowerBound",
          "statusQuo",
          "upperBound",
          "weight",
          "P_delta")
    ];
  names(res) <-
    c("determinant",
      "lower",
      "current",
      "upper",
      "weight",
      "p_delta");

  if (!is.null(sortBy)) {
    if (sortByAbs) {
      res <-
        res[
          order(
            abs(res[, sortBy]),
            decreasing = decreasing
          ),
        ];
    } else {
      res <-
        res[
          order(
            res[, sortBy],
            decreasing = decreasing
          ),
        ];
    }
  }

  if (is.null(potentialScale)) {
    potentialScale <-
      c(0,
        max(abs(res[["p_delta"]]), na.rm=TRUE));
  }

  if ((!is.numeric(potentialScale) || (length(potentialScale) != 2))) {
    stop("When providing `potentialScale`, it must have two values!");
  }

  attr(res, "potentialScale") <- potentialScale;
  attr(res, "digits") <- digits;
  attr(res, "output") <- output;

  class(res) <- c("determinantSelectionTable",
                  class(res));

  return(res);

}

###-----------------------------------------------------------------------------

#' @rdname potential_for_change
#' @export
determinantSelectionTable_partial <- function(x,
                                              digits = attr(x, 'digits'),
                                              headingLevel = attr(x, 'headingLevel'),
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
      system.file("partials", "_determinantSelectionTable_partial.Rmd",
                  package="behaviorchange");
  }

  rmdpartials::partial(rmdPartialFilename);

}

###-----------------------------------------------------------------------------

#' @rdname potential_for_change
#' @method knit_print determinantSelectionTable
#' @importFrom knitr knit_print
#' @export
knit_print.determinantSelectionTable <- function(x,
                                                 digits = attr(x, 'digits'),
                                                 headingLevel = attr(x, 'headingLevel'),
                                                 echoPartial = FALSE,
                                                 partialFile = NULL,
                                                 quiet=TRUE,
                                                 ...) {
  determinantSelectionTable_partial(x = x,
                                    headingLevel = headingLevel,
                                    quiet = quiet,
                                    echoPartial = echoPartial,
                                    partialFile = partialFile,
                                    ...);
}

###-----------------------------------------------------------------------------

### (not exported)

prettyDeterminantSelectionTable <- function(x,
                                            digits = attr(x, 'digits'),
                                            potentialScale = attr(x, 'potentialScale')) {

  prettyRes <- x;

  names(prettyRes) <-
    c("(Sub-)determinant",
      "Lower bound",
      "Current",
      "Upper bound",
      "Weight",
      "Potential for Change Index");

  prettyRes[, 2:6] <- round(prettyRes[, 2:6], digits);
  prettyRes[, 1:5] <-
    lapply(
      prettyRes[, 1:5],
      kableExtra::cell_spec,
      extra_css = "padding: 5px; "
    );
  prettyRes[["Potential for Change Index"]] <-
    kableExtra::cell_spec(
      prettyRes[["Potential for Change Index"]],
      bold = TRUE,
      color = kableExtra::spec_color(
        abs(prettyRes[["Potential for Change Index"]]),
        end = 0.9,
        direction = -1,
        scale_from = potentialScale
      ),
      font_size = kableExtra::spec_font_size(
        abs(prettyRes[["Potential for Change Index"]]),
        scale_from = potentialScale,
      ),
      extra_css = "padding: 5px; "
    );
  prettyResTable <-
    kableExtra::kable_styling(
      knitr::kable(
        prettyRes,
        escape=FALSE,
        format="html",
        row.names = FALSE
      ),
      html_font = "arial"
    );

  return(prettyResTable);
}

###-----------------------------------------------------------------------------

#' @rdname potential_for_change
#' @export
print.determinantSelectionTable <- function(x,
                                            digits = attr(x, 'digits'),
                                            headingLevel = attr(x, 'headingLevel'),
                                            output = attr(x, 'output'),
                                            forceKnitrOutput = FALSE,
                                            ...) {

  if (requireNamespace("kableExtra", quietly = TRUE)) {
    prettyResTable <- prettyDeterminantSelectionTable(x,
                                                      digits=digits);
  }

  ### Set viewer based on whether we're in rstudio and interactive
  if (interactive() && ("viewer" %in% output)) {
    ### Set viewer depending on whether we're in RStudio
    if ((requireNamespace("rstudioapi", quietly = TRUE)) &&
        (rstudioapi::isAvailable())) {
      viewer <- rstudioapi::viewer;
    } else {
      viewer <- getOption("viewer",
                          utils::browseURL);
    }
    outputToViewer <- TRUE;
  } else {
    outputToViewer <- FALSE;
  }

  if (outputToViewer && requireNamespace('htmltools', quietly = TRUE)) {
    htmltools::html_print(htmltools::HTML(prettyResTable),
                          viewer=viewer);
  }

  if ('console' %in% output) {
    printableRes <- x;
    class(printableRes) <- "data.frame";
    print(printableRes);
  }

  return(invisible(x));

}
