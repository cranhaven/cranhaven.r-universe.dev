###########################################################
###########################################################
###
### Function to generate an object with several useful
### statistics and a plot to assess how the elements
### (usually items) in a scale relate to each other.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### Scale Diagnosis


#' scaleDiagnosis
#'
#' scaleDiagnosis provides a number of diagnostics for a scale (an aggregative
#' measure consisting of several items).
#'
#'
#' Function to generate an object with several useful statistics and a plot to
#' assess how the elements (usually items) in a scale relate to each other,
#' such as Cronbach's Alpha, omega, the Greatest Lower Bound, a factor
#' analysis, and a correlation matrix.
#'
#' @param data A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items is NULL.
#' @param items If not NULL, this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param plotSize Size of the final plot in millimeters.
#' @param sizeMultiplier Allows more flexible control over the size of the plot
#' elements
#' @param axisLabels Passed to ggpairs function to set axisLabels.
#' @param scaleReliability.ci TRUE or FALSE: whether to compute confidence
#' intervals for Cronbach's Alpha and Omega (uses bootstrapping function in
#' MBESS, takes a while).
#' @param conf.level Confidence of confidence intervals for reliability
#' estimates (if requested with scaleReliability.ci).
#' @param normalHist Whether to use the default ggpairs histogram on the
#' diagonal of the scattermatrix, or whether to use the [normalHist()] version.
#' @param poly Whether to also request the estimates based on the polychoric
#' correlation matrix when calling [scaleStructure()].
#' @param scaleName Optionally, a name for the scale to print as heading for
#' the results.
#' @param x The object to print.
#' @param digits The number of digits to pass to the `print` method for the
#' descriptives dataframe.
#' @param headingLevel The level of the heading (number of hash characters to insert
#' before the heading, to be rendered as headings of that level in Markdown).
#' @param quiet Whether to be chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the code in the partial (`TRUE`) or hide it (`FALSE`).
#' @param partialFile The file with the Rmd partial (if you want to overwrite the default).
#' @param ...  Additional arguments for `scaleDiagnosis()` are passed on to
#' [scatterMatrix()], and additional arguments for the `print` method are passed
#' to the default `print` method.
#' @return
#'
#' An object with the input and several output variables. Most notably:
#' \item{scaleReliability}{The results of scaleReliability.} \item{pca}{A
#' Principal Components Analysis} \item{fa}{A Factor Analysis}
#' \item{describe}{Decriptive statistics about the items}
#' \item{scatterMatrix}{A scattermatrix with histograms on the diagonal and
#' correlation coefficients in the upper right half.}
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar
#' @rdname scaleDiagnosis
#' @examples
#'
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#' ### This will prompt the user to select an SPSS file
#' scaleDiagnosis();
#'
#' ### Generate a datafile to use
#' exampleData <- data.frame(item1=rnorm(100));
#' exampleData$item2 <- exampleData$item1+rnorm(100);
#' exampleData$item3 <- exampleData$item1+rnorm(100);
#' exampleData$item4 <- exampleData$item2+rnorm(100);
#' exampleData$item5 <- exampleData$item2+rnorm(100);
#'
#' ### Use a selection of two variables
#' scaleDiagnosis(data=exampleData, items=c('item2', 'item4'));
#'
#' ### Use all items
#' scaleDiagnosis(data=exampleData);
#' }
#'
#' @export scaleDiagnosis
scaleDiagnosis <- function(data=NULL, items=NULL, plotSize=180, sizeMultiplier = 1,
                           axisLabels = "none", scaleReliability.ci=FALSE,
                           conf.level=.95, normalHist=TRUE, poly=TRUE,
                           digits=3,
                           headingLevel=3,
                           scaleName=NULL, ...) {

  dat <- data;

  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(dat)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid SPSS file selected in ",
                                       "the dialog I then showed to allow selection of a dataset."),
                   use.value.labels=FALSE);
  }
  else {
    if (!is.data.frame(dat)) {
      stop("Argument 'dataframe' must be a dataframe or NULL! Class of ",
           "provided argument: ", class(dat));
    }
  }

  if (is.null(items)) {
    items <- names(dat);
  }

  ### Create object to store results
  res <- list(scaleName = scaleName,
              headingLevel = headingLevel,
              items = items,
              plotSize = plotSize,
              sizeMultiplier = sizeMultiplier);

  ### Extract dataframe and select only complete cases
  res$dat <- dat[stats::complete.cases(dat[, items]), items];
  res$n <- nrow(res$dat);

  ### Convert all variables to numeric vectors, if they weren't already
  res$dat <- data.frame(lapply(res$dat, 'as.numeric'));

  ### Basic univariate descriptives
  ### ------------
  ### 2019-05-31: used to be 'describe', I guess from psych.
  ### ------------
  res$describe <-
    do.call(rbind,
            lapply(res$dat,
                   function(x) {
                     return(as.data.frame(descr(convertToNumeric(x))));
                   }));

  ### --- 2020-09-25
  ### For some reason these are character columns?
  res$describe <-
    massConvertToNumeric(res$describe);

  ### Bivariate correlations
  res$cor <- stats::cor(res$dat, use="complete.obs");


  suppressMessages(suppressWarnings(
    if (!requireNamespace("GGally")) {
      message("For the scatter matrix, you need the {GGally} package.\n\n",
              "You can install it with the following command:\n\n",
              "install.packages('GGally');");

      res$scatterMatrix <- NULL;

    } else {

      res$scatterMatrix <- scatterMatrix(res$dat, plotSize=180, sizeMultiplier = 1,
                                         axisLabels = "none", normalHist=normalHist,
                                         progress=FALSE, ...);

    }
  ))

  ### Exploratory factor analysis
  #pa.out <- factor.pa(r = bfi, nfactors = 5, residuals = FALSE,
  #                    + rotate = "varimax", n.obs = NA, scores = FALSE, SMC = TRUE,
  #                    + missing = FALSE, impute = "median", min.err = 0.001, digits = 2,
  #                    + max.iter = 100, symmetric = TRUE, warnings = TRUE, fm = "pa")

  ### Extract eigen values
  res$eigen <- eigen(res$cor);
  ### Determine how many factors have eigenvalues
  ### over 1 - note that we're not doing a real
  ### exploratory factor analysis, we're just interested
  ### in whether this scale works out (it's not
  ### unidimensional if more than one factor has an
  ### eigenvalue a lot over 1)
  res$factors <- sum(res$eigen$values > 1);


  if (requireNamespace("psych", quietly = TRUE)) {
    ### If there are more than two items, do a principal
    ### component analysis and a factor analysis
    if (ncol(res$cor) > 2) {
      ### Principal components analysis
      res$pca <- psych::principal(r = res$cor, n.obs = res$n, rotate="oblimin",
                                  nfactors=res$factors);
      ### Exploratory factor analysis
      res$fa <- psych::fa(r = res$cor, n.obs = res$n, rotate="oblimin",
                          fm="ml", nfactors=res$factors);
    }
  }

  ### Internal consistency measures
  res$scaleReliability <- scaleStructure(data=res$dat, items=items,
                                         ci=scaleReliability.ci,
                                         conf.level=conf.level,
                                         poly = poly,
                                         headingLevel=headingLevel+2);

  ### Return results
  class(res) <- c('scaleDiagnosis');
  return(res);
}

#' @method print scaleDiagnosis
#' @rdname scaleDiagnosis
#' @export
print.scaleDiagnosis <- function(x, digits=x$digits, ...) {
  if (!is.null(x$scaleName)) {
    cat("Scale diagnosis for ", x$scaleName);
  }
  print(x$scaleReliability, ...);
  cat(paste0("\n\nEigen values: ", paste(round(x$eigen$values, 3), collapse=", ")));
  if (!is.null(x$pca) && !is.null(x$fa)) {
    cat("\n\nFactor analysis (reproducing only shared variance):\n\n");
    print(x$fa$loadings, ...);
    cat("\n\nComponent analysis (reproducing full covariance matrix):\n\n");
    print(x$pca$loadings, ...);
  }
  cat("\n\n");
  print(x$describe, digits=digits, ...);
  if (!is.null(x$scatterMatrix)) {
    print(x$scatterMatrix$output$scatterMatrix, ...);
  }
  invisible();
}

#' @rdname scaleDiagnosis
#' @export
scaleDiagnosis_partial <- function(x,
                                   headingLevel = x$input$headingLevel,
                                   quiet=TRUE,
                                   echoPartial = FALSE,
                                   partialFile = NULL,
                                   ...) {

  ### Get filename
  if ((!is.null(partialFile)) && file.exists(partialFile)) {
    rmdPartialFilename <-
      partialFile;
  } else {
    rmdPartialFilename <-
      system.file("partials", "_scaleDiagnosis_partial.Rmd", package="ufs");
  }

  if (!file.exists(rmdPartialFilename)) {
    stop(
      "The file with the RMarkdown partial specified to ",
      "`ufs::scaleStructure_partial()` was not found (this file ",
      "was '", rmdPartialFilename, "')!"
    );
  }

  rmdpartials::partial(rmdPartialFilename);

}



#' @rdname scaleDiagnosis
#' @method knit_print scaleDiagnosis
#' @importFrom knitr knit_print
#' @export
knit_print.scaleDiagnosis <- function(x,
                                      headingLevel = x$headingLevel,
                                      quiet=TRUE,
                                      echoPartial = FALSE,
                                      partialFile = NULL,
                                      ...) {

  scaleDiagnosis_partial(x = x,
                         headingLevel = headingLevel,
                         quiet = quiet,
                         echoPartial = echoPartial,
                         partialFile = partialFile,
                         ...);

}


# partial.scaleDiagnosis <- function(x,
#                                    headingLevel = 3,
#                                    quiet=TRUE,
#                                    echoPartial = FALSE,
#                                    partialFile = NULL) {
#
#   digits <- 3;
#
#   ### Get filename
#   if (!is.null(partialFile) && file.exists(partialFile)) {
#     rmdPartialFilename <-
#       partialFile;
#   } else {
#     rmdPartialFilename <-
#       system.file("partials", "_scaleDiagnosis.rmd", package="ufs");
#   }
#
#   ### Read file with partial
#   rmdPartial <-
#     readLines(con=con<-file(rmdPartialFilename,
#                             encoding="UTF-8"));
#   close(con);
#
#   ### Paste into one character vector
#   rmdPartial <-
#     paste0(rmdPartial, collapse = "\n");
#
#   ### Specify indent to be used in other partial functions, if needed
#   indent <- repStr("#", headingLevel);
#
#   res <- knitr::knit(text = rmdPartial,
#                      encoding="UTF-8",
#                      tangle=knitr::opts_knit$get("tangle"),
#                      quiet=quiet,
#                      envir=environment());
#
#   knitr::asis_output(paste(c("", res),
#                            collapse = "\n"));
#
# }
