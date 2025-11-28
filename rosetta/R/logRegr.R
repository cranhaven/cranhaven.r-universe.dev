#' Userfriendly wrapper to do logistic regression in R
#'
#' This function is meant as a userfriendly wrapper to approximate the way
#' logistic regression is done in SPSS.
#'
#' @param formula The formula, specified in the same way as for
#' [stats::glm()] (which is used for the actual analysis).
#' @param data Optionally, a dataset containing the variables in the formula
#' (if not specified, the variables must exist in the environment specified in
#' \code{env}.
#' @param predictGroupValue,comparisonGroupValue Can optionally be used
#' to set the value to predict and the value to compare with.
#' @param conf.level The confidence level for the confidence intervals.
#' @param digits The number of digits used when printing the results.
#' @param pvalueDigits The number of digits used when printing the p-values.
#' @param crossTabs Whether to show cross tabulations of the correct
#' predictions for the null model and the tested model, as well as the
#' percentage of correct predictions.
#' @param oddsRatios Whether to also present the regression coefficients
#' as odds ratios (i.e. simply after a call to [base::exp()]).
#' @param plot Whether to display the plot.
#' @param collinearity Whether to show collinearity diagnostics.
#' @param env If no dataframe is specified in \code{data}, use this argument to
#' specify the environment holding the variables in the formula.
#' @param predictionColor,dataColor,observedMeansColor The color of,
#' respectively, the line and confidence interval showing the prediction; the
#' points representing the observed data points; and the means based on the
#' observed data.
#' @param predictionAlpha,dataAlpha,observedMeansAlpha The alpha of,
#' respectively, the confidence interval of the prediction; the points
#' representing the observed data points; and the means based on the observed
#' data (set to 0 to hide an element).
#' @param predictionSize,dataSize,observedMeansSize The size of, respectively,
#' the line of the prediction; the points representing the observed data
#' points; and the means based on the observed data (set to 0 to hide an
#' element).
#' @param binObservedMeans Whether to bin the observed means; either FALSE or a
#' single numeric value specifying the number of bins.
#' @param observedMeansWidth The width of the lines of the observed means. If
#' not specified (i.e. \code{NULL}), this is computed automatically and set to
#' the length of the shortest interval between two successive points in the
#' predictor data series (found using [ufs::findShortestInterval()].
#' @param theme The theme used to display the plot.
#' @param headingLevel The number of hashes to print in front of the headings
#' @param x The object to print (i.e. as produced by `rosetta::logRegr`).
#' @param forceKnitrOutput Force knitr output.
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available.
#' @param \dots Any additional arguments are passed to the default print method
#' by the print method, and to [rmdpartials::partial()] when knitting an
#' RMarkdown partial.
#'
#' @return Mainly, this function prints its results, but it also returns them
#' in an object containing three lists: \item{input}{The arguments specified
#' when calling the function} \item{intermediate}{Intermediat objects and
#' values} \item{output}{The results, such as the plot, the cross tables, and
#' the coefficients.}
#'
#' @author Ron Pat-El & Gjalt-Jorn Peters (both while at the Open University of
#' the Netherlands)
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{regr}} and \code{\link{fanova}} for similar functions
#' for linear regression and analysis of variance and [stats::glm()] for the
#' regular interface for logistic regression.
#' @keywords htest models regression nonlinear hplot
#' @rdname rosettaLogRegr
#' @examples
#'
#' ### Simplest way to call logRegr
#' rosetta::logRegr(data=mtcars, formula = vs ~ mpg);
#'
#' ### Also ordering a plot
#' rosetta::logRegr(
#'   data=mtcars,
#'   formula = vs ~ mpg,
#'   plot=TRUE
#' );
#'
#' ### Only use five bins
#' rosetta::logRegr(
#'   data=mtcars,
#'   formula = vs ~ mpg,
#'   plot=TRUE,
#'   binObservedMeans=5
#' );
#'
#' \dontrun{
#' ### Mimic output that would be obtained
#' ### when calling from an R Markdown file
#' rosetta::rosettaLogRegr_partial(
#'   rosetta::logRegr(
#'     data=mtcars,
#'     formula = vs ~ mpg,
#'     plot=TRUE
#'   )
#' );
#' }
#'
#' @export logRegr
logRegr <- function(formula, data=NULL, conf.level=.95, digits=2,
                    predictGroupValue= NULL,
                    comparisonGroupValue=NULL,
                    pvalueDigits = 3,
                    crossTabs = TRUE,
                    oddsRatios = TRUE,
                    plot=FALSE,
                    collinearity = FALSE,
                    env=parent.frame(),
                    predictionColor = rosetta::opts$get('viridis3')[3],
                    predictionAlpha = .5,
                    predictionSize = 2,
                    dataColor = rosetta::opts$get('viridis3')[1],
                    dataAlpha = .33,
                    dataSize=2,
                    observedMeansColor = rosetta::opts$get('viridis3')[2],
                    binObservedMeans = 7,
                    observedMeansSize = 2,
                    observedMeansWidth = NULL,
                    observedMeansAlpha = .5,
                    theme=ggplot2::theme_bw(),
                    headingLevel = 3) {

  ### Generate object to store input, intermediate outcomes, and results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  ### Extract variables from formula
  res$intermediate$variableNames <- all.vars(formula);

  ### Convert formula to a character string
  res$intermediate$formula.as.character <-
    paste0(as.character(formula)[c(2, 1, 3)], collapse=" ");

  dat <- data;

  if (is.null(dat)) {
    ### Extract variables from formula
    res$intermediate$variables <-
      as.character(as.list(attr(stats::terms(formula), 'variables'))[-1]);

    ### Store variablesnames only for naming in raw dataframe
    res$intermediate$variables_namesOnly <- unlist(
      lapply(strsplit(res$intermediate$variables, "\\$"), utils::tail, 1));

    ### Store variables in a dataframe
    res$intermediate$dat.raw <- list();
    for (varToGet in 1:length(res$intermediate$variables)) {
      res$intermediate$dat.raw[[res$intermediate$variables_namesOnly[varToGet]]] <-
        eval(parse(text=res$intermediate$variables[varToGet]), envir=env);
    }
    ### Convert the list to a dataframe
    res$intermediate$dat.raw <- data.frame(res$intermediate$dat.raw);

    ### Convert variable names to bare variable names
    for (currentVariableIndex in 1:length(res$intermediate$variables)) {
      res$intermediate$formula.as.character <-
        gsub(res$intermediate$variables[currentVariableIndex],
             res$intermediate$variables_namesOnly[currentVariableIndex],
             res$intermediate$formula.as.character, fixed=TRUE);
    }

  } else {
    ### Store variables in a dataframe
    res$intermediate$dat.raw <- dat[, res$intermediate$variableNames];
    res$intermediate$variables_namesOnly <- res$intermediate$variableNames;
  }

  res$intermediate$formula <- formula(res$intermediate$formula.as.character,
                                      env = environment());

  if (is.null(predictGroupValue)) {
    highestValue <-
      sort(names(table(res$intermediate$dat.raw[, 1])), decreasing=TRUE)[1];
  } else {
    highestValue <- predictGroupValue;
  }
  if (is.null(comparisonGroupValue)) {
    lowestValue <-
      sort(names(table(res$intermediate$dat.raw[, 1])), decreasing=TRUE)[2];
  } else {
    lowestValue <- comparisonGroupValue;
  }

  res$intermediate$levelNames <-
    levelNames <- c(lowestValue, highestValue);

  res$intermediate$dat.raw[, 1] <-
    ifelse(
      res$intermediate$dat.raw[, 1] == lowestValue,
      0,
      ifelse(
        res$intermediate$dat.raw[, 1] == highestValue,
        1,
        NA
      )
    );

  res$intermediate$dat.raw <-
    res$intermediate$dat.raw[
      stats::complete.cases(
        res$intermediate$dat.raw
      ),
    ];

  ### Run and store lm objects
  suppressMessages(
    res$intermediate$glm <-
      stats::glm(formula=res$intermediate$formula,
                 data=res$intermediate$dat.raw,
                 family=stats::binomial(link='logit'))
  );

  ### Test difference from intercept-only model
  ###   (written by Ron Pat-El)
  res$intermediate$modelChi <-
    res$intermediate$glm$null.deviance -
    res$intermediate$glm$deviance;
  res$intermediate$chiDf <- res$intermediate$glm$df.null -
    res$intermediate$glm$df.residual;
  res$intermediate$deltaChisq <-
    1 - stats::pchisq(res$intermediate$modelChi, res$intermediate$chiDf);

  ### Calculate Cox & Snell R-squared and NagelKerke R-squared
  ###   (also written by Ron Pat-El)
  res$intermediate$CoxSnellRsq <-
    1 - exp((res$intermediate$glm$deviance -
             res$intermediate$glm$null.deviance) /
            length(res$intermediate$glm$fitted.values));
  res$intermediate$NagelkerkeRsq <-
    res$intermediate$CoxSnellRsq /
    (1 - exp(-(res$intermediate$glm$null.deviance /
               length(res$intermediate$glm$fitted.values))));

  ### Run confint on lm object
  suppressMessages(
    res$intermediate$confint <-
      stats::confint(res$intermediate$glm, level=conf.level)
  );

  ### Run lm.influence on lm object
  res$intermediate$influence <-
    stats::influence(res$intermediate$glm);

  ### Get variance inflation factors and compute tolerances
  if (collinearity && (length(res$intermediate$variables) > 2)) {
    res$intermediate$vif <- car::vif(res$intermediate$glm);
    if (is.vector(res$intermediate$vif)) {
      res$intermediate$tolerance <- 1/res$intermediate$vif;
    }
  }

  ### get summary for lm objects
  res$intermediate$summary <- summary(res$intermediate$glm);

  ### Generate output
  res$output$coef <- cbind(data.frame(res$intermediate$confint[, 1],
                                      res$intermediate$confint[, 2]),
                           res$intermediate$summary$coefficients);

  names(res$output$coef) <-
    c(paste0(conf.level*100,"% CI, lo"),
      paste0(conf.level*100,"% CI, hi"),
      'estimate', 'se', 'z', 'p');

  ### Odds ratios
  res$output$coef_oddsRatios <-
    data.frame(exp(res$output$coef[, 1]),
               exp(res$output$coef[, 2]),
               exp(res$output$coef[, 3]));
  names(res$output$coef_oddsRatios) <-
    paste0("OR: ",
           names(res$output$coef)[1:3]);

  ######################################################################
  ### Make the prediction tables
  ######################################################################

  res$intermediate$predictedY.raw <-
    stats::predict(res$intermediate$glm);

  ### Convert to probability
  res$intermediate$predictedY.prob <- exp(res$intermediate$predictedY.raw) /
    (1 + exp(res$intermediate$predictedY.raw));

  ### Round to 0 or 1
  res$intermediate$predictedY.dichotomous <-
    round(res$intermediate$predictedY.prob);

  res$output$crossTab.model <- table(res$intermediate$dat.raw[, 1],
                                     res$intermediate$predictedY.dichotomous,
                                     dnn=c("Observed", "Predicted"));
  row.names(res$output$crossTab.model) <-
    #levelNames;
    sort(unique(res$intermediate$dat.raw[, 1]));
  colnames(res$output$crossTab.model) <-
    #levelNames;
    sort(unique(res$intermediate$predictedY.dichotomous));

  ### Best predictions on the basis of the null model is either 0 or 1
  if (
    (
      table(res$intermediate$dat.raw[, 1])[1] /
      sum(table(res$intermediate$dat.raw[, 1]))
    ) < .5) {
    res$intermediate$predictedY.null <- rep(1, nrow(res$intermediate$dat.raw));
  } else {
    res$intermediate$predictedY.null <- rep(0, nrow(res$intermediate$dat.raw));
  }

  ### Build crosstables
  # res$output$crossTab.model <- table(res$intermediate$dat.raw[, 1],
  #                                    res$intermediate$predictedY.dichotomous,
  #                                    dnn=c("Observed", "Predicted"));
  res$output$crossTab.null <- table(res$intermediate$dat.raw[, 1],
                                    res$intermediate$predictedY.null,
                                    dnn=c("Observed", "Predicted"));

  row.names(res$output$crossTab.null) <-
    levelNames;

  if (res$intermediate$predictedY.null[1] == 0) {
    colnames(res$output$crossTab.null) <-
      levelNames[1];
  } else {
    colnames(res$output$crossTab.null) <-
      levelNames[2];
  }

  ### Compute the proportion of correct predictions for the null model
  ### and the real model
  res$output$proportionCorrect.null <- max(res$output$crossTab.null) /
    sum(res$output$crossTab.null);
  if (ncol(res$output$crossTab.model) == 2) {
    res$output$proportionCorrect.model <- sum(diag(res$output$crossTab.model)) /
      sum(res$output$crossTab.model);
  } else {
    res$output$proportionCorrect.model <-
      isTRUE(
        res$intermediate$predictedY.raw ==
          res$intermediate$dat.raw[, 1]
      ) / sum(res$output$crossTab.model);
  }

  if (plot) {
    if (length(res$intermediate$variables_namesOnly) == 2) {

      ### Get a vector of equally distributed values for predictor
      res$intermediate$plotDat <-
        data.frame(x = seq(from = min(res$intermediate$dat.raw[, 2]),
                           to = max(res$intermediate$dat.raw[, 2]),
                           length.out=10000));
      names(res$intermediate$plotDat) <- res$intermediate$variables_namesOnly[2];

      ### Get predicted log odds and add them to the dataframe
      res$intermediate$predictedData <-
        stats::predict(res$intermediate$glm,
                       newdata=res$intermediate$plotDat,
                       se.fit=TRUE);
      res$intermediate$plotDat$predictedLogOdds <-
        res$intermediate$predictedData$fit;
      res$intermediate$plotDat$predictionSE <-
        res$intermediate$predictedData$se.fit;

      convertLogOdds <- function(x) {
        return(exp(x) / (1 + exp(x)));
      }

      ### Convert to odds and add confidence intervals
      zValue <- stats::qnorm(1 - (1-conf.level)/2);
      res$intermediate$plotDat$predictedProbability <-
        convertLogOdds(res$intermediate$plotDat$predictedLogOdds);
      res$intermediate$plotDat$ci.lo <-
        convertLogOdds(res$intermediate$plotDat$predictedLogOdds -
                         (zValue * res$intermediate$plotDat$predictionSE));
      res$intermediate$plotDat$ci.hi <-
        convertLogOdds(res$intermediate$plotDat$predictedLogOdds +
                         (zValue * res$intermediate$plotDat$predictionSE));

      ### Create dataframe for observed values
      res$intermediate$plotDatObserved <- res$intermediate$dat.raw;
      tmpDat <- res$intermediate$dat.raw;

      if (is.numeric(binObservedMeans)) {

        tmpDat[, res$intermediate$variables_namesOnly[2]] <-
          cut(tmpDat[, res$intermediate$variables_namesOnly[2]],
              breaks=binObservedMeans);

        res$intermediate$binCenters <-
          levels(tmpDat[, res$intermediate$variables_namesOnly[2]]);

        res$intermediate$binCenters <- substr(res$intermediate$binCenters,
                                              2,
                                              nchar(res$intermediate$binCenters) - 1);

        res$intermediate$binCenters <- sapply(strsplit(res$intermediate$binCenters, ","),
                                              function(x) return(mean(as.numeric(x))));

        names(res$intermediate$binCenters) <-
          levels(tmpDat[, res$intermediate$variables_namesOnly[2]]);
        tmpDat[, res$intermediate$variables_namesOnly[2]] <-
          res$intermediate$binCenters[tmpDat[, res$intermediate$variables_namesOnly[2]]];

      }

      res$intermediate$plotDatObservedMeanPrep <- tmpDat;

      res$intermediate$plotDatObservedMean <-
        plyr::ddply(tmpDat,
                    res$intermediate$variables_namesOnly[2],
                    function(x) {
                      return(mean(x[, res$intermediate$variables_namesOnly[1]], na.rm = TRUE))

                    })

      names(res$intermediate$plotDatObservedMean) <- c('x', 'y')

      if (is.null(observedMeansWidth)) {
        observedMeansWidth <- (diff(range(res$intermediate$dat.raw[, 1])));
        if (observedMeansWidth < (diff(range(res$intermediate$dat.raw[, 2])) / 20)) {
          observedMeansWidth <- (diff(range(res$intermediate$dat.raw[, 2])) / 20);
        }
      }

      res$intermediate$plotDatObservedMean$minX <-
        res$intermediate$plotDatObservedMean$x - (observedMeansWidth / 2)
      res$intermediate$plotDatObservedMean$maxX <-
        res$intermediate$plotDatObservedMean$x + (observedMeansWidth / 2)

      ### Compute jitter parameters
      jitterWidth <-
        ufs::findShortestInterval(res$intermediate$plotDatObserved[, res$intermediate$variables_namesOnly[2]]) / 2


      res$output$plot <-
        ggplot2::ggplot(
          res$intermediate$plotDat,
          ggplot2::aes_string(
            y = 'predictedProbability',
            x = res$intermediate$variables_namesOnly[2]
          )
        ) +
        ggplot2::geom_ribbon(
          mapping = ggplot2::aes_string(ymin = 'ci.lo',
                                        ymax = 'ci.hi'),
          fill = predictionColor,
          alpha = predictionAlpha
        ) +
        ggplot2::geom_line(color = predictionColor,
                           size = predictionSize) +
        ggplot2::geom_jitter(
          data = res$intermediate$plotDatObserved,
          ggplot2::aes_string(
            x = res$intermediate$variables_namesOnly[2],
            y = res$intermediate$variables_namesOnly[1]
          ),
          color = dataColor,
          alpha = dataAlpha,
          width = jitterWidth,
          size = dataSize,
          height = .025
        ) +
        ggplot2::geom_segment(
          data = res$intermediate$plotDatObservedMean,
          ggplot2::aes_string(
            x = 'minX',
            y = 'y',
            xend = 'maxX',
            yend = 'y'
          ),
          color = observedMeansColor,
          size = observedMeansSize,
          alpha = observedMeansAlpha
        ) +
        ggplot2::labs(y = "Predicted probability") +
        theme

      # res$output$plot <- ggplot(res$intermediate$dat.raw,
      #                           aes_string(y=res$intermediate$variables_namesOnly[1],
      #                                   x=res$intermediate$variables_namesOnly[2])) +
      #   geom_point(alpha = pointAlpha) + geom_smooth(method='lm') + theme_bw();
    } else {
      warning("You requested a plot, but for now plots are ",
              "only available for logistic regression ",
              "analyses with one predictor.");
    }
  }

  class(res) <- 'rosettaLogRegr';
  return(res);

}

###-----------------------------------------------------------------------------

#' @rdname rosettaLogRegr
#' @export
rosettaLogRegr_partial <- function(x,
                                   digits = x$input$digits,
                                   pvalueDigits=x$input$pvalueDigits,
                                   headingLevel = x$input$headingLevel,
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
      system.file("partials", "_rosettaLogRegr_partial.Rmd",
                  package="rosetta");
  }

  rmdpartials::partial(rmdPartialFilename);

}

###-----------------------------------------------------------------------------

#' @rdname rosettaLogRegr
#' @method knit_print rosettaLogRegr
#' @importFrom knitr knit_print
#' @export
knit_print.rosettaLogRegr <- function(x,
                                      digits = x$input$digits,
                                      headingLevel = x$input$headingLevel,
                                      pvalueDigits=x$input$pvalueDigits,
                                      echoPartial = FALSE,
                                      partialFile = NULL,
                                      quiet=TRUE,
                                      ...) {
  rosettaLogRegr_partial(x = x,
                         headingLevel = headingLevel,
                         quiet = quiet,
                         echoPartial = echoPartial,
                         partialFile = partialFile,
                         digits = digits,
                         pvalueDigits = pvalueDigits,
                         ...);
}

###-----------------------------------------------------------------------------

#' @method print rosettaLogRegr
#' @rdname rosettaLogRegr
#' @export
print.rosettaLogRegr <- function(x, digits=x$input$digits,
                                 pvalueDigits=x$input$pvalueDigits,
                                 headingLevel = x$input$headingLevel,
                                 forceKnitrOutput = FALSE,
                                 ...) {

  if (isTRUE(getOption('knitr.in.progress')) || forceKnitrOutput) {

    rosettaLogRegr_partial(x = x,
                           digits = digits,
                           headingLevel = headingLevel,
                           ...);

  } else {

    cat0("Logistic regression analysis\n  Formula: ",
         x$intermediate$formula.as.character, "\n",
         "  Sample size: ",
         sum(stats::complete.cases(x$intermediate$dat.raw)), "\n",
         "  Predicting: ",
         x$intermediate$levelNames[2],
         "\n\n",
         "Significance test of the entire model (all predictors together):\n",
         "  Cox & Snell R-squared: ",
         formatR(x$intermediate$CoxSnellRsq, digits),
         ",\n",
         "  Nagelkerke R-squared: ",
         formatR(x$intermediate$NagelkerkeRsq, digits),
         "\n",
         "  Test for significance: ChiSq[",
         x$intermediate$chiDf,
         "] = ",
         round(x$intermediate$modelChi, digits),
         ", ",
         formatPvalue(x$intermediate$deltaChisq, digits=pvalueDigits), "\n");

    if (x$input$crossTabs) {
      cat0("\nPredictions by the null model (",
           round(100 * x$output$proportionCorrect.null, digits),
           "% correct):\n\n");
      print(x$output$crossTab.null);

      cat0("\nPredictions by the tested model (",
           round(100 * x$output$proportionCorrect.model, digits),
           "% correct):\n\n");
      print(x$output$crossTab.model);
    }

    cat("\nRaw regression coefficients (log odds values, called 'B' in SPSS):\n\n");
    tmpDat <- round(x$output$coef[, 1:5], digits);
    tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
    tmpDat[[2]] <- NULL;
    names(tmpDat)[1] <- paste0(x$input$conf.level*100, "% conf. int.");
    tmpDat$p <- formatPvalue(x$output$coef$p,
                                  digits=pvalueDigits,
                                  includeP=FALSE);
    print(tmpDat, ...);

    if (x$input$oddsRatios) {
      cat("\nRegression coefficients as odds ratios (ORs, called 'Exp(B)' in SPSS):\n\n");
      tmpDat <- round(x$output$coef_oddsRatios, digits);
      tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
      tmpDat[[2]] <- NULL;
      names(tmpDat) <- c(
        paste0("OR ", x$input$conf.level*100, "% conf. int."),
        "OR point estimate"
      );
      print(tmpDat, ...);
    }

    if (x$input$collinearity && (!is.null(x$intermediate$vif))) {
      cat0("\nCollinearity diagnostics:\n\n");
      if (is.vector(x$intermediate$vif)) {
        collinearityDat <- data.frame(
          VIF = round(x$intermediate$vif, digits),
          Tolerance = round(x$intermediate$tolerance, digits)
        );
        row.names(collinearityDat) <- paste0(ufs::repStr(4), names(x$intermediate$vif));
        print(collinearityDat);
      }
    }

    if (!is.null(x$output$plot)) {
      print(x$output$plot);
    }

    cat("\n");

    invisible();
  }
}
