#' Flexible anova
#'
#' This function is meant as a userfriendly wrapper to approximate the way
#' analysis of variance is done in SPSS.
#'
#' This wrapper uses \code{\link{oneway}} and \code{\link{lm}} and
#' \code{\link{lmer}} in combination with \code{car}'s \code{\link{Anova}}
#' function to conduct the analysis of variance.
#'
#' @param data The dataset containing the variables to analyse.
#' @param y The dependent variable. For oneway anova, factorial anova, or
#' ancova, this is the name of a variable in dataframe \code{data}. For
#' repeated measures anova, this is a vector with the names of all variable
#' names in dataframe \code{data}, e.g. \code{c('t0_value', 't1_value',
#' 't2_value')}.
#' @param between A vector with the variables name(s) of the between subjects
#' factor(s).
#' @param covar A vector with the variables name(s) of the covariate(s).
#' @param withinReference Number of reference category (variable) for within
#' subjects treatment contrast (dummy).
#' @param betweenReference Name of reference category for between subject factor
#' in RM anova.
#' @param withinNames Names of within subjects categories (dependent variables).
#' @param plot Whether to produce a plot. Note that a plot is only produced for
#' oneway and twoway anova and oneway repeated measures designs: if covariates
#' or more than two between-subjects factors are specified, not plot is
#' produced. For twoway anova designs, the second predictor is plotted as
#' moderator (and the first predictor is plotted on the x axis).
#' @param levene Whether to show Levene's test for equality of variances (using
#' \code{car}'s \code{\link{leveneTest}} function but specifying
#' \code{\link{mean}} as function to compute the center of each group).
#' @param digits Number of digits (actually: decimals) to use when printing
#' results. The p-value is printed with one extra digit.
#' @param contrast This functionality has been implemented for repeated measures only.
#' @param x The object to print (i.e. as produced by `regr`).
#' @param \dots Any additional arguments are ignored.
#' @return Mainly, this function prints its results, but it also returns them
#' in an object containing three lists: \item{input}{The arguments specified
#' when calling the function} \item{intermediate}{Intermediat objects and
#' values} \item{output}{The results such as the plot.}
#' @author Gjalt-Jorn Peters and Peter Verboon
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{regr}} and \code{\link{logRegr}} for similar functions
#' for linear and logistic regression and \code{\link{oneway}},
#' \code{\link{lm}}, \code{\link{lmer}} and \code{\link{Anova}} for the
#' functions used behind the scenes.
#' @keywords htest hplot factorial anova repeated measures
#' @examples
#'
#' ### Oneway anova with a plot
#' fanova(dat=mtcars, y='mpg', between='cyl', plot=TRUE);
#'
#' ### Factorial anova
#' fanova(dat=mtcars, y='mpg', between=c('vs', 'am'), plot=TRUE);
#'
#' ### Ancova
#' fanova(dat=mtcars, y='mpg', between=c('vs', 'am'), covar='hp');
#'
#' ### Don't run these examples to not take too much time during testing
#' ### for CRAN
#' \dontrun{
#' ### Repeated measures anova; first generate datafile
#' dat <- mtcars[, c('am', 'drat', 'wt')];
#' names(dat) <- c('factor', 't0_dependentVar' ,'t1_dependentVar');
#' dat$factor <- factor(dat$factor);
#'
#' ### Then do the repeated measures anova
#' fanova(dat, y=c('t0_dependentVar' ,'t1_dependentVar'),
#'        between='factor', plot=TRUE);
#' }
#'
#' @rdname fanova
#' @export
fanova <- function(data,
                   y,
                   between = NULL,
                   covar = NULL,
                   withinReference = 1,
                   betweenReference = NULL,
                   withinNames = NULL,
                   plot = FALSE,
                   levene = FALSE,
                   digits = 2,
                   contrast = NULL) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  res$intermediate$dataName <- as.character(deparse(substitute(data)));

  if (length(y) == 1) {
    res$intermediate$yVarName <- y;
  } else {
    res$intermediate$yVarName <- ufs::sharedSubString(y);
    if (is.na(res$intermediate$yVarName))
      res$intermediate$yVarName <- ufs::vecTxt(y);
  }

  res$output$msg <- paste0("Flexible Analysis of Variance was called with:\n\n",
                           "  Dependent variable: ", res$intermediate$yVarName, "\n",
                           ifelse(is.null(between), "", paste0("  Factors: ", ufs::vecTxt(between), "\n")),
                           ifelse(is.null(covar), "", paste0("  Covariates: ", ufs::vecTxt(covar), "\n")),
                           "\n");


  ### Convert 'between' variables (factors) to factors
  for (currentVar in seq_along(between)) {
    if (!is.factor(data[, between[currentVar]])) {
      ufs::cat0("Between-subjects factor ", between[currentVar],
                " does not have class 'factor' in dataframe '",
                res$intermediate$dataName, "'. Converting it now.\n");
      data[, between[currentVar]] <- factor(data[, between[currentVar]]);
    }
  }

  if (length(y) == 1) {

    ### oneway, factorial anova or ancova

    ### Generate formula (interactions between factors only)
    factorPart <- paste(y, "~", paste(c(between), collapse="*"))
    res$intermediate$formula <-
      stats::formula(paste(c(factorPart, covar), collapse = "+"));

    ### Run linear model
    res$intermediate$lmResult <-
      stats::lm(formula=res$intermediate$formula,
                data = data )

    ### Get Anova results using car's Anova
    res$intermediate$anovaTable1 <-
      car::Anova(res$intermediate$lmResult, type=3);

    suppressMessages(res$intermediate$anovaTable2 <-
                       stats::anova(res$intermediate$lmResult));

    ### Gather coefficients
    res$intermediate$coefs <- (summary(res$intermediate$lmResult))$coefficients



    ### Make a plot if we want one
    if (plot) {
      if (is.null(covar) && (length(between) < 3)) {
        if (length(between) == 1) moderator <- NULL
        if (length(between) == 2) moderator <- between[2]
        res$output$plot <- dlvPlot(data,
                                   y=y,
                                   x=between[1],
                                   z=moderator)$plot
      } else {
        warning("Sorry, I can only generate a plot for oneway or ",
                "two-way anovas (not for ancovas or anovas with ",
                "more than two factors).");
      }
    }

    ### Optional Levene's test
    if (levene) {
      if (length(between) == 1) {
        leveneGroups <- as.factor(data[,between])
      } else {
        leveneGroups <-
          as.factor(apply(data[, between], 1, function(x) return(paste0(x, collapse="-"))))
      }
      res$intermediate$leveneTest <-
        car::leveneTest(y = data[,y], group = leveneGroups)
    }

  } else {
    ### We need to do a repeated measures anova, so first convert the
    ### data to a long format.

    ## Note: this model can only deal with one between and one within factor, more covariates are possible

    if (length(between) > 1) {
      ufs::cat0("\nWarning: only one between subjects factor can be modelled in a repeated measures model.
           The first one you specified will be taken.")
      between <- between[1]
    }

    ## The factors are given treatment contrast (dummy coding), wth reference category specified by user
    ## First, take the first category of the between factor as reference, if not specified
    ### Set contrast function; first set default contrast for repeated
    ### measures anova

    contrastFunction <- "dummy"
    if (!is.null(contrast)) {
      contrastFunction <- paste0('contr.', contrast);
    }

    if (is.null(betweenReference)) betweenReference <- levels(data[,between])[1]

    longDat <- data.frame(subject = factor(rep(row.names(data), length(y))),
                          withinFactor = factor(rep(seq_along(y), each=nrow(data))),
                          y = unlist(data[, y]));
    for (currentVar in between) {
      longDat[, currentVar] <- factor(rep(data[, currentVar],
                                          length(y)));
      lengthCurrent <- length(levels(longDat[, currentVar]))
      refNr <-
        (1:lengthCurrent)[levels(longDat[, currentVar]) == betweenReference]
      stats::contrasts(longDat[, currentVar]) <- stats::contr.treatment(n = lengthCurrent, base = refNr)
    }
    for (currentVar in covar) {
      longDat[, currentVar] <- as.numeric(rep(data[, currentVar],
                                              length(y)));
    }

    levels(longDat$withinFactor) <- withinNames

    ## Take the first category of the within factor as reference, if not specified
    if (withinReference > length(y)) {
      ufs::cat0("\nWarning: Specification of the reference category for the within subjects factor is incorrect.
           First category is taken as reference category.")
      withinReference <- 1
    }
    ## set within contrast if specified
    if (contrastFunction == "contr.poly") {
      stats::contrasts(longDat$withinFactor) <-
        stats::contr.poly(n = length(y), scores= 1:length(y), contrasts =TRUE)
    }
    if (contrastFunction == "contr.sum") {
      stats::contrasts(longDat$withinFactor) <-
        stats::contr.sum(n = length(y), contrasts =TRUE)
    }
    if (contrastFunction == "contr.helmert") {
      stats::contrasts(longDat$withinFactor) <-
        stats::contr.helmert(n = length(y), contrasts =TRUE)
    }
    if (contrastFunction == "dummy") {
      stats::contrasts(longDat$withinFactor) <-
        stats::contr.treatment(n = length(y), base = withinReference)
    }


    res$intermediate$longDat <- longDat;

    ### Then build the lmer formula: y is predicted by all
    ### interactions and main effects for all factors ('between')
    ### and covariates ('covar'), all interactions between all
    ### factors and the time variable (called 'time'), and
    ### the random slope for time (the final term).

    covarPart <- NULL
    covarPart <- if (!is.null(covar)) paste(c(covar), collapse=" + ")
    factorPart <- paste(c(between, "withinFactor"), collapse=" * ")
    res$intermediate$formula <-
      stats::formula(paste(paste("y ~",
                                 paste(c(covarPart, factorPart), collapse=" + "),
                                 "+ (1|subject)")))


    ### Run the mixed model
    res$intermediate$lmerResult <-
      lmerTest::lmer(formula=res$intermediate$formula,
                     data = longDat)

    ### Run the analysis of variance
    suppressMessages(res$intermediate$anovaTable1 <-
                       car::Anova(res$intermediate$lmerResult,
                                  type=3, test.statistic="F"));

    suppressMessages(res$intermediate$anovaTable2 <-
                       stats::anova(res$intermediate$lmerResult));


    ### Produce plot if requested
    if (plot) {
      if (length(between) > 1) {
        warning("Sorry, I can only generate a plot for ",
                "oneway repeated measures anovas.");
      } else {
        res$output$plot <-
          dlvPlot(longDat,
                  x='withinFactor',
                  y='y',
                  z=between)$plot +
          ggplot2::labs(x='withinFactor', y=res$intermediate$yVarName);
      }
    }

  }


  class(res) <- 'fanova';
  return(res);

}

#' @method print fanova
#' @rdname fanova
#' @export
print.fanova <- function(x, digits=x$input$digits, ...) {
  cat(x$output$msg);
  cat("\n");
  if (!is.null(x$output$plot)) {
    grid::grid.newpage();
    grid::grid.draw(x$output$plot);
  }

  if (length(x$input$y) == 1) {
    ufs::cat0("\nAnova table with effect sizes \n\n")
    a <- x$intermediate$anovaTable2
    tab <- sjstats::anova_stats(a)
    print(tab[,c(1:6,8,9)])
  }

  if (length(x$input$y) > 1) {

    print(x$intermediate$anovaTable2)

    if (!is.null(x$input$contrast)) {
      a <- lme4::getME(x$intermediate$lmerResult,"X")
      b <- attr(a, "contrasts")
      withinContrast <- b$withinFactor
    } else {
      withinContrast <- stats::contrasts(x$intermediate$longDat$withinFactor)
    }

    ufs::cat0("\n\nMultilevel results \n\n")
    print(summary(x$intermediate$lmerResult), correlation = FALSE)
    ufs::cat0("\n\nContrasts of the within subjects factor \n")
    print(withinContrast, digits = digits)
    #      ufs::cat0(c("Category sequence: ",paste("",levels(x$intermediate$longDat$withinFactor))))

    if (!is.null(x$input$between)) {
      varName <- x$input$between[1]
      ufs::cat0("\n\n",paste0("Contrasts of the between subjects factor: ",varName), "\n")
      print(stats::contrasts(x$intermediate$longDat[,varName]))
    }
  }





  if(!is.null(x$intermediate$leveneTest)) {
    ufs::cat0("\n### Levene's test for homogeneity of variances:\n\n",
              "F[", x$intermediate$leveneTest[1, 1],
              ", ", x$intermediate$leveneTest[2, 1],
              "] = ", round(x$intermediate$leveneTest[1, 2], digits),
              ", ", ufs::formatPvalue(x$intermediate$leveneTest[1, 3], digits=digits+1),
              ".\n");
  }

}

