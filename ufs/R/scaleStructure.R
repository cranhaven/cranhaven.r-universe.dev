#' scaleStructure
#'
#' The scaleStructure function (which was originally called scaleReliability)
#' computes a number of measures to assess scale reliability and internal
#' consistency. Note that to compute omega, the `MBESS` and/or the
#' `psych` packages need to be installed, which are suggested packages and
#' therefore should be installed separately (i.e. won't be installed
#' automatically).
#'
#' If you use this function in an academic paper, please cite Peters (2014),
#' where the function is introduced, and/or Crutzen & Peters (2015), where the
#' function is discussed from a broader perspective.
#'
#' This function is basically a wrapper for functions from the psych and MBESS
#' packages that compute measures of reliability and internal consistency. For
#' backwards compatibility, in addition to \code{scaleStructure},
#' \code{scaleReliability} can also be used to call this function.
#'
#' @aliases scaleStructure scaleReliability scaleStructurePartial
#' @param data A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items = 'all'. If \code{dat} is \code{NULL},
#' a the \code{\link{getData}} function will be called to show the user a
#' dialog to open a file.
#' @param items If not 'all', this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param digits Number of digits to use in the presentation of the results.
#' @param ci Whether to compute confidence intervals as well. This requires the
#' suggested MBESS package, which has to be installed separately. If true, the
#' method specified in \code{interval.type} is used. When specifying a
#' bootstrapping method, this can take quite a while!
#' @param interval.type Method to use when computing confidence intervals. The
#' list of methods is explained in the help file for `ci.reliability` in MBESS.
#' Note that when
#' specifying a bootstrapping method, the method will be set to
#' \code{normal-theory} for computing the confidence intervals for the ordinal
#' estimates, because these are based on the polychoric correlation matrix, and
#' raw data is required for bootstrapping.
#' @param conf.level The confidence of the confidence intervals.
#' @param silent If computing confidence intervals, the user is warned that it
#' may take a while, unless \code{silent=TRUE}.
#' @param samples The number of samples to compute for the bootstrapping of the
#' confidence intervals.
#' @param bootstrapSeed The seed to use for the bootstrapping - setting this
#' seed makes it possible to replicate the exact same intervals, which is
#' useful for publications.
#' @param omega.psych Whether to also compute the interval estimate for omega
#' using the `omega` function in the `psych` package.
#' The default point estimate and confidence interval for omega are based on
#' the procedure suggested by Dunn, Baguley & Brunsden (2013) using the
#' `MBESS` function `ci.reliability` (because it has
#' more options for computing confidence intervals, not always requiring
#' bootstrapping), whereas the `psych` package point estimate was
#' suggested in Revelle & Zinbarg (2008). The `psych` estimate
#' usually (perhaps always) results in higher estimates for omega.
#' @param omega.psych_nfactors The number of factor to use in the factor
#' analysis when computing Omega. The default in [psych::omega()] is 3; to
#' obtain the same results as in jamovi's "Reliability", set this to 1.
#' @param omega.psych_flip Whether to let `psych` automatically flip items with
#' negative correlations. The default in [psych::omega()] is`TRUE`; to obtain
#' the same results as in jamovi's "Reliability", set this to `FALSE`.
#' @param poly Whether to compute ordinal measures (if the items have
#' sufficiently few categories).
#' @param headingLevel The level of the Markdown heading to provide; basically
#' the number of hashes ('`#`') to prepend to the headings.
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available, which is the result of a call to
#' `scaleStructure()`.
#' @param x The object to print
#' @param suppressSuggestedPkgsMsg Whether to suppress the message about the
#' suggested `MBESS` and `psych` packages.
#' @param ... Any additional arguments for the default print function.
#' @return
#'
#' An object with the input and several output variables. Most notably:
#' \item{input}{Input specified when calling the function}
#' \item{intermediate}{Intermediate values and objects computed to get to the
#' final results} \item{output}{Values of reliability / internal consistency
#' measures, with as most notable elements:} \item{output$dat}{A dataframe with
#' the most important outcomes} \item{output$omega}{Point estimate for omega}
#' \item{output$glb}{Point estimate for the Greatest Lower Bound}
#' \item{output$alpha}{Point estimate for Cronbach's alpha}
#' \item{output$coefficientH}{Coefficient H} \item{output$omega.ci}{Confidence
#' interval for omega} \item{output$alpha.ci}{Confidence interval for
#' Cronbach's alpha}
#' @author Gjalt-Jorn Peters and Daniel McNeish (University of North Carolina,
#' Chapel Hill, US).
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso [psych::omega()], [psych::alpha()], and
#' [MBESS::ci.reliability()].
#' @references Crutzen, R., & Peters, G.-J. Y. (2015). Scale quality: alpha is
#' an inadequate estimate and factor-analytic evidence is needed first of all.
#' \emph{Health Psychology Review.}
#' \doi{10.1080/17437199.2015.1124240}
#'
#' Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: A
#' practical solution to the pervasive problem of internal consistency
#' estimation. \emph{British Journal of Psychology}, 105(3), 399-412.
#' \doi{10.1111/bjop.12046}
#'
#' Eisinga, R., Grotenhuis, M. Te, & Pelzer, B. (2013). The reliability of a
#' two-item scale: Pearson, Cronbach, or Spearman-Brown? \emph{International
#' Journal of Public Health}, 58(4), 637-42.
#' \doi{10.1007/s00038-012-0416-3}
#'
#' Gadermann, A. M., Guhn, M., Zumbo, B. D., & Columbia, B. (2012). Estimating
#' ordinal reliability for Likert-type and ordinal item response data: A
#' conceptual, empirical, and practical guide. \emph{Practical Assessment,
#' Research & Evaluation}, 17(3), 1-12. \doi{10.7275/n560-j767}
#'
#' Peters, G.-J. Y. (2014). The alpha and the omega of scale reliability and
#' validity: why and how to abandon Cronbach's alpha and the route towards more
#' comprehensive assessment of scale quality. \emph{European Health
#' Psychologist}, 16(2), 56-69.
#' \doi{10.31234/osf.io/h47fv}
#'
#' Revelle, W., & Zinbarg, R. E. (2009). Coefficients Alpha, Beta, Omega, and
#' the glb: Comments on Sijtsma. \emph{Psychometrika}, 74(1), 145-154.
#' \doi{10.1007/s11336-008-9102-z}
#'
#' Sijtsma, K. (2009). On the Use, the Misuse, and the Very Limited Usefulness
#' of Cronbach's Alpha. \emph{Psychometrika}, 74(1), 107-120.
#' \doi{10.1007/s11336-008-9101-0}
#'
#' Zinbarg, R. E., Revelle, W., Yovel, I., & Li, W. (2005). Cronbach's alpha,
#' Revelle's beta and McDonald's omega H: Their relations with each other and
#' two alternative conceptualizations of reliability. \emph{Psychometrika},
#' 70(1), 123-133. \doi{10.1007/s11336-003-0974-7}
#'
#' @keywords utilities univar
#' @rdname scaleStructure
#' @examples
#'
#'
#' \dontrun{
#' ### (These examples take a lot of time, so they are not run
#' ###  during testing.)
#'
#' ### This will prompt the user to select an SPSS file
#' scaleStructure();
#'
#' ### Load data from simulated dataset testRetestSimData (which
#' ### satisfies essential tau-equivalence).
#' data(testRetestSimData);
#'
#' ### Select some items in the first measurement
#' exampleData <- testRetestSimData[2:6];
#'
#' ### Use all items (don't order confidence intervals to save time
#' ### during automated testing of the example)
#' ufs::scaleStructure(dat=exampleData, ci=FALSE);
#'
#' ### Use a selection of three variables (without confidence
#' ### intervals to save time
#' ufs::scaleStructure(
#'   dat=exampleData,
#'   items=c('t0_item2', 't0_item3', 't0_item4'),
#'   ci=FALSE
#' );
#'
#' ### Make the items resemble an ordered categorical (ordinal) scale
#' ordinalExampleData <- data.frame(apply(exampleData, 2, cut,
#'                                        breaks=5, ordered_result=TRUE,
#'                                        labels=as.character(1:5)));
#'
#' ### Now we also get estimates assuming the ordinal measurement level
#' ufs::scaleStructure(ordinalExampleData, ci=FALSE);
#' }
#'
#'
#' @export scaleStructure
scaleStructure <- scaleReliability <- function (data=NULL, items = 'all', digits = 2,
                                                ci = TRUE, interval.type="normal-theory",
                                                conf.level=.95, silent=FALSE,
                                                samples=1000, bootstrapSeed = NULL,
                                                omega.psych = TRUE,
                                                omega.psych_nfactors = 3,
                                                omega.psych_flip = TRUE,
                                                poly = TRUE,
                                                suppressSuggestedPkgsMsg = FALSE,
                                                headingLevel = 3) {

  if (!suppressSuggestedPkgsMsg) {
    if (!requireNamespace("MBESS", quietly = TRUE)) {
      if (!requireNamespace("psych", quietly = TRUE)) {
        message(paste0("The `MBESS` and `psych` packages are not installed, but at ",
                       "least one of them is required if you also want to compute ",
                       "omega. You can install them using one or both of these commands:\n",
                       "install.packages('MBESS');\n",
                       "install.packages('psych');\n"));
      }
    }
  }

  ### Make object to store results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  dat <- data;

  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(data)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid datafile selected in ",
                                       "the dialog I then showed to allow selection of a dataset.",
                                       "Original error:\n\n[defaultErrorMessage]"),
                   use.value.labels=FALSE, applyRioLabels = FALSE, silent=TRUE);
    res$input$dat.name <- paste0("SPSS file imported from ", attr(dat, "fileName"));
  }
  else {
    if (!is.data.frame(dat)) {
      stop("Argument 'dat' must be a dataframe or NULL! Class of ",
           "provided argument: ", class(dat));
    }
    res$input$dat.name <- deparse(substitute(data));
  }

  ### if items contains only 1 element (or less), we
  ### include all items.
  if (length(items) <= 1) {
    ### Remove all cases with missing data (listwise deletion)
    res$input$dat <- stats::na.omit(dat);
  }
  else {
    ### Select relevant items and remove all cases with
    ### missing data (listwise deletion)
    res$input$dat <- stats::na.omit(subset(dat, select=items));
  }

  ### If any of the variables in the dataframe are factors, convert
  ### them to numeric vectors:
  if ("factor" %in% unlist(lapply(res$input$dat, 'class'))) {
    res$input$dat <- data.frame(lapply(res$input$dat, 'as.numeric'));
  }

  ### Set number of items and number of observations
  res$input$n.items <- ncol(res$input$dat);
  res$input$n.observations <- nrow(res$input$dat);

  if (samples < res$input$n.observations) {
    res$intermediate$samples <- samples <- 1.2*res$input$n.observations;
  }

  ### Also generate a dataframe (useful when
  ### requesting measures for many scales)
  res$output$dat <- data.frame(n.items        = res$input$n.items,
                               n.observations = res$input$n.observations);

  ### Get correlation matrix
  res$intermediate$cor <- stats::cor(res$input$dat, use="complete.obs");

  ### Store number and proportion of positive correlations
  res$intermediate$cor.pos <-
    sum(res$intermediate$cor[lower.tri(res$intermediate$cor)] > 0);
  res$intermediate$cor.total <-
    (res$input$n.items^2 - res$input$n.items) / 2;
  res$intermediate$cor.proPos <-
    res$intermediate$cor.pos / res$intermediate$cor.total;

  ### Cronbach's alpha
  invisible(utils::capture.output(suppressMessages(suppressWarnings(res$intermediate$alpha <-
                                                                      psych::alpha(res$input$dat,
                                                                                   check.keys=FALSE)))));
  res$output$cronbach.alpha <- res$intermediate$alpha$total$raw_alpha;
  res$output$dat$cronbach.alpha <- res$output$cronbach.alpha;

  ### GLB and Omega can only be computed if the number
  ### of items exceeds two

  if (res$input$n.items == 2) {
    ### Otherwise, compute Spearman Brown coefficient
    ### (see Eisinga, te Grotenhuis & Pelzer (2013). The reliability
    ### of a two-item scale: Pearson, Cronbach, or Spearman-Brown?
    ### International journal of public health, 58(4), 637-42.
    ### doi:10.1007/s00038-012-0416-3)
    ### Get r in numeric variable for convenience
    r <- res$intermediate$cor[1,2];
    res$intermediate$spearman.brown <- 1 / (1 + (1 / ((r/(1-r)) + (r/(1-r)))));
    res$output$spearman.brown <- res$intermediate$spearman.brown;
    res$output$dat$spearman.brown <- res$intermediate$spearman.brown;
  }
  else if (res$input$n.items > 2) {

    ### Added at 2016-12-05: Coefficient H

    res$intermediate$fa <-
      suppressWarnings(psych::fa(res$input$dat,
                                 nfactors=1,
                                 fm='ml'));
    res$intermediate$loadings <- as.vector(res$intermediate$fa$Structure);
    res$intermediate$minorDenom <- 1 / sum((res$intermediate$loadings ^ 2) /
                                           (1 - (res$intermediate$loadings ^ 2)));
    res$output$coefficientH <- 1 / (1 + res$intermediate$minorDenom);

    ### GLB
    tryCatch(
      suppressWarnings(res$intermediate$glb <-
                         psych::glb(res$input$dat)),
      error = function(e) {
        warning("\n\nWhen calling `psych::glb`, it threw an ",
                "error, specifically:\n\n", e,
                "\nThis prevents me from providing an ",
                "estimate for the Greatest Lower Bound (GLB). I am setting ",
                "it to NA.");
      }
    )
    if (is.null(res$intermediate$glb)) {
      res$intermediate$glb <- list(glb.max = NA);
      res$output$glb  <- NA;
      res$output$dat$glb  <- NA;
    } else {
      res$output$glb  <- res$intermediate$glb$glb.max;
      res$output$dat$glb  <- res$intermediate$glb$glb.max;
    }

    ### Omega
    if (requireNamespace("psych", quietly = TRUE)) {
      if (requireNamespace("GPArotation", quietly = TRUE)) {
        invisible(
          utils::capture.output(
            suppressMessages(
              suppressWarnings(
                res$intermediate$omega.psych <-
                  psych::omega(res$input$dat,
                               nfactors = omega.psych_nfactors,
                               flip = omega.psych_flip,
                               plot = FALSE)
              )
            )
          )
        );
        res$output$omega.psych <- res$intermediate$omega.psych$omega.tot;
        res$output$dat$omega.psych.tot <- res$output$omega.psych;
        res$output$dat$omega.psych.h <- res$intermediate$omega.psych$omega_h;
      } else {
        message("\n\nIf you want to compute omega, you need the {psych} package, ",
                "which in turn needs the {GPArotation} package. You can ",
                "install it with:\n\ninstall.packages('GPArotation');\n\n");
        return(invisible(FALSE));
      }
    }
    if (requireNamespace("MBESS", quietly = TRUE)) {
      res$intermediate$omega <-
        MBESS::ci.reliability(res$input$dat, type="omega");
      res$output$omega <- res$intermediate$omega$est;
      res$output$dat$omega <- res$intermediate$omega$est;
    }

    ### Examine largest number of levels for the items
    res$intermediate$maxLevels <- max(apply(res$input$dat, 2,
                                            function(x){return(nlevels(factor(x)))}));
    res$intermediate$maxRange <- max(res$input$dat, na.rm=TRUE) -
      min(res$input$dat, na.rm=TRUE) + 1;

    ### If sufficiently low, also provide ordinal estimates
    if (poly && res$intermediate$maxLevels < 9 && res$intermediate$maxRange < 9) {

      if ((requireNamespace("psych", quietly = TRUE)) && (requireNamespace("MBESS", quietly = TRUE))) {

        ### Compute polychoric correlation matrix
        tryCatch({
            res$intermediate$polychor <-
              suppressWarnings(psych::polychoric(res$input$dat)$rho);
          }, error = function(e) {
            warning(paste0(
              "An error was thrown by `psych::polychoric` ",
              "(perhaps there were missing values?): ",
              e$message));
          });
        if (is.null(res$intermediate$polychor)) {
          res$intermediate$polychor <- NA;
        }

        if (!any(is.na(res$intermediate$polychor))) {
          res$intermediate$omega.ordinal <-
            MBESS::ci.reliability(
              S = res$intermediate$polychor,
              N = res$input$n.observations,
              type = "omega",
              interval.type = 'none'
            )

          res$intermediate$omega.ordinal.hierarchical <-
            MBESS::ci.reliability(
              S = res$intermediate$polychor,
              N = res$input$n.observations,
              type = "hierarchical",
              interval.type = 'none'
            )

          res$intermediate$alpha.ordinal <-
            MBESS::ci.reliability(
              S = res$intermediate$polychor,
              N = res$input$n.observations,
              type = "alpha",
              interval.type = 'none'
            )
          ################################################################
          ### 2016-10-10: replaced psych estimate with MBESS function
          ### to ensure consistency with confidence intervals
          ################################################################
          ### Ordinal omega
          # suppressWarnings(res$intermediate$omega.ordinal <-
          #                    omega(res$input$dat, poly=TRUE, plot=FALSE));
          ### Ordinal alpha
          # res$intermediate$alpha.ordinal <- alpha(res$intermediate$polychoric,
          #                                         check.keys=FALSE);
        }
      }
    }

    if (ci && requireNamespace("MBESS", quietly = TRUE)) {

      if (interval.type %in% c("bi", "perc", "bca")) {
        ### Use bootstrapping for the confidence intervals

        ### Check whether seed value is specified
        if (is.null(bootstrapSeed)) {
          bootstrapSeed <- as.numeric(format(Sys.time(), "%Y%m%d"));
          warning("No 'bootstrapSeed' specified. Without a seed for the ",
                  "random number generator, bootstrapping results will not  ",
                  "be exactly replicable. Setting current date as ",
                  "seed (", bootstrapSeed, "). Specify this value using the ",
                  "'bootstrapSeed' parameter to exactly replicate the ",
                  "bootstrapping results.");
        }

        ### Set seed
        res$input$bootstrapSeed <- bootstrapSeed;
        set.seed(res$input$bootstrapSeed);

        if (!silent) {
          cat("-- STARTING BOOTSTRAPPING TO COMPUTE CONFIDENCE INTERVALS! --\n");
          cat("--    (this might take a while, computing", samples,"samples)    --\n");
        }
      }

      ### Compute CI for alpha
      res$intermediate$alpha.ci <-
        MBESS::ci.reliability(res$input$dat, type="alpha",
                              conf.level = conf.level,
                              interval.type=interval.type, B=samples);

      ### Compute CI for omega
      res$intermediate$omega.ci <-
        MBESS::ci.reliability(res$input$dat, type="omega",
                              conf.level = conf.level,
                              interval.type=interval.type, B=samples);

      ### Confidence intervals for ordinal estimates
      if (poly && res$intermediate$maxLevels < 9 && res$intermediate$maxRange < 9) {

        ### Set interval to wald if bootstrapping was requested
        if (interval.type %in% c("bi", "perc", "bca")) {
          intervalType <- "normal-theory";
        } else {
          intervalType <- interval.type;
        }

        ### Compute CI and point estimates for ordinal alpha
        res$intermediate$alpha.ordinal.ci <-
          MBESS::ci.reliability(S=res$intermediate$polychor,
                                N = res$input$n.observations,
                                type="alpha",
                                conf.level = conf.level,
                                interval.type=intervalType);

        ### Compute CI and point estimates for ordinal omega
        res$intermediate$omega.ordinal.ci <-
          MBESS::ci.reliability(S=res$intermediate$polychor,
                                N = res$input$n.observations,
                                type="omega",
                                conf.level = conf.level,
                                interval.type=intervalType);
        res$output$dat$alpha.ordinal <- res$intermediate$alpha.ordinal.ci$est;
        res$output$dat$omega.ordinal <- res$intermediate$omega.ordinal.ci$est;

      }

      ### Extract and store ci bounds
      res$output$alpha.ci <- c(res$intermediate$alpha.ci$ci.lower,
                               res$intermediate$alpha.ci$ci.upper);
      res$output$omega.ci <- c(res$intermediate$omega.ci$ci.lower,
                               res$intermediate$omega.ci$ci.upper);
      res$output$alpha.ordinal.ci <- c(res$intermediate$alpha.ordinal.ci$ci.lower,
                                       res$intermediate$alpha.ordinal.ci$ci.upper);
      res$output$omega.ordinal.ci <- c(res$intermediate$omega.ordinal.ci$ci.lower,
                                       res$intermediate$omega.ordinal.ci$ci.upper);
      ### Add to dat
      res$output$dat$alpha.ci.lo <- res$output$alpha.ci[1];
      res$output$dat$alpha.ci.hi <- res$output$alpha.ci[2];
      res$output$dat$omega.ci.lo <- res$output$omega.ci[1];
      res$output$dat$omega.ci.hi <- res$output$omega.ci[2];
      res$output$dat$alpha.ordinal.ci.lo <- res$output$alpha.ordinal.ci[1];
      res$output$dat$alpha.ordinal.ci.hi <- res$output$alpha.ordinal.ci[2];
      res$output$dat$omega.ordinal.ci.lo <- res$output$omega.ordinal.ci[1];
      res$output$dat$omega.ordinal.ci.hi <- res$output$omega.ordinal.ci[2];
      if ((interval.type %in% c("bi", "perc", "bca")) && (!silent)) {
        cat("-- FINISHED BOOTSTRAPPING TO COMPUTE CONFIDENCE INTERVALS! --\n");
      }
    }
  }

  class(res) <- c("scaleStructure", "scaleReliability");
  ### Return result
  return(res);
}

#' @method print scaleStructure
#' @rdname scaleStructure
#' @export
print.scaleStructure <- function (x, digits=x$input$digits, ...) {

  if (utils::packageVersion('psych') < '1.5.4') {
    cat("Note: your version of package 'psych' is lower than 1.5.4 (",
        as.character(utils::packageVersion('psych')), " to be precise). This means that you ",
        "might see errors from the 'fa' function above this notice. ",
        "You can safely ignore these.\n\n", sep="");
  }

  cat(paste0("\nInformation about this analysis:\n",
             "\n                 Dataframe: ", x$input$dat.name,
             "\n                     Items: ", paste(x$input$items, collapse=", "),
             "\n              Observations: ", x$input$n.observations,
             "\n     Positive correlations: ", x$intermediate$cor.pos,
             " out of ", x$intermediate$cor.total, " (",
             round(100*x$intermediate$cor.proPos), "%)\n\n",
             "Estimates assuming interval level:\n"));
  if ((x$input$n.items > 2) && (!is.null(x$output$omega))) {
    cat(paste0("\n             Omega (total): ", round(x$output$omega, digits=digits),
               "\n      Omega (hierarchical): ",
               round(x$intermediate$omega.psych$omega_h, digits=digits)));
    if (x$input$omega.psych) {
      cat(paste0("\n   Revelle's omega (total): ", round(x$output$omega.psych, digits=digits)));
    }
    cat(paste0("\nGreatest Lower Bound (GLB): ", round(x$output$glb, digits=digits),
               "\n             Coefficient H: ", round(x$output$coefficientH, digits=digits),
               "\n         Coefficient alpha: ", round(x$output$cronbach.alpha, digits=digits), "\n"));
    if (x$input$ci & !is.null(x$output$alpha.ci)) {
      ### If confidence intervals were computed AND obtained, print them
      cat(paste0("Confidence intervals:\n             Omega (total): [",
                 round(x$output$omega.ci[1], digits=digits), ", ",
                 round(x$output$omega.ci[2], digits=digits), "]\n",
                 "         Coefficient alpha: [", round(x$output$alpha.ci[1], digits=digits),
                 ", ", round(x$output$alpha.ci[2], digits=digits), "]\n"));
    }
    if (x$input$poly && x$intermediate$maxLevels < 9 && x$intermediate$maxRange < 9) {
      if (!is.null(x$intermediate$omega.ordinal)) {
        cat(paste0("\nEstimates assuming ordinal level:\n",
                   "\n     Ordinal Omega (total): ",
                   round(x$intermediate$omega.ordinal$est, digits=digits),
                   "\n Ordinal Omega (hierarch.): ",
                   round(x$intermediate$omega.ordinal.hierarchical$est, digits=digits)));
        # if (x$input$omega.psych) {
        #   cat(paste0("\nOrd. Omega (psych package): ", round(x$intermediate$omega.ordinal$omega.tot, digits=digits)));
        # }
        cat(paste0("\n  Ordinal Cronbach's alpha: ",
                   round(x$intermediate$alpha.ordinal$est, digits=digits), "\n"));
        if (x$input$ci & !is.null(x$output$alpha.ordinal.ci)) {
          ### If confidence intervals were computed AND obtained, print them
          cat(paste0("Confidence intervals:\n     Ordinal Omega (total):  [",
                     round(x$output$omega.ordinal.ci[1], digits=digits), ", ",
                     round(x$output$omega.ordinal.ci[2], digits=digits), "]\n",
                     "  Ordinal Coefficient alpha: [", round(x$output$alpha.ordinal.ci[1], digits=digits),
                     ", ", round(x$output$alpha.ordinal.ci[2], digits=digits), "]\n"));
        }
      } else {
        cat0("\n(Estimates assuming ordinal level not computed, as the polychoric ",
             "correlation matrix has missing values.)\n");
      }
    } else if (x$input$poly == TRUE){
      cat("\n(Estimates assuming ordinal level not computed, as at least one item seems to have more than 8 levels; ",
          "the highest number of distinct levels is ", x$intermediate$maxLevels, " and the highest range is ",
          x$intermediate$maxRange, ". This last number needs to be lower than 9 for the polychoric function to work. ",
          "If this is unexpected, you may want to check for outliers.)\n", sep="");
    }
    if (x$input$omega.psych) {
      cat(paste0("\nNote: the normal point estimate and confidence interval for omega are based on the procedure suggested by ",
                 "Dunn, Baguley & Brunsden (2013) using the MBESS function ci.reliability, whereas the psych package point estimate was ",
                 "suggested in Revelle & Zinbarg (2008). See the help ('?scaleStructure') for more information.\n"));
    } else {
      cat(paste0("\nNote: the normal point estimate and confidence interval for omega are based on the procedure suggested by ",
                 "Dunn, Baguley & Brunsden (2013). To obtain the (usually higher) omega point estimate using the procedure ",
                 "suggested by Revelle & Zinbarg (2008), use argument 'omega.psych=TRUE'. See the help ('?scaleStructure') ",
                 "for more information. Of course, you can also call the 'omega' function from the psych package directly.\n"));
    }
  } else if (x$input$n.items == 2) {
    cat(paste0("\nSpearman Brown coefficient: ", round(x$output$spearman.brown, digits=digits),
               "\n         Coefficient alpha: ", round(x$output$cronbach.alpha, digits=digits),
               "\n       Pearson Correlation: ", round(x$intermediate$cor[1, 2], digits=digits), "\n\n"));
  }
  invisible();
}

#' @rdname scaleStructure
#' @export
scaleStructure_partial <- function(x,
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
      system.file("partials", "_scaleStructure_partial.Rmd", package="ufs");
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

#' @rdname scaleStructure
#' @method knit_print scaleStructure
#' @importFrom knitr knit_print
#' @export
knit_print.scaleStructure <- function(x,
                                      headingLevel = x$input$headingLevel,
                                      quiet=TRUE,
                                      echoPartial = FALSE,
                                      partialFile = NULL,
                                      ...) {
  scaleStructure_partial(x = x,
                         headingLevel = headingLevel,
                         quiet = quiet,
                         echoPartial = echoPartial,
                         partialFile = partialFile,
                         ...);
}


# #' @rdname scaleStructure
# #' @method partial scaleStructure
# #' @export
# partial.scaleStructure <- function (x,
#                                     headingLevel = 3,
#                                     quiet=TRUE,
#                                     echoPartial = FALSE,
#                                     partialFile = NULL) {
#
#   digits <- x$input$digits;
#
#   ### Get filename
#   if (!is.null(partialFile) && file.exists(partialFile)) {
#     rmdPartialFilename <-
#       partialFile;
#   } else {
#     rmdPartialFilename <-
#       system.file("partials", "_scaleStructure.rmd", package="ufs");
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

