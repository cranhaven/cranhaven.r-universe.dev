#' Numbers Needed for Change
#'
#' This function computes the Numbers Needed for Change, and shows a
#' visualisation to illustrate them. Numbers Needed for Change is
#' the name for a Numbers Needed to Treat estimate that was computed
#' for a continuous outcome as is common in behavior change research.
#'
#' Numbers Needed to Treat is a common and very useful effect size
#' measure in use in the medical sciences. It is computed based on the
#' Control Event Rate (CER) and the Experimental Event Rate (EER), and
#' expresses how many people would need to received a treatment to yield
#' a beneficial result for one person. In behavior change research, a
#' similar measure would be useful, but the outcome is normally not
#' dichotomous as is common in the medical literature (i.e. whether a
#' participants survives or is cured), but continuous. Numbers Needed
#' for Change fills this lacuna: it is simply the Numbers Needed to Treat,
#' but converted from a Cohen's d value. `nnt` is an alias for `nnc`.
#'
#' For more details, see Gruijters & Peters (2019) for details.
#'
#' @param d The value of Cohen's *d*.
#' @param cer The Control Event Rate.
#' @param r The correlation between the determinant and behavior (for mediated
#' Numbers Needed for Change).
#' @param n The sample size.
#' @param threshold If the event rate is not available, a threshold value can
#' be specified instead, which is then used in conjunction with the mean
#' (\code{mean}) and standard deviation (`sd`) and assuming a normal
#' distribution to compute the event rate.
#' @param mean The mean value, used to draw the plot, or, if no CER is provided
#' but instead the threshold value, to compute the CER.
#' @param sd The standard deviation, used to draw the plot (and to compute the
#' CER if a threshold value is supplied instead of the CER).
#' @param poweredFor The Cohen's *d* value for which the study was
#' powered. This expected Cohen's *d* value can be used to compute the
#' threshold, which then in turn is used to compute the CER. To use this
#' approach, also specify the mean and the standard deviation.
#' @param thresholdSensitivity This argument can be used to provide a vector of
#' potential threshold values, each of which is used to compute an NNC. This
#' enables easy inspection of whether the value chosen as threshold matters
#' much for the NNC.
#' @param eventDesirable Whether an event is desirable or undesirable.
#' @param eventIfHigher Whether scores above or below the threshold are
#' considered 'an event'.
#' @param conf.level The confidence level of the confidence interval.
#' @param dReliability If Cohen's d was not measured with perfect reliability,
#' `nnc` can disattenuate it to correct for the resulting attenuation using
#' [ufs::disattenuate.d()] before computating the Experimental Event Rate. Use
#' this argument to specify the reliability of the outcome measure. By default,
#' the setting of 1 means that no disattenuation is applied.
#' @param d.ci Instead of providing a point estimate for Cohen's *d*, a
#' confidence interval can be provided.
#' @param cer.ci Instead of providing a point estimate for the Control Event
#' Rate, a confidence interval can be provided.
#' @param r.ci Instead of providing a point estimate for the correlation, a
#' confidence interval can be provided.
#' @param d.n In addition to providing a point estimate for Cohen's *d*, a
#' sample size can be provided; if it is, the confidence interval is computed.
#' @param cer.n In addition to providing a point estimate for the Control Event
#' Rate, a sample size can be provided; if it is, the confidence interval is
#' computed.
#' @param r.n In addition to providing a point estimate for the correlation, a
#' sample size can be provided; if it is, the confidence interval is computed.
#' @param plot Whether to generate and show the plot.
#' @param returnPlot Whether to return the plot (as an attribute), or to only
#' display it.
#' @param silent Whether to suppress notifications.
#' @param x The `nnc` object to print.
#' @param digits The number of digits to round to.
#' @param ... Any additional arguments are passed to the `print` function.
#' @return The Numbers Needed for Change (NNC), potentially with a plot
#' visualising the NNC in an attribute.
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references Gruijters, S. L., & Peters, G. Y. (2019). Gauging the
#' impact of behavior change interventions: A tutorial on the Numbers
#' Needed to Treat. *PsyArXiv.* \doi{10.31234/osf.io/2bau7}
#' @keywords utilities
#' @examples
#'
#' ### Simple example
#' behaviorchange::nnc(d=.4, cer=.3);
#'
#' ### Or for a scenario where events are undesirable, and the
#' ### intervention effective (therefore having a negative value for d):
#' behaviorchange::nnc(d=-.4, cer=.3, eventDesirable=FALSE);
#'
#' @name nnc
#' @rdname nnc
#' @aliases nnc nnt
#' @export nnc
#' @export nnt
nnc <- nnt <- function(d = NULL, cer = NULL, r = 1, n = NULL,
                       threshold = NULL, mean = 0, sd = 1,
                       poweredFor = NULL, thresholdSensitivity = NULL,
                       eventDesirable = TRUE, eventIfHigher = TRUE,
                       conf.level=.95, dReliability = 1,
                       d.ci = NULL, cer.ci = NULL, r.ci=NULL,
                       d.n = NULL, cer.n = NULL, r.n = NULL, plot = TRUE,
                       returnPlot = TRUE, silent=FALSE) {

  numbersNeededTitle <- ifelse(curfnfinder() == "nnt",
                               "Numbers needed to treat",
                               "Numbers needed for change");

  if (is.null(d)) {
    stop("You have to provide an estimate for Cohen's d (argument 'd'). If you do not have ",
         "a Cohen's d estimate, instead use convert.d.to.t (see ?convert.d.to.t for the ",
         "manual), e.g. provide:\n\n  nnc(d=convert.t.to.d(t = 3.2, df=98));\n\n",
         "Of course, replace '3.2' and '98' with your t value and the degrees of freedom.");
  }

  if (!is.null(poweredFor) && !is.null(threshold)) {
    warning("You specified a value for both 'powerFor' and 'threshold'. Only using the latter!");
  } else if (!is.null(poweredFor) && (is.null(mean) || is.null(sd))) {
    stop("You specified 'powerFor', but to use that expected Cohen's d value to compute the ",
         "threshold, which then in turn is used to compute the CER, I also need the mean and ",
         "the standard deviation, but you didn't specify both of those.");
  } else if (!is.null(poweredFor) && !is.null(mean) && !is.null(sd)) {
    threshold <- mean + poweredFor * sd;
  }

  if (is.null(cer) && is.null(threshold)) {
    if (!silent) {
      warning("You did not specify a Control Event Rate (CER, argument 'cer'). I will assume ",
              "a Control Event Rate of 50% (cer = .5).");
      cer <- .5;
    }
  } else {
    if (length(cer) > 1) {
      stop("When specifying a confidence interval for the CER, use argument 'cer.ci'!");
    }
  }

  if (length(d) > 1) {
    stop("When specifying a confidence interval for Cohen's d, use argument 'd.ci'!");
  }
  if (length(r) > 1) {
    stop("When specifying a confidence interval for the correlation, use argument 'r.ci'!");
  }

  if (!is.null(r.ci) && (r == 1)) r <- NULL;

  ### Disattenuate Cohen's d
  d <- ufs::disattenuate.d(d, dReliability);
  if (!is.null(d.ci)) {
    d <- ufs::disattenuate.d(d.ci, dReliability);
  }

  ### Compute CER if it was not specified
  if (is.null(cer) && !is.null(threshold)) {
    cer <- behaviorchange::convert.threshold.to.er(threshold = threshold,
                                                   mean = mean,
                                                   sd = sd);
  }

  if (!is.null(thresholdSensitivity)) {
    cer.sensitivity <- behaviorchange::convert.threshold.to.er(threshold = thresholdSensitivity,
                                                               mean = mean,
                                                               sd = sd);
    eer.sensitivity <-
      ufs::convert.d.to.eer(d=d,
                            cer=cer.sensitivity,
                            eventDesirable=eventDesirable,
                            eventIfHigher=eventIfHigher);
    nnc.sensitivity <-
      ufs::convert.d.to.nnc(d=d,
                            cer=cer.sensitivity,
                            r=r,
                            eventDesirable=eventDesirable,
                            eventIfHigher=eventIfHigher);
    sensitivityDf <-
      data.frame(threshold = thresholdSensitivity,
                 cer = cer.sensitivity,
                 eer = eer.sensitivity,
                 nnc = nnc.sensitivity);
  }

  ### Compute confidence intervals if we can
  if (is.null(d.ci) && !is.null(d.n))
    d.ci <- ufs::cohensdCI(d=d, n = sum(d.n));
  if (is.null(cer.ci) && !is.null(cer.n))
    cer.ci <- stats::prop.test(cer*cer.n, cer.n)$conf.int[1:2]
  if (is.null(r.ci) && !is.null(r.n))
    r.ci <- ufs::confIntR(r=r, N = r.n);

  ### Where we were unable to compute confidence intervals, just take the
  ### point estimate as both lower and upper bounds
  if (is.null(cer.ci)) cer.ci <- rep(cer, 2);
  if (is.null(d.ci)) d.ci <- rep(d, 2);
  if (is.null(r.ci)) r.ci <- rep(r, 2);

  ### Sort confidence intervals so that the value leading to the
  ### most conservative outcome is the highest

  ### Lower values are more conservative
  d.ci <- sort(d.ci);

  ### Higher values are more conservative
  r.ci <- sort(r.ci, decreasing=TRUE);

  ### Values closer to .5 are more conservative
  if (cer.ci[2] - .5 == min(abs(cer.ci - .5))) cer.ci <- rev(cer.ci);

  nnc.est <- ufs::convert.d.to.nnc(d=d, cer=cer, r=r,
                                   eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);
  nnc.lb <- ufs::convert.d.to.nnc(d=d.ci[1], cer=cer.ci[1], r=r.ci[1],
                                  eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);
  nnc.ub <- ufs::convert.d.to.nnc(d=d.ci[2], cer=cer.ci[2], r=r.ci[2],
                                  eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);

  eer.est <- attr(nnc.est, 'eer');
  eer.ci <- c(attr(nnc.lb, 'eer'),
              attr(nnc.ub, 'eer'));

  nnc <- c(nnc.lb,
           nnc.ub);

  if (identical(nnc[1], nnc[2])) nnc <- nnc[1];

  res <- nnc;

  attr(res, 'nnc.raw') <- nnc;
  attr(res, 'eventDesirable') <- eventDesirable;
  if (!is.null(thresholdSensitivity)) {
    attr(res, 'sensitivityDf') <- sensitivityDf;
  }

  if (diff(range(cer.ci))) {
    attr(res, 'cer.ci') <- cer.ci;
  } else {
    attr(res, 'cer') <- cer.ci[1]
  }

  if (diff(range(eer.ci))) {
    attr(res, 'eer.ci') <- eer.ci;
  } else {
    attr(res, 'eer') <- eer.ci[1]
  }

  if (diff(range(r.ci))) {
    attr(res, 'r.ci') <- r.ci;
  } else {
    attr(res, 'r') <- r.ci[1]
  }

  if (diff(range(d.ci))) {
    attr(res, 'd.ci') <- d.ci;
  } else {
    attr(res, 'd') <- d.ci[1]
  }

  ### Compute confidence intervals

  if (!is.null(n)) {
    if (length(n) > 2) {
      stop("For n, please provide either a total sample size, or a vector with respectively the control and experimental sample sizes.");
    } else if (length(n) == 1) {
      n <- c(trunc(n/2),ceiling(n/2));
    }
    dataMatrix <- matrix(c(eer.est * n[1], (1-eer.est) * n[1],
                           cer * n[2], (1-cer) * n[2]),
                         byrow=TRUE, ncol=2);
    epiResult <-
      from_epiR_epi.2by2(dat = dataMatrix,
                         method = "cohort.count",
                         conf.level = conf.level,
                         units = 1);

    ### Lifted from https://cran.r-project.org/web/packages/RcmdrPlugin.EBM/
    #.ARR.est <- 0-epiResult$rval$AR$est
    #.ARR1 <- 0-epiResult$rval$AR$lower
    #.ARR2 <- 0-epiResult$rval$AR$upper
    .ARR.est <- epiResult$res$ARisk.conf$est
    .ARR1 <- epiResult$res$ARisk.conf$lower
    .ARR2 <- epiResult$res$ARisk.conf$upper
    .ARR.lower <- min(.ARR1, .ARR2)
    .ARR.upper <- max(.ARR1, .ARR2)
    .NNT.est <- 1/.ARR.est
    .NNT1 <- 1/.ARR.lower
    .NNT2 <- 1/.ARR.upper
    .NNT.lower <- min(.NNT1, .NNT2)
    .NNT.upper <- max(.NNT1, .NNT2)
    if (.ARR.lower < 0) {
      .NNT.lower <- .NNT.upper
      .NNT.upper <- 1/0
    }

    ### Store CI
    attr(res, 'NNC.ci') <- c(.NNT.lower, .NNT.upper);

  }

  if (plot) {
    if (is.null(d)) {
      d <- mean(d.ci);
      if (!silent)
        ufs::cat0("Warning: no point estimate for Cohen's d supplied, so using the simple mean ",
                  "of the lower and upper confidence interval bounds (", round(d, 2), ") for the plot!\n");
    }
    if (is.null(cer)) {
      cer <- mean(cer.ci);
      if (!silent)
        ufs::cat0("Warning: no point estimate for the CER supplied, so using the simple mean ",
                  "of the lower and upper confidence interval bounds (", ufs::formatR(cer), ") for the plot!\n");
    }
    if (is.null(r)) {
      r <- mean(r.ci);
      if (!silent)
        ufs::cat0("Warning: no point estimate for the correlation supplied, so using the simple mean ",
                  "of the lower and upper confidence interval bounds (", ufs::formatR(r), ") for the plot!\n");
    }

    plot <- behaviorchange::ggNNC(behaviorchange::erDataSeq(er=cer,
                                                            mean=mean,
                                                            sd=sd,
                                                            eventIfHigher=eventIfHigher),
                                  plotTitle = c(paste0(numbersNeededTitle, " = "),
                                                ""),
                                  eventDesirable = eventDesirable,
                                  d=d, r=r);
    if (returnPlot) {
      attr(res, 'plot') <- plot;
    } else {
      grid::grid.newpage();
      grid::grid.draw(plot);
    }
  }

  attr(res, 'numbersNeededTitle') <- numbersNeededTitle;

  class(res) <- c('nnc', class(res));

  return(res);

}

#' @rdname nnc
#' @method print nnc
#' @export
print.nnc <- function(x, digits=2, ...) {
  if (!is.null(attr(x, 'plot'))) {
    grid::grid.newpage();
    grid::grid.draw(attr(x, 'plot'));
  }

  if (length(x) > 1) {
    nnc <- as.numeric(x);
    nncStatement <-
      paste0(attr(x, 'numbersNeededTitle'), ": ", ufs::formatCI(ceiling(nnc)),
             " (exact: ", ufs::formatCI(round(nnc, digits)), ")");
  } else {
    nnc <- as.numeric(x);
    nncStatement <-
      paste0(attr(x, 'numbersNeededTitle'), ": ", ceiling(nnc),
             " (exact: ", round(nnc, digits), ")");
  }

  if (is.null(attr(x, 'cer.ci'))) {
    cer <- attr(x, 'cer');
    cerStatement <- paste0("a Control Event Rate (CER) of ", ufs::formatR(cer));
  } else {
    cer <- ufs::formatCI(sort(attr(x, 'cer.ci')), noZero=TRUE);
    cerStatement <- paste0("a Control Event Rate (CER) with a confidence interval of ",
                           cer);
  }

  if (is.null(attr(x, 'eer.ci'))) {
    eer <- ufs::formatR(attr(x, 'eer'));
    eerStatement <- paste0(", an Experimental Event Rate (EER) of ", eer);
  } else {
    eer <- ufs::formatCI(sort(attr(x, 'eer.ci')), noZero=TRUE);
    eerStatement <- paste0(", an Experimental Event Rate (EER) with a confidence interval of ",
                           eer);
  }

  if (is.null(attr(x, 'd.ci'))) {
    d <- attr(x, 'd');
    dStatement <- paste0(" and a Cohen's d of ", round(d, digits));
  } else {
    d <- ufs::formatCI(sort(attr(round(x, digits), 'd.ci')));
    dStatement <- paste0(" and a Cohen's d with a confidence interval of ",
                         d);
  }

  if (is.null(attr(x, 'r.ci'))) {
    r <- attr(x, 'r');
    if (r < 1) {
      rStatement <- paste0(", and assuming a correlation of ", ufs::formatR(r),
                           " between the dependent measure and behavior");
    } else {
      rStatement <- "";
    }
  } else {
    r <- ufs::formatCI(sort(attr(x, 'r.ci')), noZero=TRUE);
    rStatement <- paste0(", and assuming a correlation with a confidence interval of ",
                         r,
                         " between the dependent measure and behavior");
  }

  ufs::cat0("\n",
            nncStatement, "\n\n",
            "(Based on ", cerStatement,
            eerStatement, dStatement, rStatement, ".)\n");

  if (!is.null(attr(x, 'sensitivityDf'))) {
    cat("\nSensitivity analysis to easily compare the effect of choosing ",
        "different thresholds:\n\n");
    print(attr(x, 'sensitivityDf'), digits=digits);
  }

}

###############################################################################
###############################################################################
### SAS script for computing confidence intervals (see Bender, 2001)
###############################################################################
###############################################################################

# ***********************************************************************
#   *               CALCULATION of CONFIDENCE INTERVALS FOR               *
#   *                    RISK DIFFERENCES, NNT and PIN                    *
#   *      BASED ON WILSON SCORE METHOD WITHOUT CONTINUITY CORRECTION     *
#   *  REFERENCES: NEWCOMBE (1998), STATISTICS IN MEDICINE 17, 873-890    *
#   *              BENDER (2001), CONTROLLED CLINICAL TRIALS 22, 102-110  *
#   *              HELLER & DOBSON (2000), BMJ 321, 950-952               *
#   ***********************************************************************;
#
# *------------------------------------------------------------------*
#   |                           QUESTIONS ?                            |
#   |                                                                  |
#   | --->  Prof. Dr. Ralf BENDER  Phone:  +49 221 35685-451           |
#   |                              Fax:    +49 221 35685-891           |
#   |                              E-Mail: Ralf@rbsd.de                |
#   *------------------------------------------------------------------*;
# *------------------------------------------------------------------*
#   !!                         NOTE:                                  !!
#   !!   You have to enter the actual results of the two groups       !!
#   !!   at the end of the program (see EXAMPLE below).               !!
#   !!   e1 is the number of adverse events in the high risk group    !!
#   !!      (e.g. standard treatment or exposure)                     !!
#   !!   n1 is the sample size of the high risk group                 !!
#   !!   e2 is the number of adverse events in the low risk group     !!
#   !!      (e.g. new treatment or no exposure)                       !!
#   !!   n2 is the sample size of the low risk group                  !!
#   *------------------------------------------------------------------*;
#
# options linesize=105;
#
# %MACRO PIN (E1,N1,E2,N2);%*MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
# proc iml;
# e1 = &E1;             /* Number of events in high-risk group A */
#   n1 = &N1;             /* Sample size of high risk group A      */
#   e2 = &E2;             /* Number of events in low-risk group B  */
#   n2 = &N2;             /* Sample size of low risk group B       */
#   *--------------------------*Descriptive*-------------------------------*;
# start Descript;
# e  = e1+e2;     n  = n1+n2;
# r1 = e1/n1;     r2 = e2/n2;
# r  = e/n;
# RD = r1-r2;      /* Usual risk difference      */
#   PRD = r-r2;      /* Population risk difference */
#   NNT   = 1/RD;
# PREV1 = n1/n;
# PIN   = 1/PRD;
# print "Events and sample sizes of the groups:     " e n " " e1 n1 e2 n2;
# print "Risks of the groups:                       " r r1 r2;
# print "Usual risk difference (r1-r2) and NNT:     " RD NNT;
# print "Population risk difference (r-r2) and PIN: " PRD PIN,,;
# finish;
# *------------------------*Standard Wald Method*----------------------------*;
# start Wald;
# print "Standard Wald method  (frequently inadequate)";
# z95 = probit(1-(0.05/2));   z99 = probit(1-(0.01/2));
# RD_L95 = RD - z95*SQRT((e1*(n1-e1)/n1**3)+(e2*(n2-e2)/n2**3));
# RD_U95 = RD + z95*SQRT((e1*(n1-e1)/n1**3)+(e2*(n2-e2)/n2**3));
# RD_L99 = RD - z99*SQRT((e1*(n1-e1)/n1**3)+(e2*(n2-e2)/n2**3));
# RD_U99 = RD + z99*SQRT((e1*(n1-e1)/n1**3)+(e2*(n2-e2)/n2**3));
# PRD_L95 = PREV1*RD_L95;    PRD_U95 = PREV1*RD_U95;
# PRD_L99 = PREV1*RD_L99;    PRD_U99 = PREV1*RD_U99;
# NNT_L95 = 1/RD_U95;    NNT_U95 = 1/RD_L95;
# NNT_L99 = 1/RD_U99;    NNT_U99 = 1/RD_L99;
# PIN_L95 = 1/PRD_U95;    PIN_U95 = 1/PRD_L95;
# PIN_L99 = 1/PRD_U99;    PIN_U99 = 1/PRD_L99;
# print "95% confidence intervals for RD and NNT:  " RD_L95 RD_U95   "  " NNT_L95 NNT_U95;
# print "95% confidence intervals for PRD and PIN: " PRD_L95 PRD_U95 "  " PIN_L95 PIN_U95,,;
# print "99% confidence intervals for RD and NNT:  " RD_L99 RD_U99   "  " NNT_L99 NNT_U99;
# print "99% confidence intervals for PRD and PIN: " PRD_L99 PRD_U99 "  " PIN_L99 PIN_U99,,,;
# finish;
# *-----------------------*Wilson Score Method*---------------------------*;
# start Wilson;
# print "Confidence intervals based upon Wilson Scores";
# z95 = probit(1-(0.05/2));   z99 = probit(1-(0.01/2));
# l195 = (2*e1+z95**2) / (2*(n1+z95**2)) -
#   SQRT(((2*e1+z95**2)/(2*(n1+z95**2)))**2-e1**2/(n1**2+n1*z95**2));
# u195 = (2*e1+z95**2) / (2*(n1+z95**2)) +
#   SQRT(((2*e1+z95**2)/(2*(n1+z95**2)))**2-e1**2/(n1**2+n1*z95**2));
# l295 = (2*e2+z95**2) / (2*(n2+z95**2)) -
#   SQRT(((2*e2+z95**2)/(2*(n2+z95**2)))**2-e2**2/(n2**2+n2*z95**2));
# u295 = (2*e2+z95**2) / (2*(n2+z95**2)) +
#   SQRT(((2*e2+z95**2)/(2*(n2+z95**2)))**2-e2**2/(n2**2+n2*z95**2));
# l199 = (2*e1+z99**2) / (2*(n1+z99**2)) -
#   SQRT(((2*e1+z99**2)/(2*(n1+z99**2)))**2-e1**2/(n1**2+n1*z99**2));
# u199 = (2*e1+z99**2) / (2*(n1+z99**2)) +
#   SQRT(((2*e1+z99**2)/(2*(n1+z99**2)))**2-e1**2/(n1**2+n1*z99**2));
# l299 = (2*e2+z99**2) / (2*(n2+z99**2)) -
#   SQRT(((2*e2+z99**2)/(2*(n2+z99**2)))**2-e2**2/(n2**2+n2*z99**2));
# u299 = (2*e2+z99**2) / (2*(n2+z99**2)) +
#   SQRT(((2*e2+z99**2)/(2*(n2+z99**2)))**2-e2**2/(n2**2+n2*z99**2));
# delta95 = SQRT((e1/n1-l195)**2+(u295-e2/n2)**2);
# epsil95 = SQRT((u195-e1/n1)**2+(e2/n2-l295)**2);
# delta99 = SQRT((e1/n1-l199)**2+(u299-e2/n2)**2);
# epsil99 = SQRT((u199-e1/n1)**2+(e2/n2-l299)**2);
# RD_L95 = RD - delta95;
# RD_U95 = RD + epsil95;
# RD_L99 = RD - delta99;
# RD_U99 = RD + epsil99;
# PRD_L95 = PREV1*RD_L95;    PRD_U95 = PREV1*RD_U95;
# PRD_L99 = PREV1*RD_L99;    PRD_U99 = PREV1*RD_U99;
# NNT_L95 = 1/RD_U95;     NNT_U95 = 1/RD_L95;
# NNT_L99 = 1/RD_U99;     NNT_U99 = 1/RD_L99;
# PIN_L95 = 1/PRD_U95;    PIN_U95 = 1/PRD_L95;
# PIN_L99 = 1/PRD_U99;    PIN_U99 = 1/PRD_L99;
# print "95% confidence intervals for RD and NNT:  " RD_L95 RD_U95   "  " NNT_L95 NNT_U95;
# print "95% confidence intervals for PRD and PIN: " PRD_L95 PRD_U95 "  " PIN_L95 PIN_U95,,;
# print "99% confidence intervals for RD and NNT:  " RD_L99 RD_U99   "  " NNT_L99 NNT_U99;
# print "99% confidence intervals for PRD and PIN: " PRD_L99 PRD_U99 "  " PIN_L99 PIN_U99,,;
# finish;
# run Descript;
# run Wald;
# run Wilson;
# quit;
# %MEND PIN;%*MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
#
# title 'EXAMPLE';
# %PIN (135,8251,735,102098);
