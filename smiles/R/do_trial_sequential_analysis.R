#' @title Trial sequential analysis.
#'
#' @author Enoch Kang
#'
#' @description
#' **DoTSA()** is a function for conducting trial sequential analysis.
#'
#' @param data     DATAFRAME consists of relevant information.
#' @param source   CHARACTER for labeling the included data sets.
#' @param time     NUMERIC values of time sequence.
#' @param n        INTEGER values of sample sizes.
#' @param es       NUMERIC values of effect sizes.
#' @param se       NUMERIC values of standard errors for the effect sizes.
#' @param r1       INTEGER values of observed events in group 1 in the included data.
#' @param m1       NUMERIC values of estimated means in group 1 in the included data.
#' @param sd1      NUMERIC values of standard deviations in group 1 in the
#'                 included data.
#' @param n1       INTEGER values of sample sizes in group 1 in the included data.
#' @param r2       INTEGER values of observed events in group 2 in the included data.
#' @param m2       NUMERIC values of estimated means in group 2 in the included data.
#' @param sd2      NUMERIC values of standard deviations in group 2 in the
#'                 included data.
#' @param n2       INTEGER values of sample sizes in group 2 in the included data.
#' @param group    CHARACTER for labeling two groups.
#' @param ref      NUMERIC values of 1 or 2 for indicating group 1 or 2 as reference.
#' @param prefer   CHARACTER of "small" and "large" for indicating which direction
#'                 is beneficial effect in statistic test.
#' @param measure  CHARACTER for indicating which statistic measure should be used.
#' @param model    CHARACTER of "random" and "fixed" for indicating whether
#'                 to use random-effects model or fixed-effect model.
#' @param method   CHARACTER for indicating which estimator should be used in
#'                 random-effects model. In addition to the default "DL" method,
#'                 the current version also supports "REML" and "PM" methods for
#'                 calculating heterogeneity estimator.
#' @param pooling  CHARACTER for indicating which method has to be used for pooling
#'                 binary data. Besides, current version also supports "MH" and
#'                 "Peto" for binary data pooling.
#' @param trnsfrm  CHARACTER for indicating which method for transforming pooled
#'                 proportion. Current version supports "none", "logit", "log",
#'                 "arcsine", and "DAT" for the transformation.
#' @param poolProp CHARACTER for indicating which method has to be used for pooling
#'                 proportion. Current version supports "IV" and "GLMM" for the
#'                 data pooling.
#' @param alpha    NUMERIC value between 0 to 1 for indicating the assumed type I
#'                 error.
#' @param beta     NUMERIC value between 0 to 1 for indicating the assumed type II
#'                 error.
#' @param PES      NUMERIC value for indicating the presumed meaningful effect size.
#' @param RRR      NUMERIC value between 0 and 1 for indicating the presumed relative
#'                 risk reduction. This parameter only works for dichotomous outcome
#'                 by replacing parameter `PES`.
#' @param PV       NUMERIC value for indicating the presumed variance of the
#'                 meaningful effect size. Current version allows a numeric value,
#'                 "post-hoc", and "PES" based on different considerations.
#' @param adjust   CHARACTER for indicating how to adjust optimal information size.
#'                 Current version consists of "none", "D2", "I2", "CHL", "CHM", and
#'                 "CHH" for the adjustment.
#' @param plot     LOGIC value for indicating whether to illustrate alpha-spending
#'                 monitoring plot.
#' @param id       LOGIC value for indicating whether to label each data source.
#' @param invert   LOGIC value for indicating whether to invert plot.
#' @param smooth   LOGIC value for indicating whether to smooth error boundaries.
#' @param SAP      LOGIC value for indicating whether to show sequential-adjusted
#'                 power.
#' @param BSB      LOGIC value for indicating whether to illustrate beta-spending
#'                 boundaries.
#'
#'
#' @details
#' 1. Basic information for the function **DoTSA()**:
#' **DoTSA()** supports sequential analysis of aggregate data synthesis based on
#' head-to-head comparison using either binary or continuous data in each group.
#' Minimum information for the function **DoTSA()** encompasses a data set of
#' study-level data, and information for analysis settings in terms of time
#' sequence, presumed meaningful effect size, and presumed variance of the
#' meaningful effect size. Operative points of using function **DoTSA()** are listed
#' below:
#'
#' 1.1. Parameter `data` should be used for assigning a data set.
#' 1.2. Study-level data have to be assigned according to outcome type:
#' 1.2.1. **For dichotomous outcome**: Parameter `n1` and `n2` should be defined
#' with parameter `r1` and `r2`.
#' 1.2.2. **For continuous outcome**: parameter `n1` and `n2` should be defined
#' with parameter `m1`, `sd1`, `m2`, `sd2`.
#' 1.3. Parameter `source`, `time`, `PES`, and `PV` are required for conducting
#' sequential analysis. Other parameters are auxiliary.
#'
#' 2. Default in the function **DoTSA()**
#' Certain defaults have been elucidated in the introductory section about the
#' parameters, but some of them need to be elaborated upon due to their complexity.
#'
#' 2.1. Default on the parameter `measure` is `"ES"` that automatically uses risk
#' ratio ("RR") for binary outcome and mean difference ("MD") for continuous
#' outcome respectively. Argument `"OR"` and `"SMD"` can be used for the parameter
#' `measure` when original analysis pools data based on odds ratio or standardized
#' mean difference.
#'
#' 2.2. Default on the parameter `method` is `"DL"` for applying DerSimonian-Laird
#' heterogeneity estimator in the original pooled analysis. Other eligible arguments
#' for the parameter are `"REML"` for restricted maximum-likelihood estimator,
#' `"PM"` for Paule-Mandel estimator, `"ML"` for maximum-likelihood estimator,
#' `"HS"` for Hunter-Schmidt estimator, `"SJ"` for Sidik-Jonkman estimator,
#' `"HE"` for Hedges estimator, and `"EB"` for empirical Bayes estimator.
#'
#' 2.3. Default on the parameter `pooling` is `"IV"` for applying inverse variance
#' weighting method. Other commonly-used and eligible arguments for the parameter
#' are `"MH"` for Mantel-Haenszel method and `"Peto"` for pooling data using Peto
#' method. The arguments `"MH"` and `"Peto"` are exclusively available for binary
#' outcomes, while the argument `"IV"` will be automatically applied in the case
#' of continuous outcomes.
#'
#' 2.4. Default on the parameter `adjust` is `"D2"` for adjusting required
#' information size (RIS) based on diversity (D-squared statistics). Other eligible
#' arguments for the parameter are `"None"` for the RIS without adjustment, `"I2"`
#' for adjusted RIS based on I-squared statistics, `"CHL"` for adjusted RIS based
#' on low heterogeneity by multiplying 1.33, `"CHM"` for adjusted RIS by multiplying
#' 2 due to moderate heterogeneity, and `"CHL"` for adjusted RIS by multiplying
#' 4 due to high heterogeneity.
#'
#'
#' @return
#' **DoTSA()** returns a summary on the result of sequential analysis, and can be
#' stored as an object in `DoTSA` class. Explanations of returned information are
#' listed as follows:
#'
#' \item{studies}{Numbers of studies included in the sequential analysis.}
#' \item{AIS}{Acquired information size refers to the total sample size in the
#'       sequential analysis.}
#' \item{alpha}{A numeric value of type I error for the sequential analysis.}
#' \item{beta}{A numeric value of type II error for the sequential analysis.}
#' \item{PES}{A numeric value of presumed meaningful effect size for the
#'       sequential analysis.}
#' \item{RRR}{A numeric value of relative risk reduction.}
#' \item{variance}{A numeric value of presumed variance of the meaningful effect
#'       size for the sequential analysis.}
#' \item{diversity}{A numeric value to show diversity in the pooled analysis.}
#' \item{AF}{A numeric value of adjustment factor.}
#' \item{RIS.org}{A numeric value for required information size without adjustment.}
#' \item{RIS.adj}{A numeric value for adjusted required information size.}
#' \item{frctn}{A vector of fraction of each study included in the sequential
#'       analysis.}
#' \item{weight}{A vector of weight of each study included in the sequential
#'       analysis.}
#' \item{es.cum}{A vector of cumulative effect size in the sequential analysis.}
#' \item{se.cum}{A vector of standard error for the cumulative effect size in the
#'       sequential analysis.}
#' \item{zval.cum}{A vector of cumulative z-value in the sequential analysis.}
#' \item{asb}{A data frame of alpha-spending values for each study.}
#' \item{aslb}{A numeric value for lower alpha-spending boundary.}
#' \item{asub}{A numeric value for upper alpha-spending boundary.}
#'
#'
#' @references
#' Jennison, C., & Turnbull, B. W. (2005). Meta-analyses and adaptive group
#' sequential designs in the clinical development process.
#' **Journal of biopharmaceutical statistics**, *15(4)*, 537–558.
#' https://doi.org/10.1081/BIP-200062273.
#'
#' Wetterslev, J., Jakobsen, J. C., & Gluud, C. (2017). Trial sequential analysis
#' in systematic reviews with meta-analysis. **BMC medical research methodology**,
#' *17(1)*, 1-18.
#'
#' NCSS Statistical Software (2023). **Group-sequential analysis for two proportions**.
#' In *PASS Documentation*. Available online:
#' https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
#'
#'
#' @seealso \code{\link{PlotCoRNET}}
#'
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993bin")
#'
#' # 2. Perform trial sequential analysis
#' DoTSA(Fleiss1993bin, study, year,
#'       r1 = d.asp, n1 = n.asp,
#'       r2 = d.plac, n2 = n.plac,
#'       measure = "RR", RRR = 0.2,
#'       group = c("Aspirin", "Control"))
#'
#' ## End(Not run)
#'
#' @export DoTSA



DoTSA <- function(data     = NULL,
                  source   = NULL,
                  time     = NULL,
                  n        = NULL,
                  es       = NULL,
                  se       = NULL,
                  r1       = NULL,
                  m1       = NULL,
                  sd1      = NULL,
                  n1       = NULL,
                  r2       = NULL,
                  m2       = NULL,
                  sd2      = NULL,
                  n2       = NULL,
                  group    = c("Group 1", "Group 2"),
                  ref      = 2,
                  prefer   = "small",
                  measure  = "ES",
                  model    = "random",
                  method   = "DL",
                  pooling  = "IV",
                  trnsfrm  = "logit",
                  poolProp = "IV",
                  alpha    = 0.05,
                  beta     = 0.2,
                  PES      = NULL,
                  RRR      = NULL,
                  PV       = "post-hoc",
                  adjust   = "D2",
                  plot     = FALSE,
                  id       = FALSE,
                  invert   = FALSE,
                  smooth   = FALSE,
                  SAP      = FALSE,
                  BSB      = FALSE) {

  # 01. CHECK arguments -----
  lgcInData   <- ifelse(is.null(data), FALSE, TRUE)
  lgcInSource <- ifelse(is.null(substitute(source)), FALSE, TRUE)
  lgcInN      <- ifelse(is.null(substitute(n)), FALSE, TRUE)
  lgcInES     <- ifelse(is.null(substitute(es)), FALSE, TRUE)
  lgcInSE     <- ifelse(is.null(substitute(se)), FALSE, TRUE)
  lgcInTime   <- ifelse(is.null(substitute(time)), FALSE, TRUE)
  lgcInR1     <- ifelse(is.null(substitute(r1)), FALSE, TRUE)
  lgcInM1     <- ifelse(is.null(substitute(m1)), FALSE, TRUE)
  lgcInSD1    <- ifelse(is.null(substitute(sd1)), FALSE, TRUE)
  lgcInN1     <- ifelse(is.null(substitute(n1)), FALSE, TRUE)
  lgcInR2     <- ifelse(is.null(substitute(r2)), FALSE, TRUE)
  lgcInM2     <- ifelse(is.null(substitute(m2)), FALSE, TRUE)
  lgcInSD2    <- ifelse(is.null(substitute(sd2)), FALSE, TRUE)
  lgcInN2     <- ifelse(is.null(substitute(n2)), FALSE, TRUE)
  lgcInPES    <- ifelse(is.null(substitute(PES)), FALSE, TRUE)
  lgcInRRR    <- ifelse(is.null(substitute(RRR)), FALSE, TRUE)
  lgcInPV     <- ifelse(is.null(substitute(PV)), FALSE, TRUE)

  lgcReq1     <- ifelse(lgcInData == TRUE, TRUE, FALSE)
  lgcReq2     <- ifelse(FALSE %in% c(lgcInN, lgcInES, lgcInSE), FALSE, TRUE)
  lgcReq3     <- ifelse(FALSE %in% c(lgcInR1, lgcInN1, lgcInR2, lgcInN2), FALSE, TRUE)
  lgcReq4     <- ifelse(FALSE %in% c(lgcInM1, lgcInSD1, lgcInN1, lgcInM2, lgcInSD2, lgcInN2), FALSE, TRUE)
  lgcReq5     <- ifelse(FALSE %in% c(lgcInSource, lgcInTime, lgcInPES, lgcInPV), FALSE, TRUE)
  lgcReq6     <- ifelse(FALSE %in% c(lgcInSource, lgcInTime, lgcInRRR, lgcInPV), FALSE, TRUE)

  lgcStop1     <- ifelse(lgcReq1 == TRUE, FALSE, TRUE)
  infoLgcStop1 <- ifelse(lgcStop1 == TRUE,
                         'Parameter "data" should be used for assigning a data set.',
                         "")
  lgcStop2     <- ifelse(TRUE %in% c(lgcReq2, lgcReq3, lgcReq4), FALSE, TRUE)
  infoLgcStop2 <- ifelse(lgcStop2 == TRUE, 'Parameter "n", "es", and "se" should be defined for the analysis based on study-level data.
                        Parameter "n1" and "n2" should be defined with "r1" and "r2" for dichotomous outcome based on arm-level data.
                        Or parameter "n1" and "n2" should be defined with "m1", "sd1", "m2", "sd2" for continuous outcome based on arm-level data.',
                         "")
  lgcStop3     <- ifelse(lgcReq5 == TRUE,
                         FALSE,
                         ifelse(lgcReq6 == TRUE, FALSE, TRUE))
  infoLgcStop3 <- ifelse(lgcStop3 == FALSE,
                         'Parameter "source", "time", "PES", and "PV" are required',
                         "")

  if (lgcStop1 | lgcStop2 | lgcStop3)
    stop(paste(ifelse(lgcStop1, paste(infoLgcStop1, "\n", "")),
               ifelse(lgcStop2, paste(infoLgcStop2, "\n", "")),
               ifelse(lgcStop3, paste(infoLgcStop3, "\n", "")),
               sep = "")
    )

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))
  infoLgcWarning <- getOption("warn")
  #options(warn = -1)
  on.exit(options(warn = infoLgcWarning))

  # 02. DEFINE core data -----
  suppressWarnings({

    dataIn <- data

    source <- deparse(substitute(source))
    time   <- deparse(substitute(time))
    colnames(dataIn)[which(colnames(dataIn) == source)] <- "source"
    colnames(dataIn)[which(colnames(dataIn) == time)]   <- "time"

    if (lgcReq2) {
      n   <- deparse(substitute(n))
      es  <- deparse(substitute(es))
      se  <- deparse(substitute(se))
      colnames(dataIn)[which(colnames(dataIn) == n)]  <- "n"
      colnames(dataIn)[which(colnames(dataIn) == es)] <- "es"
      colnames(dataIn)[which(colnames(dataIn) == se)] <- "se"
    }

    if (lgcReq3) {
      r1  <- deparse(substitute(r1))
      r2  <- deparse(substitute(r2))
      colnames(dataIn)[which(colnames(dataIn) == r1)] <- "r1"
      colnames(dataIn)[which(colnames(dataIn) == r2)] <- "r2"
      if (measure == "ES") {
        measure <- "RR"
      }
    }

    if (lgcReq4) {
      m1  <- deparse(substitute(m1))
      sd1 <- deparse(substitute(sd1))
      m2  <- deparse(substitute(m2))
      sd2 <- deparse(substitute(sd2))
      colnames(dataIn)[which(colnames(dataIn) == m1)]  <- "m1"
      colnames(dataIn)[which(colnames(dataIn) == sd1)] <- "sd1"
      colnames(dataIn)[which(colnames(dataIn) == m2)]  <- "m2"
      colnames(dataIn)[which(colnames(dataIn) == sd2)] <- "sd2"
      if (measure == "ES") {
        measure <- "MD"
      }
    }

    if (TRUE %in% c(lgcReq3, lgcReq4)) {
      n1  <- deparse(substitute(n1))
      n2  <- deparse(substitute(n2))
      colnames(dataIn)[which(colnames(dataIn) == n1)] <- "n1"
      colnames(dataIn)[which(colnames(dataIn) == n2)] <- "n2"
      dataIn$n <- dataIn$n1 + dataIn$n2
    }

    dataIn <- dataIn[order(dataIn$time), ]

    infoGroup    <- group
    infoRef      <- ref
    infoPrefer   <- prefer
    infoMeasure  <- measure
    infoModel    <- model
    infoMethod   <- ifelse(infoModel == "random", method, "DL")
    infoPooling  <- ifelse(base::isFALSE(pooling %in% c("IV", "MH", "Peto")),
                           ifelse(base::isFALSE(infoMeasure %in% c("MD", "SMD")),
                                  "MH", "Inverse"),
                           ifelse(base::isFALSE(infoMeasure %in% c("MD", "SMD")),
                                  ifelse(pooling == "IV",
                                         "Inverse",
                                         pooling),
                                  "Inverse")
    )
    infoTrnsfrm  <- ifelse(base::isFALSE(trnsfrm %in% c("none", "logit", "log", "arcsine", "DAT")),
                           "PRAW",
                           ifelse(trnsfrm == "none",
                                  "PRAW",
                                  ifelse(trnsfrm == "logit",
                                         "PLOGIT",
                                         ifelse(trnsfrm == "log",
                                                "PLN",
                                                ifelse(trnsfrm == "arcsine",
                                                       "PAS",
                                                       "PRAW"))))
    )
    infoPoolProp <- ifelse(trnsfrm != "logit",
                           "Inverse",
                           ifelse(base::isFALSE(poolProp %in% c("IV", "GLMM")),
                                  "Inverse",
                                  ifelse(poolProp == "IV",
                                         "Inverse",
                                         "GLMM"))
    )
    infoAlpha    <- alpha
    infoBeta     <- beta
    infoPES      <- PES
    infoInRRR    <- ifelse(is.null(RRR), NA, RRR)
    infoPV       <- PV
    infoAdjust   <- adjust
    infoPlot     <- plot
    infoID       <- id
    infoInvert   <- invert
    infoSmooth   <- smooth
    infoSAP      <- SAP
    infoBSB      <- BSB

    if (infoRef == 1) {
      infoGroup[c(1, 2)] <- infoGroup[c(2, 1)]
      infoPrefer <- ifelse(infoPrefer == "small", "large", "small")

      if (lgcReq3) {
        colnames(dataIn)[which(colnames(dataIn) == "r1")] <- "rGroup1"
        colnames(dataIn)[which(colnames(dataIn) == "r2")] <- "rGroup2"
        colnames(dataIn)[which(colnames(dataIn) == "rGroup1")] <- "r2"
        colnames(dataIn)[which(colnames(dataIn) == "rGroup2")] <- "r1"
      }

      if (lgcReq4) {
        colnames(dataIn)[which(colnames(dataIn) == "m1")]  <- "mGroup1"
        colnames(dataIn)[which(colnames(dataIn) == "sd1")] <- "sdGroup1"
        colnames(dataIn)[which(colnames(dataIn) == "m2")]  <- "mGroup2"
        colnames(dataIn)[which(colnames(dataIn) == "sd2")] <- "sdGroup2"
        colnames(dataIn)[which(colnames(dataIn) == "mGroup1")]  <- "m2"
        colnames(dataIn)[which(colnames(dataIn) == "sdGroup1")] <- "sd2"
        colnames(dataIn)[which(colnames(dataIn) == "mGroup2")]  <- "m1"
        colnames(dataIn)[which(colnames(dataIn) == "sdGroup2")] <- "sd1"
      }

      colnames(dataIn)[which(colnames(dataIn) == "n1")]  <- "nGroup1"
      colnames(dataIn)[which(colnames(dataIn) == "n2")]  <- "nGroup2"
      colnames(dataIn)[which(colnames(dataIn) == "nGroup1")]  <- "n2"
      colnames(dataIn)[which(colnames(dataIn) == "nGroup2")]  <- "n1"

    }




    # 03. PREPARE data before sequential analysis -----
    if (lgcReq2) {
      outMA <- meta::metagen(data = dataIn,
                             TE = es,
                             seTE = se,
                             studlab = source,
                             method.tau = infoMethod)
    }

    if (lgcReq3) {
      outMA <- meta::metabin(data = dataIn,
                             event.e = r1,
                             n.e = n1,
                             event.c = r2,
                             n.c = n2,
                             sm = infoMeasure,
                             studlab = source,
                             method = infoPooling,
                             method.tau = infoMethod)
    }

    if (lgcReq4) {
      outMA <- meta::metacont(data = dataIn,
                              mean.e = m1,
                              sd.e = sd1,
                              n.e = n1,
                              mean.c = m2,
                              sd.c = sd2,
                              n.c = n2,
                              sm = infoMeasure,
                              studlab = source,
                              method.tau = infoMethod)
    }

    outCMA <- meta::metacum(outMA, pooled = infoModel)

    infoNumStud  <- outCMA$k.study
    infoCases    <- sum(dataIn$n)

    if (lgcReq3) {
      if (infoModel == "random") {
        infoProp1 <- meta::metaprop(event = r1,
                                    n = n1,
                                    method = infoPoolProp,
                                    sm = infoTrnsfrm,
                                    data = dataIn)$TE.random
        infoProp2 <- meta::metaprop(event = r2,
                                    n = n2,
                                    method = infoPoolProp,
                                    sm = infoTrnsfrm,
                                    data = dataIn)$TE.random

        if (trnsfrm == "logit") {
          infoProp1 <- boot::inv.logit(infoProp1)
          infoProp2 <- boot::inv.logit(infoProp2)
        }
        if (trnsfrm == "log") {
          infoProp1 <- exp(infoProp1)
          infoProp2 <- exp(infoProp2)
        }

        # Hajian-Tilaki K. Sample size estimation in epidemiologic studies. Caspian J Intern Med. 2011 Fall;2(4):289-98. PMID: 24551434; PMCID: PMC3895825.
        if (infoProp2 < 0.001) {
          if (infoProp2 < 0.000001) {
            infoProp2 <- 0.000001
          } else {
            infoProp1 <- (infoProp2 * exp(outMA$TE.random)) / (1 + infoProp2 * (exp(outMA$TE.random) - 1))
          }
        }

      } else {
        infoProp1 <- meta::metaprop(event = r1,
                                    n = n1,
                                    method = infoPoolProp,
                                    sm = infoTrnsfrm,
                                    data = dataIn)$TE.fixed
        infoProp2 <- meta::metaprop(event = r2,
                                    n = n2,
                                    method = infoPoolProp,
                                    sm = infoTrnsfrm,
                                    data = dataIn)$TE.fixed

        if (trnsfrm == "logit") {
          infoProp1 <- boot::inv.logit(infoProp1)
          infoProp2 <- boot::inv.logit(infoProp2)
        }
        if (trnsfrm == "log") {
          infoProp1 <- exp(infoProp1)
          infoProp2 <- exp(infoProp2)
        }

        if (infoProp2 < 0.001) {
          if (infoProp2 < 0.000001) {
            infoProp2 <- 0.000001
          } else {
            infoProp1 <- (infoProp2 * exp(outMA$TE.fixed)) / (1 + infoProp2 * (exp(outMA$TE.fixed) - 1))
          }
        }
      }

      # Effect calculation based on binary data
      if (is.numeric(infoInRRR)) {
        infoRRR          <- infoInRRR
        infoProp1ByInRRR <- infoProp2 - (infoProp2 * infoInRRR)
        infoESMAByInRRR  <- infoProp2 - infoProp1ByInRRR
        infoPES          <- infoESMAByInRRR
        #infoVarMAByInRRR <- (infoProp2 + abs(infoProp1ByInRRR)) / 2 * (1 - (infoProp2 + abs(infoProp1ByInRRR)) / 2)
        infoVarMAByInRRR <- (infoProp2 + infoProp1ByInRRR) / 2 * (1 - (infoProp2 + infoProp1ByInRRR) / 2)
      } else if (infoPES == "post-hoc") {
        #infoRRR   <- abs(infoProp2 - infoProp1) / infoProp1
        infoRRR            <- (infoProp2 - infoProp1) / infoProp2
        infoESMAByPostHoc  <- infoProp2 - infoProp1
        infoPES            <- infoESMAByPostHoc
        infoVarMAByPostHoc <- (infoProp2 + infoProp1) / 2 * (1 - (infoProp2 + infoProp1) / 2)
        infoProp1ByinfoPES <- infoVarMAByPostHoc
      } else {
        infoProp1ByinfoPES <- infoProp2 - infoPES
        infoRRR            <- (infoProp2 - infoProp1ByinfoPES) / infoProp2
        infoESMAByinfoPES  <- infoProp2 - infoProp1ByinfoPES
        #infoVarMAByPES     <- (infoProp2 + abs(infoProp1ByinfoPES)) / 2 * (1 - (infoProp2 + abs(infoProp1ByinfoPES)) / 2)
        infoVarMAByPES     <- (infoProp2 + infoProp1ByinfoPES) / 2 * (1 - (infoProp2 + infoProp1ByinfoPES) / 2)
      }

      # Variance calculation based on binary data
      if (infoPV == "post-hoc") {
        infoPV <- (infoProp2 + infoProp1) / 2 * (1 - (infoProp2 + infoProp1) / 2)
      } else if (is.numeric(infoInRRR)) {
        infoPV <- infoVarMAByInRRR
      } else {
        infoPV <- infoVarMAByPES
      }

      #infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
    } else if (base::isFALSE(lgcReq3)) {

      infoRRR   <- NA
      infoESMA  <- ifelse(infoModel == "random", outMA$TE.random, outMA$TE.fixed)
      infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
      #infoVarMA <- ifelse(infoModel == "random", 1 / sum(outMA$w.random), 1 / sum(outMA$w.fixed))
      #infoVarMA    <- ifelse(infoModel == "random", 1 / sum(1 / (outMA$seTE + outMA$tau2)), 1 / sum(1 / outMA$seTE))
      infoVarMA  <- (infoSEMA * sqrt(infoCases - 2))^2

      infoPES   <- ifelse(infoPES == "post-hoc", infoESMA, infoPES)
      infoPV    <- ifelse(infoPV == "post-hoc", infoVarMA, infoPV)
    }

    infoI2 <- outMA$I2

    # 04. DO sequential analysis -----
    # alpha spending boundary (NCSS 710 Group-Sequential Analysis for Two Proportions 710-17)
    # https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
    # 04.01 Generate observed cumulative information

    if (infoModel == "random") {
      dataIn$weight <- outMA$w.random / sum(outMA$w.random)
    } else {
      dataIn$weight <- outMA$w.fixed / sum(outMA$w.fixed)
    }

    dataIn$esCum  <- outCMA$TE[c(1:infoNumStud)]
    dataIn$seCum  <- outCMA$seTE[c(1:infoNumStud)]
    dataIn$zCum   <- outCMA$statistic[c(1:infoNumStud)]

    ## 04.02 Calculate diversity (D-square, D2)
    infoDivers    <- 1 - (sum((outMA$seTE^2 + outMA$tau2)^(-1)) / sum(outMA$seTE^(-2)))
    #infoDivers <- (outMA$seTE.random^2 - outMA$seTE.fixed^2) / outMA$seTE.random^2
    #infoDivers <- (((outMA$seTE.random * sqrt(infoCases - 2))^2) - ((outMA$seTE.fixed * sqrt(infoCases - 2))^2)) / ((outMA$seTE.random * sqrt(infoCases - 2))^2)
    #infoDivers <- 1 / sum(1 / (outMA$seTE)) / 1 / sum(1 / (outMA$seTE + outMA$tau2))
    #infoDivers <- (((outMA$seTE.random * sqrt(infoCases))^2) - ((outMA$seTE.fixed * sqrt(infoCases))^2)) / ((outMA$seTE.random * sqrt(infoCases))^2)
    #1-(((MA.RE$seTE.fixed*sqrt(sum(DATA.TSA$n)-2))^2)/((MA.RE$seTE.random*sqrt(sum(DATA.TSA$n)-2))^2))
    #1 - (sum(outMA$w.random) / sum(outMA$w.fixed))
    #sum(((outMA$w.fixed^-1)+MA.RE$tau2)^-1)/sum(MA.RE$w.fixed)

    ## 04.03 Determinate adjustement factor
    if (infoAdjust == "D2") {
      infoAF <- 1 / (1 - infoDivers)
      #infoAF <- (outMA$seTE.random * sqrt(infoCases - 2))^2 / (outMA$seTE.fixed * sqrt(infoCases - 2))^2
    }

    if (infoAdjust == "I2") {
      infoAF <- 1 / (1 - infoI2)
    }

    if (infoAdjust == "CHL") {
      infoAF <- 1.33
    }

    if (infoAdjust == "CHM") {
      infoAF <- 2
    }

    if (infoAdjust == "CHH") {
      infoAF <- 4
    }

    ## 04.04 Calculate required information size (RIS)
    #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * (infoSEMA * sqrt(infoCases - 2))^2 / infoPES^2
    #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
    #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(1 - infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
    #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
    # X infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * (infoSEMA * sqrt(infoCases))^2 / (infoPES)
    #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) / (infoPES^2)
    # X infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * infoSEMA / infoPES^2
    #infoRIS      <- (1 / (1 - infoDivers)) * 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
    #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * infoPV / (infoPES^2)

    infoRISOrg <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(1 - infoBeta)))^2) * infoPV / infoPES^2

    if (infoAdjust == "none") {
      infoRIS    <- infoRISOrg
    } else {
      infoRISAdj <- infoRISOrg * infoAF
      infoRIS    <- infoRISAdj
    }


    dataPlotSA <- as.data.frame(cbind(sample = c(ceiling(infoRIS/20):infoRIS),
                                      frctn  = c(ceiling(infoRIS/20):infoRIS) / infoRIS)
    )
    dataPlotSA$aslb <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotSA$frctn))) / 2); dataPlotSA$aslb <- ifelse(dataPlotSA$aslb == "-Inf", -10, dataPlotSA$aslb)
    dataPlotSA$asub <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotSA$frctn))) / 2); dataPlotSA$asub <- ifelse(dataPlotSA$asub == "Inf", 10, dataPlotSA$asub)
    dataPlotSA$bsub <-  (qnorm(pnorm(qnorm((infoBeta)) / dataPlotSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataPlotSA$bsub <- ifelse(dataPlotSA$bsub < 0, 0, dataPlotSA$bsub)
    dataPlotSA$bslb <- -(qnorm(pnorm(qnorm((infoBeta)) / dataPlotSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataPlotSA$bslb <- ifelse(dataPlotSA$bslb > 0, 0, dataPlotSA$bslb)
    dataPlotSA      <- dataPlotSA[dataPlotSA$aslb > -9, ]
    dataPlotSA      <- dataPlotSA[dataPlotSA$asub < 9, ]
    infoFrctnBSB    <- dataPlotSA[max(which(dataPlotSA$bsub == 0)), "frctn"]
    lgcDataPlotSA   <- TRUE

    dataPlotSA$asub <- ifelse(dataPlotSA$asub < 2, 2, dataPlotSA$asub)
    dataPlotSA$aslb <- ifelse(dataPlotSA$aslb > -2, -2, dataPlotSA$aslb)
    dataPlotSA$bsub <- ifelse(dataPlotSA$bsub < 0, 0, dataPlotSA$bsub)
    dataPlotSA$bslb <- ifelse(dataPlotSA$bslb > 0, 0, dataPlotSA$bslb)


    dataSA <- dataIn[, c("source", "time", "n", "weight", "esCum", "seCum", "zCum")]
    dataSA <- dataSA[order(dataSA$time), ]

    if (infoInvert) {
      dataSA$zCum <- dataSA$zCum * -1
      infoPrefer  <- ifelse(infoPrefer == "small", "large", "small")
    }

    dataSA$nCum  <- 0

    for (study.i in c(1:infoNumStud)) {
      if (study.i == 1) {
        dataSA[study.i, "nCum"] <- dataSA[study.i, "n"]
      } else {
        dataSA[study.i, "nCum"] <- dataSA[study.i, "n"] + dataSA[study.i - 1, "nCum"]
      }
    }

    dataSA$frctn <- dataSA$nCum / infoRIS
    dataSA$asub  <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataSA$frctn))) / 2); dataSA$asub <- ifelse(dataSA$asub == "Inf", 10, dataSA$asub)
    dataSA$aslb  <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataSA$frctn))) / 2); dataSA$aslb <- ifelse(dataSA$aslb == "-Inf", -10, dataSA$aslb)
    dataSA$bsub  <-  (qnorm(pnorm(qnorm((infoBeta)) / dataSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataSA$bsub <- ifelse(dataSA$bsub < 0, 0, dataSA$bsub)
    dataSA$bslb  <- -(qnorm(pnorm(qnorm((infoBeta)) / dataSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataSA$bslb <- ifelse(dataSA$bslb > 0, 0, dataSA$bslb)

    if (infoSAP == TRUE) {
      dataSA$pwrCum  <- 1 - pnorm(qnorm(1 - infoAlpha / 2) - dataSA$zCum) + pnorm(-qnorm(1 - infoAlpha / 2) - dataSA$zCum)
      #dataSA$pwrSqnt <- 1 - pnorm(qnorm(1 - infoAlpha / 2 * dataSA$frctn) - dataSA$zCum) + pnorm(-qnorm(1 - infoAlpha / 2 * dataSA$frctn) - dataSA$zCum)
      #dataSA$pwrSqnt <- 1 - pnorm(qnorm(1 - infoAlpha / 2 / sqrt(dataSA$frctn)) / 2 - -dataSA$zCum) + pnorm(-qnorm(1 - infoAlpha / 2 / sqrt(dataSA$frctn)) / 2 - -dataSA$zCum)
      #dataSA$pwrSqnt <- 1 - pnorm(qnorm(2 - 2 * pnorm(qnorm(1 - infoAlpha / 2) / sqrt(dataSA$frctn))) - dataSA$zCum) + pnorm(-qnorm(2 - 2 * pnorm(qnorm(1 - infoAlpha / 2) / sqrt(dataSA$frctn))) - dataSA$zCum)
      dataSA$pwrSqnt <- 1 - pnorm(dataSA$asub - dataSA$zCum) + pnorm(-dataSA$asub - dataSA$zCum)
      dataSA$pwrCum  <- ifelse(dataSA$pwrCum > 1, 1, dataSA$pwrCum)
      dataSA$pwrSqnt <- ifelse(dataSA$pwrSqnt > 1, 1, dataSA$pwrSqnt)
    } else {
      dataSA$pwrCum  <- NA
      dataSA$pwrSqnt <- NA
    }

    #dataSA$power <- 1 - pnorm(qnorm(1 - infoAlpha / 2) - dataSA$zCum) + pnorm(-qnorm(1 - infoAlpha/2) - dataSA$zCum); dataSA$power
    #dataSA$power<-1-pnorm(qnorm(1-alpha/2*dataSA$frctn)-dataSA$zCum)+pnorm(-qnorm(1-alpha/2*dataSA$frctn)-dataSA$zCum);dataSA$power
    #dataSA$power<-1-pnorm(qnorm(1-alpha/2)/sqrt(dataSA$frctn)/2-dataSA$zCum)+pnorm(-qnorm(1-alpha/2)/sqrt(dataSA$frctn)/2-dataSA$zCum);dataSA$power


    dataSA <- as.data.frame(dataSA)

    dataSA <- dataSA[order(dataSA$frctn), ]

    infoColorASB <- ifelse(dataSA$nCum > infoRIS,
                           rgb(1, 1, 1, 1),
                           "gray25")
    infoPosLabel <- ifelse(dataSA$zCum > 0, 4, 2)
    infoPwrObs   <- dataSA[nrow(dataSA), "pwrSqnt"]

    if (max(dataSA$frctn) < infoFrctnBSB) {
      dataSA[nrow(dataSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA, NA,
                                      round(infoRIS * infoFrctnBSB, 0), infoFrctnBSB,
                                      qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(infoFrctnBSB))) / 2),
                                      -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(infoFrctnBSB))) / 2),
                                      0, 0,
                                      (1 - infoBeta) * infoFrctnBSB,
                                      NA
                                      #,(1 - infoBeta) * infoFrctnBSB
      )
    }

    dataSA[nrow(dataSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA, NA,
                                    round(infoRIS, 0), 1,
                                    qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                    -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                    qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                    -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                    1 - infoBeta,
                                    NA
                                    #,1 - infoBeta
    )

    dataSA$asub <- ifelse(dataSA$asub < 2, 2, dataSA$asub)
    dataSA$aslb <- ifelse(dataSA$aslb > -2, -2, dataSA$aslb)



    ## 04.05 Adjusted MA -----
    lgcMAAdj <- abs(dataSA$zCum[infoNumStud]) < abs(dataSA$asub[infoNumStud])
    infoLgcMAAdj  <- ifelse(lgcMAAdj,
                            "Adjusted confidence interval is suggested to be performed.",
                            "Adjusted confidence interval is not necessary to be performed.")
    infoPValMAAdj <- pnorm(ifelse(dataSA$asub[infoNumStud] > 5.3,
                                  5.3,
                                  dataSA$asub[infoNumStud]))

    if (lgcMAAdj) {
      if (lgcReq2) {
        outAdjMA <- meta::metagen(data = dataIn,
                                  TE = es,
                                  seTE = se,
                                  studlab = source,
                                  method.tau = infoMethod,
                                  level.ma = infoPValMAAdj)
      }

      if (lgcReq3) {
        outAdjMA <- meta::metabin(data = dataIn,
                                  event.e = r1,
                                  n.e = n1,
                                  event.c = r2,
                                  n.c = n2,
                                  sm = infoMeasure,
                                  studlab = source,
                                  method.tau = infoMethod,
                                  method = infoPooling,
                                  level.ma = infoPValMAAdj)
      }

      if (lgcReq4) {
        outAdjMA <- meta::metacont(data = dataIn,
                                   mean.e = m1,
                                   sd.e = sd1,
                                   n.e = n1,
                                   mean.c = m2,
                                   sd.c = sd2,
                                   n.c = n2,
                                   sm = infoMeasure,
                                   studlab = source,
                                   method.tau = infoMethod,
                                   level.ma = infoPValMAAdj)
      }

      if (infoModel == "random") {
        infoLCIMAAdj <- outAdjMA$lower.random
        infoUCIMAAdj <- outAdjMA$upper.random
      } else {
        infoLCIMAAdj <- outAdjMA$lower.fixed
        infoUCIMAAdj <- outAdjMA$upper.fixed
      }
    }



    # 05. BUILD an DoTSA object -----
    lsDoTSA <- list(name     = "Sequential analysis",
                    group    = infoGroup,
                    prefer   = infoPrefer,
                    studies   = infoNumStud,
                    AIS       = infoCases,
                    alpha     = infoAlpha,
                    beta      = infoBeta,
                    measure   = infoMeasure,
                    pooling   = infoPooling,
                    model     = infoModel,
                    method    = infoMethod,
                    transform = trnsfrm,
                    pool.prop = infoPoolProp,
                    PES       = infoPES,
                    RRR       = ifelse(infoRRR < 0.001, "< 0.001", round(infoRRR, 3)),
                    variance  = infoPV,
                    diversity = infoDivers,
                    adjust    = infoAdjust,
                    AF        = ifelse(infoAdjust == "none",
                                       "Undefined",
                                       infoAF),
                    RIS.org   = ceiling(infoRISOrg),
                    RIS.adj   = ifelse(infoAdjust == "none",
                                       "No adjusted",
                                       ceiling(infoRISAdj))
    )
    class(lsDoTSA)    <- c("aides", "DoTSA")
    lsDoTSA$frctn      <- paste(round(dataSA$frctn[infoNumStud], 4) * 100,
                                "%",
                                sep = "")
    lsDoTSA$weight    <- paste(round(dataSA$weight[c(1:infoNumStud)], 4) * 100,
                               "%",
                               sep = "")
    lsDoTSA$es.cum    <- round(dataSA$esCum[c(1:infoNumStud)], 3)
    lsDoTSA$se.cum    <- round(dataSA$seCum[c(1:infoNumStud)], 3)
    lsDoTSA$zval.cum  <- round(dataSA$zCum[c(1:infoNumStud)], 3)
    lsDoTSA$asb       <- round(dataSA[c(1:infoNumStud), c("aslb", "asub")], 3)
    lsDoTSA$aslb      <- round(dataSA$aslb[infoNumStud], 3)
    lsDoTSA$asub      <- round(dataSA$asub[infoNumStud], 3)

    if (infoPlot == TRUE | infoSAP == TRUE) {
      lsDoTSA$group          <- infoGroup
      lsDoTSA$ref            <- infoRef
      lsDoTSA$color.ASB      <- infoColorASB
      lsDoTSA$position.label <- infoPosLabel
      if (infoSAP == TRUE) {
        lsDoTSA$data         <- dataSA
      } else {
        lsDoTSA$data         <- dataSA[, -which(colnames(dataSA) %in% c("pwrCum", "pwrSqnt"))]
      }
      lsDoTSA$data.bounds    <- dataPlotSA
      lsDoTSA$lsOutMA        <- outMA
    }



    # 06. RETURN summary of function `DoTSA()` -----
    #message(paste("\n"), fill = TRUE, sep = "")
    message(paste("Summary of sequential analysis (main information)\n",
                  " Acquired sample size: ",
                  infoCases,
                  "\n Required sample size",
                  ifelse(infoAdjust != "none",
                         " (heterogeneity adjusted): ",
                         " (without adjusted): "
                  ),
                  ifelse(infoAdjust == "none",
                         ceiling(infoRISOrg),
                         ceiling(infoRISAdj)
                  ),
                  "\n Cumulative z score: ",
                  round(dataSA$zCum[infoNumStud], 3),
                  "\n Alpha-spending boundary: ",
                  round(dataSA$asub[infoNumStud], 3),
                  " and ",
                  round(dataSA$aslb[infoNumStud], 3),
                  "\n ",
                  infoLgcMAAdj,
                  ifelse(infoSAP == TRUE,
                         paste("\n Observed power (sequential-adjusted): ",
                               round(infoPwrObs, 3),
                               sep = ""),
                         ""),
                  sep = ""),
            fill = TRUE, sep = "")

    if (lgcMAAdj) {
      message(paste("\n",
                    "Adjusted confidence interval based on type I error ",
                    ifelse(dataSA$asub[infoNumStud] > 5.3,
                           "< 0.000001",
                           1 - pnorm(dataSA$asub[infoNumStud])
                    ),
                    ": \n",
                    round(infoLCIMAAdj, 3),
                    " to ",
                    round(infoUCIMAAdj, 3),
                    sep = ""),
              fill = TRUE, sep = "")
    }

    message(paste("\n",
                  "Summary of sequential analysis (additional information)",
                  "\n 1. Assumed information",
                  "\n 1.1. Defined type I error: ",
                  infoAlpha,
                  "\n 1.2. Defined type II error: ",
                  infoBeta,
                  "\n 1.3. Defined power: ",
                  1 - infoBeta,
                  "\n 1.4. Presumed effect: ",
                  round(infoPES, 3),
                  ifelse(lgcReq3,
                         paste("\n      (risks in group 1 and 2 were ",
                               ifelse(is.numeric(infoInRRR),
                                      paste(round(infoProp1ByInRRR, 10) * 100,
                                            "% (expected) and ",
                                            sep = ""),
                                      ifelse(infoPES == "post-hoc",
                                             paste(round(infoProp1, 10) * 100,
                                                   "% and ",
                                                   sep = ""),
                                             paste(round(infoProp1ByinfoPES, 10) * 100,
                                                   "% (expected) and ",
                                                   sep = ""))
                               ),
                               round(infoProp2, 10) * 100,
                               "% respectively; ",
                               ifelse(PES == "post-hoc",
                                      paste(trnsfrm, " transformation with ",
                                            infoPoolProp,
                                            " method for pooling the proportion; ",
                                            sep = ""),
                                      ""
                               ),
                               "RRR ",
                               ifelse(infoRRR < 0.001, "< 0.001)",
                                      paste("= ", round(infoRRR, 3), ")",
                                            sep = "")),
                               sep = ""),
                         ""
                  ),
                  "\n 1.5. Presumed variance: ",
                  round(infoPV, 3),

                  "\n",
                  "\n 2. Meta-analysis",
                  "\n 2.1. Setting of the meta-analysis",
                  ifelse(infoMeasure %in% c("RR", "OR"),
                         paste("\n Data were pooled using ",
                               ifelse(infoPooling == "Inverse",
                                      "inverse variance",
                                      ifelse(infoPooling == "MH",
                                             "Mantel-Haensze",
                                             infoPooling
                                      )
                               ),
                               " approach in ",
                               ifelse(infoModel == "random",
                                      paste("random-effects model with ",
                                            infoMethod, " method.",
                                            sep = ""),
                                      "fixed-effect model."),
                               sep = ""),
                         paste("\n Data were pooled using inverse variance in ",
                               ifelse(infoModel == "random",
                                      paste("random-effects model with ",
                                            infoMethod, " method.",
                                            sep = ""),
                                      "fixed-effect model."),
                               sep = "")
                  ),

                  "\n 2.2. Result of the meta-analysis \n ",
                  paste(ifelse(infoMeasure %in% c("RR", "OR"),
                               paste("Log ", infoMeasure,
                                     sep = ""),
                               infoMeasure
                  ),
                  ": ",
                  ifelse(infoModel == "random",
                         round(outMA$TE.random, 3),
                         round(outMA$TE.fixed, 3)
                  ),
                  " (95% CI: ",
                  ifelse(infoModel == "random",
                         round(outMA$lower.random, 3),
                         round(outMA$lower.fixed, 3)
                  ),
                  " to ",
                  ifelse(infoModel == "random",
                         round(outMA$upper.random, 3),
                         round(outMA$upper.fixed, 3)
                  ),
                  ")",
                  sep = ""),

                  "\n ",
                  "\n 3. Adjustment factor \n ",
                  paste("The required information size is calculated ",
                        ifelse(infoAdjust == "none",
                               "without adjustment factor.",
                               paste("with adjustment factor based on ",
                                     ifelse(infoAdjust == "D2",
                                            "diversity (D-squared).",
                                            ifelse(infoAdjust == "I2",
                                                   "heterogeneity (I-squared).",
                                                   ifelse(infoAdjust == "CHL",
                                                          "low conceptual heterogeneity (around 25%).",
                                                          ifelse(infoAdjust == "CHM",
                                                                 "moderate conceptual heterogeneity (around 50%).",
                                                                 ifelse(infoAdjust == "CHH",
                                                                        "high conceptual heterogeneity (around 75%).",
                                                                        "?")
                                                          )
                                                   )
                                            )
                                     ),
                                     sep = "")
                        ),
                        " Relevant parameters are listed as follows.",
                        sep = ""),
                  "\n 3.1. Heterogeneity (I-squared): ",
                  paste(round(outMA$I2, 3) * 100, "%",
                        sep = ""),
                  "\n 3.2. Diversity (D-squared): ",
                  paste(round(infoDivers, 2) * 100, "%",
                        sep = ""),
                  "\n 3.3. Adjustement factor: ",
                  ifelse(infoAdjust == "none",
                         "Undefined",
                         round(infoAF, 3)),
                  sep = ""),
            fill = TRUE, sep = "")



    # 07. ILLUSTRATE proportion of alpha-spending monitoring plot -----

    if (infoPlot == TRUE) {

      plot(dataSA$frctn * 1.2, dataSA$asub,
           type = "l", frame = FALSE,
           xlim = c(0, max(dataSA$frctn) * 1.2),
           ylim = c(ceiling(min(dataSA$aslb)) * (-10) / ceiling(min(dataSA$aslb)),
                    ceiling(max(dataSA$asub)) * 10 / ceiling(max(dataSA$asub)) + 1),
           col = rgb(1, 1, 1, 0),
           xlab = "Sample size",
           yaxt = "n", xaxt = "n",
           #ylab=paste("Favors", Txs[1], "   (Z score)   Favors",Txs[2]),
           ylab = "",
           main = "Sequential analysis")
      mtext(paste("(Note: the meta-analysis was conducted in ",
                  ifelse(infoModel == "random",
                         paste("random-effects model based on ",
                               pooling, " with ", infoMethod,
                               " method)",
                               sep = ""),
                         paste("fixed-effect model based on ",
                               pooling,
                               " method)",
                               sep = "")
                  ),
                  sep = ""),
            side = 1, line = 4, cex = 0.6)

      axis(side = 1,
           at = c(0, 0.25, 0.5, 0.75, 1, max(dataSA$frctn) * 1.2),
           labels = c(0,
                      ceiling(max(dataSA$nCum) * 0.25),
                      ceiling(max(dataSA$nCum) * 0.5),
                      ceiling(max(dataSA$nCum) * 0.75),
                      ceiling(max(dataSA$nCum)),
                      ceiling(max(dataSA$nCum) * 1.2)),
           cex.axis = 1)
      axis(side = 2, at = c(seq(ceiling(min(dataSA$aslb)) * (-10) / ceiling(min(dataSA$aslb)),
                                ceiling(max(dataSA$asub)) * 10 / ceiling(max(dataSA$asub)), 2)),
           padj = 0, hadj = 1, las = 1)
      mtext("Cumulative\n z score", side = 3, line = 0, at = -infoRIS * 0.05)
      mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[2], infoGroup[1])),
            side = 2, line = 2, at = 5,
            cex = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)) #(1/sqrt(seq(11,100,by=1)))^2*10
      mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[1], infoGroup[2])),
            side = 2, line = 2, at = -5,
            cex = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)) #(1/sqrt(seq(11,100,by=1)))^2*10

      #lines(dataSA$nCum,
      #      dataSA$asub,
      #      lwd = 1, col = "darkred", lty = 1)
      if (infoSmooth) {
        lines(dataPlotSA$frctn,
              dataPlotSA$asub,
              lwd = 1, col = "darkred", lty = 1)
        lines(dataPlotSA$frctn,
              dataPlotSA$aslb,
              lwd = 1, col = "darkred", lty = 1)
      } else {
        lines(dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "asub"] < 9), ]$frctn,
              dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "asub"] < 9), ]$asub,
              lwd = 1, col = "darkred", lty = 1)
        lines(dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "aslb"] > -9), ]$frctn,
              dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "aslb"] > -9), ]$aslb,
              lwd = 1, col = "darkred", lty = 1)
      }
      points(dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "asub"] < 9), ]$frctn,
             dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "asub"] < 9), ]$asub,
             col = infoColorASB, pch = 15, cex = 0.8)
      #lines(dataSA$nCum,
      #      dataSA$aslb,
      #      lwd = 1, col = "darkred", lty = 1)
      points(dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "aslb"] > -9), ]$frctn,
             dataSA[which(!is.na(dataSA[, "source"]) & dataSA[, "nCum"] < infoRIS & dataSA[, "aslb"] > -9), ]$aslb,
             col = infoColorASB, pch = 15, cex = 0.8)
      #lines(dataSA$nCum,
      #      dataSA$bsub,
      #      lwd = 1, col = "darkred", lty = 1)
      #lines(dataSA$nCum,
      #      dataSA$bslb,
      #      lwd = 1, col = "darkred", lty = 1)

      if (infoBSB) {
        lines(dataPlotSA$frctn,
              dataPlotSA$bsub,
              lwd = 1, col = "darkred", lty = 1)
        lines(dataPlotSA$frctn,
              dataPlotSA$bslb,
              lwd = 1, col = "darkred", lty = 1)
      }

      segments(c(0),
               c(-2, 0, 2),
               c(max(dataSA$frctn) * 1.1),
               c(-2, 0, 2),
               lty = c(2, 1, 2), lwd = 1, col = "gray25")
      lines(dataSA$frctn,
            dataSA$zCum,
            col = "blue3", lwd = 2)
      segments(c(0),
               c(0),
               dataSA[1, "frctn"],
               dataSA[1, "zCum"],
               lty = c(1), lwd = 2, col = 'blue3')
      points(dataSA$frctn,
             dataSA$zCum,
             col = "gray25", cex = 1 + dataSA$weight^2, pch = 15)

      arrows(max(dataSA$frctn),
             0,
             max(dataSA$frctn) * 1.1,
             0,
             lty = 1, length = 0.1)

      if (infoID) {
        points(dataSA$frctn,
               dataSA$zCum - dataSA$zCum,
               col = "gray25", cex = 0.6, pch = "|")
        text(dataSA$frctn,
             #ifelse(dataSA$zCum > 0,
             #      dataSA$zCum + 0.5,
             #      dataSA$zCum - 0.5),
             ifelse(dataSA$zCum > 0, -0.5, 0.5),
             dataSA$source,
             srt = 45,
             #pos = infoPosLabel,
             pos = 6 - infoPosLabel,
             cex = 0.8,
             col = c("gray20"))
      }

      #text(dataSA$time, dataSA$zCum - 0.5,
      #     c(round(dataSA$zCum, 2)),
      #     col = c("gray20"))

      rect(0,
           10,
           max(dataSA$frctn) * 0.99,
           8,
           lty = 0, col = rgb(1, 1, 1, 0.5))
      segments(c(0.03),
               c(10),
               0.05, # c(infoRIS / 20),
               c(10),
               lty = c(1), lwd = 1, col = 'blue3')

      #text(0.1, -8.5,
      #paste("Observed z score; observed power:",
      #round(dataSA$power[length(dataSA$power) - 1], 2)),
      #pos = 4, cex = 0.8)
      text(0.08, #infoRIS / 15,
           10,
           paste("Cumulative z score", sep = ""),
           pos = 4,
           cex = 0.7)
      segments(0.03,
               c(9),
               0.05, # c(infoRIS / 20),
               c(9),
               lty = c(1), lwd = 1.5, col = "darkred")
      text(0.08, # infoRIS / 15,
           9,
           paste("Parameters for alpha-spending boundary:"),
           pos = 4, cex = 0.7)
      text(0.08, # infoRIS
           8.3, #8.5,
           paste(ifelse(infoMeasure %in% c("MD", "SMD"),
                        infoMeasure,
                        "Assumed effect "
           ),
           " = ",
           round(infoPES, 3),
           ifelse(infoMeasure %in% c("MD", "SMD"),
                  "",
                  paste("; RRR",
                        ifelse(infoRRR < 0.001,
                               ifelse(infoRRR > 0,
                                      " < 0.1%",
                                      paste(" = ",
                                            -floor(infoRRR * 100),
                                            "%",
                                            sep = "")
                               ),
                               paste(" = ",
                                     floor(infoRRR * 100),
                                     "%",
                                     sep = "")
                        ),
                        sep = "")
           ),
           "; assumed power: ", round(1 - infoBeta, 2),
           "; assumed alpha: ", infoAlpha, sep = ""
           ),
           pos = 4, cex = 0.7)
      segments(1, # c(infoRIS),
               c(-9),
               1, # c(infoRIS),
               c(9),
               lty = c(2), col = "darkred")
      text(ifelse((infoCases / infoRIS) < 1,
                  1.05,
                  0.95), # 1, infoRIS
           -10,
           paste("RIS = ", ceiling(infoRIS), sep = ""),
           pos = ifelse((infoCases / infoRIS) < 1, 2, 4),
           cex = 0.7, col = "darkred")


      segments(dataSA$frctn[nrow(dataSA[!is.na(dataSA$source), ])],
               dataSA$zCum[nrow(dataSA[!is.na(dataSA$source), ])],
               dataSA$frctn[nrow(dataSA[!is.na(dataSA$source), ])],
               c(-8.5),
               lty = c(2), col = "blue3")
      text(ifelse((infoCases / infoRIS) < 0.5,
                  dataSA$frctn[nrow(dataSA[!is.na(dataSA$source), ])] * 0.95,
                  dataSA$frctn[nrow(dataSA[!is.na(dataSA$source), ])] * 1.05),
           -9,
           paste("AIS = ",
                 ceiling(max(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum))),
           pos = ifelse((infoCases / infoRIS) < 0.5, 4, 2),
           cex = 0.7, col = "blue 4")
    }

    output <- lsDoTSA

  })
}
