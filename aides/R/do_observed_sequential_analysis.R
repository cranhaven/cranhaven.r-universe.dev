#' @title Observed sequential analysis.
#'
#' @author Enoch Kang
#'
#' @description
#' **DoOSA()** is a function for conducting observed sequential analysis.
#'
#' @param data    DATAFRAME consists of relevant information.
#' @param source  CHARACTER for labeling the included data sets.
#' @param time    NUMERIC values of time sequence.
#' @param n       INTEGER values of sample sizes.
#' @param es      NUMERIC values of effect sizes.
#' @param se      NUMERIC values of standard errors for the effect sizes.
#' @param r1      INTEGER values of observed events in group 1 in the included data.
#' @param m1      NUMERIC values of estimated means in group 1 in the included data.
#' @param sd1     NUMERIC values of standard deviations in group 1 in the
#'                included data.
#' @param n1      INTEGER values of sample sizes in group 1 in the included data.
#' @param r2      INTEGER values of observed events in group 2 in the included data.
#' @param m2      NUMERIC values of estimated means in group 2 in the included data.
#' @param sd2     NUMERIC values of standard deviations in group 2 in the
#'                included data.
#' @param n2      INTEGER values of sample sizes in group 2 in the included data.
#' @param group   CHARACTER for labeling two groups.
#' @param ref     NUMERIC values of 1 or 2 for indicating group 1 or 2 as reference.
#' @param prefer  CHARACTER of "small" and "large" for indicating which direction
#'                is beneficial effect in statistic test.
#' @param measure CHARACTER for indicating which statistic measure should be used.
#' @param model   CHARACTER of "random" and "fixed" for indicating whether
#'                to use random-effects model or fixed-effect model.
#' @param method  CHARACTER for indicating which estimator should be used in
#'                random-effects model. In addition to the default "DL" method,
#'                the current version also supports "REML" and "PM" methods for
#'                calculating heterogeneity estimator.
#' @param pooling CHARACTER for indicating which method has to be used for pooling
#'                binary data. Current version consists of "IV" and "MH" for
#'                binary data pooling.
#' @param trnsfrm  CHARACTER for indicating which method for transforming pooled
#'                 proportion. Current version supports "none", "logit", "log",
#'                 "arcsine", and "DAT" for the transformation.
#' @param poolProp CHARACTER for indicating which method has to be used for pooling
#'                 proportion. Current version supports "IV" and "GLMM" for the
#'                 data pooling.
#' @param alpha   NUMERIC value between 0 to 1 for indicating the assumed type I
#'                error.
#' @param beta    NUMERIC value between 0 to 1 for indicating the assumed type II
#'                error.
#' @param anchor  NUMERIC value for indicating the presumed meaningful effect based
#'                on anchor-based approach.
#' @param adjust  CHARACTER for indicating how to adjust optimal information size.
#'                Current version consists of "none", "D2", "I2", "CHL", "CHM", and
#'                "CHH" for the adjustment.
#' @param plot    LOGIC value for indicating whether to illustrate alpha-spending
#'                monitoring plot.
#' @param SAP     LOGIC value for indicating whether to show sequential-adjusted
#'                power.
#'
#'
#' @details
#' 1. Basic information for the function **DoOSA()**:
#' **DoOSA()** supports observed sequential analysis of aggregate data synthesis
#' based on head-to-head comparison using either binary or continuous data in
#' each group. Minimum information for the function **DoOSA()** encompasses a data
#' set of study-level data, and time sequence. Operative points of using function
#' **DoOSA()** are listed below:
#'
#' 1.1. Parameter `data` should be used for assigning a data set.
#'
#' 1.2. Study-level data have to be assigned according to outcome type:
#'
#' 1.2.1. **For dichotomous outcome**: Parameter `n1` and `n2` should be defined
#' with parameter `r1` and `r2`.
#'
#' 1.2.2. **For continuous outcome**: parameter `n1` and `n2` should be defined
#' with parameter `m1`, `sd1`, `m2`, `sd2`.
#'
#' 1.3. Parameter `source` and `time` are required for doing observed sequential
#' analysis. Other parameters are auxiliary.
#'
#' 2. Default in the function **DoOSA()**
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
#' 2.4. Default on the parameter `adjust` is `"D2"` for adjusting optimal information
#' size (OIS) based on diversity (D-squared statistics). Other eligible arguments
#' for the parameter are `"None"` for the OIS without adjustment, `"I2"` for adjusted
#' OIS based on I-squared statistics, `"CHL"` for adjusted OIS based on low
#' heterogeneity by multiplying 1.33, `"CHM"` for adjusted OIS by multiplying 2
#' due to moderate heterogeneity, and `"CHL"` for adjusted OIS by multiplying 4
#' due to high heterogeneity.
#'
#'
#' @return
#' **DoOSA()** returns a summary on the result of sequential analysis, and can be
#' stored as an object in `DoOSA` class. Explanations of returned information are
#' listed as follows:
#'
#' \item{studies}{Numbers of studies included in the sequential analysis.}
#' \item{AIS}{Acquired information size refers to the total sample size in the
#'       sequential analysis.}
#' \item{alpha}{A numeric value of type I error for the sequential analysis.}
#' \item{beta}{A numeric value of type II error for the sequential analysis.}
#' \item{OES}{A numeric value of observed effect size of meta-analysis.}
#' \item{variance}{A numeric value of variance of meta-analysis.}
#' \item{diversity}{A numeric value to show diversity in the pooled analysis.}
#' \item{AF}{A numeric value of adjustment factor.}
#' \item{OIS.org}{A numeric value for optimal information size without adjustment.}
#' \item{OIS.adj}{A numeric value for optimal information size with adjustment.}
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
#' Revicki, D., Hays, R. D., Cella, D., & Sloan, J. (2008). Recommended methods
#' for determining responsiveness and minimally important differences for
#' patient-reported outcomes. **Journal of clinical epidemiology**, *61(2)*,
#' 102-109.
#' https://doi.org/10.1016/j.jclinepi.2007.03.012.
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
#' @seealso \code{\link{DoSA}}, \code{\link{PlotOSA}}, \code{\link{PlotPower}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993bin")
#'
#' # 2. Perform observed sequential analysis
#' output <- DoOSA(Fleiss1993bin, study, year,
#'                 r1 = d.asp, n1 = n.asp,
#'                 r2 = d.plac, n2 = n.plac,
#'                 measure = "RR",
#'                 group = c("Aspirin", "Control"))
#'
#' ## End(Not run)
#'
#' @export DoOSA



DoOSA <- function(data     = NULL,
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
                  anchor   = NULL,
                  adjust   = "D2",
                  plot     = FALSE,
                  SAP      = FALSE) {

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
  lgcInAnchor <- ifelse(is.null(substitute(anchor)), FALSE, TRUE)

  lgcReq1     <- ifelse(lgcInData == TRUE, TRUE, FALSE)
  lgcReq2     <- ifelse(FALSE %in% c(lgcInN, lgcInES, lgcInSE), FALSE, TRUE)
  lgcReq3     <- ifelse(FALSE %in% c(lgcInR1, lgcInN1, lgcInR2, lgcInN2), FALSE, TRUE)
  lgcReq4     <- ifelse(FALSE %in% c(lgcInM1, lgcInSD1, lgcInN1, lgcInM2, lgcInSD2, lgcInN2), FALSE, TRUE)
  lgcReq5     <- ifelse(FALSE %in% c(lgcInSource, lgcInTime), FALSE, TRUE)

  lgcStop1     <- ifelse(lgcReq1 == TRUE, FALSE, TRUE)
  infoLgcStop1 <- ifelse(lgcStop1 == TRUE,
                        'Argument "data" should be used for assigning a data set.',
                        "")
  lgcStop2     <- ifelse(TRUE %in% c(lgcReq2, lgcReq3, lgcReq4), FALSE, TRUE)
  infoLgcStop2 <- ifelse(lgcStop2 == TRUE, 'Arguments "n", "es", and "se" should be defined for the analysis based on study-level data.
                        Arguments "n1" and "n2" should be defined with "r1" and "r2" for dichotomous outcome based on arm-level data.
                        Or arguments "n1" and "n2" should be defined with "m1", "sd1", "m2", "sd2" for continuous outcome based on arm-level data.',
                        "")
  lgcStop3     <- ifelse(lgcReq5 == TRUE, FALSE, TRUE)
  infoLgcStop3 <- ifelse(lgcStop3 == FALSE,
                        'Argument "source" and "time" are required',
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
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))


  # 02. DEFINE core data -----
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
  infoAnchor   <- anchor
  infoAdjust   <- adjust
  infoPlot     <- plot
  infoSAP      <- SAP

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


  if (infoModel == "random") {
    infoVarOrg <- 1 / sum(outMA$w.random)
    infoESOrg  <- outMA$TE.random
    infoZOrg   <- outMA$zval.random
  } else {
    infoVarOrg <- 1 / sum(outMA$w.fixed)
    infoESOrg  <- outMA$TE.fixed
    infoZOrg   <- outMA$zval.fixed
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

    infoRRR    <- (infoProp2 - infoProp1) / infoProp2
    infoESMA   <- infoProp2 - infoProp1
    infoVarMA  <- (infoProp2 + infoProp1) / 2 * (1 - (infoProp2 + infoProp1) / 2)
    infoSEMA   <- infoVarMA
    #infoZScrMA <- infoESMA / infoVarMA
    #infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
  } else if (base::isFALSE(lgcReq3)) {
    infoRRR    <- NA
    infoESMA   <- ifelse(infoModel == "random", outMA$TE.random, outMA$TE.fixed)
    infoSEMA   <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
    #infoVarMA <- ifelse(infoModel == "random", 1 / sum(outMA$w.random), 1 / sum(outMA$w.fixed))
    #infoVarMA    <- ifelse(infoModel == "random", 1 / sum(1 / (outMA$seTE + outMA$tau2)), 1 / sum(1 / outMA$seTE))
    infoVarMA  <- (infoSEMA * sqrt(infoCases - 2))^2
    #infoVarMA    <- infoSEMA^2
    #infoZScrMA <- ifelse(infoModel == "random", outMA$zval.random, outMA$zval.fixed)
  }

  #infoESMA  <- ifelse(infoModel == "random", outMA$TE.random, outMA$TE.fixed)
  #infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
  #infoVarMA <- (infoSEMA * sqrt(infoCases - 2))^2

  # Revicki, D., Hays, R. D., Cella, D., & Sloan, J. (2008). Recommended methods for determining responsiveness and minimally important differences for patient-reported outcomes. Journal of clinical epidemiology, 61(2), 102-109. https://doi.org/10.1016/j.jclinepi.2007.03.012.
  infoES    <- ifelse(infoESMA < infoAnchor, infoESMA, infoAnchor)
  infoOES   <- infoESMA
  infoOV    <- infoVarMA
  infoI2    <- outMA$I2

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
    #infoDivers   <- 1 / sum(1 / (outMA$seTE)) / 1 / sum(1 / (outMA$seTE + outMA$tau2))
    #infoDivers   <- (((outMA$seTE.random * sqrt(infoCases))^2) - ((outMA$seTE.fixed * sqrt(infoCases))^2)) / ((outMA$seTE.random * sqrt(infoCases))^2)
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

  ## 04.04 Calculate optimal information size (OIS)
  #infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * (infoSEMA * sqrt(infoCases - 2))^2 / infoOES^2
  #infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoOES^2))
  #infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(1 - infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoOES^2))
  #infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoOES^2))
  # X infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * (infoSEMA * sqrt(infoCases))^2 / (infoOES)
  #infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) / (infoOES^2)
  # X infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * infoSEMA / infoOES^2
  #infoOIS      <- (1 / (1 - infoDivers)) * 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoOES^2))
  #infoOIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * infoOV / (infoOES^2)
  infoOISOrg   <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(1 - infoBeta)))^2) * infoOV / infoOES^2
  infoZScrTrgt <- infoOES / infoSEMA

  if (infoAdjust == "none") {
    infoOIS    <- infoOISOrg
  } else {
    infoOISAdj <- infoOISOrg * infoAF
    infoOIS    <- infoOISAdj
  }

  if (infoOIS > infoCases) {
    dataPlotOSA <- as.data.frame(cbind(sample = c(1:ceiling(infoOIS)),
                                       frctn   = c(1:ceiling(infoOIS)) / infoOIS)
    )
  } else {
    dataPlotOSA <- as.data.frame(cbind(sample = c(1:infoCases),
                                       frctn   = c(1:infoCases) / infoOIS)
    )
  }

  dataPlotOSA$aslb     <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotOSA$frctn))) / 2); dataPlotOSA$aslb <- ifelse(dataPlotOSA$aslb == "-Inf", -10, dataPlotOSA$aslb)
  dataPlotOSA$asub     <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotOSA$frctn))) / 2); dataPlotOSA$asub <- ifelse(dataPlotOSA$asub == "Inf", 10, dataPlotOSA$asub)
  dataPlotOSA$bsub     <-  (qnorm(pnorm(qnorm(infoBeta) / dataPlotOSA$frctn)) + (qnorm(pnorm(qnorm(1 - infoAlpha / 2) / sqrt(1))) - qnorm(pnorm(qnorm(infoBeta) / sqrt(1)))))#; dataPlotOSA$bsub <- ifelse(dataPlotOSA$bsub < 0, 0, dataPlotOSA$bsub)
  dataPlotOSA$bslb     <- -(qnorm(pnorm(qnorm(infoBeta) / dataPlotOSA$frctn)) + (qnorm(pnorm(qnorm(1 - infoAlpha / 2) / sqrt(1))) - qnorm(pnorm(qnorm(infoBeta) / sqrt(1)))))#; dataPlotOSA$bslb <- ifelse(dataPlotOSA$bslb > 0, 0, dataPlotOSA$bslb)
  #dataPlotOSA$bsub     <-  (qnorm(pnorm(qnorm(infoBeta) / dataPlotOSA$frctn)) + (qnorm(pnorm(qnorm(1 - infoBeta) / sqrt(1))) - qnorm(pnorm(qnorm(infoBeta) / sqrt(1)))))#; dataPlotOSA$bsub <- ifelse(dataPlotOSA$bsub < 0, 0, dataPlotOSA$bsub)
  #dataPlotOSA$bslb     <- -(qnorm(pnorm(qnorm(infoBeta) / dataPlotOSA$frctn)) + (qnorm(pnorm(qnorm(1 - infoBeta) / sqrt(1))) - qnorm(pnorm(qnorm(infoBeta) / sqrt(1)))))#; dataPlotOSA$bslb <- ifelse(dataPlotOSA$bslb > 0, 0, dataPlotOSA$bslb)
  dataPlotOSA$pwrExpct <- 1 - pnorm(dataPlotOSA$bslb - (min(dataPlotOSA$bslb) - qnorm(infoBeta)))
  dataPlotOSA          <- dataPlotOSA[dataPlotOSA$aslb > -9, ]
  dataPlotOSA          <- dataPlotOSA[dataPlotOSA$asub < 9, ]
  #dataPlotOSA$pwrPrdct <- 1 - pnorm(qnorm(1 - infoAlpha / 2 * dataPlotOSA$frctn) - infoZOrg) + pnorm(-qnorm(1 - infoAlpha / 2 * dataPlotOSA$frctn) - infoZOrg)
  dataPlotOSA$pwrPrdct <- 1 - pnorm(dataPlotOSA$asub - infoZOrg) + pnorm(-dataPlotOSA$asub - infoZOrg)
  infoFrctnBSB         <- dataPlotOSA[max(which(dataPlotOSA$bsub < 0)), "frctn"]

  if (infoOIS < infoCases) {
    dataPlotOSA <- dataPlotOSA[dataPlotOSA$sample < infoOIS, ]
  }


  dataOSA <- dataIn[, c("source", "time", "n", "weight", "esCum", "seCum", "zCum")]
  dataOSA <- dataOSA[order(dataOSA$time), ]

  dataOSA$nCum  <- 0

  for (study.i in c(1:infoNumStud)) {
    if (study.i == 1) {
      dataOSA[study.i, "nCum"] <- dataOSA[study.i, "n"]
    } else {
      dataOSA[study.i, "nCum"] <- dataOSA[study.i, "n"] + dataOSA[study.i - 1, "nCum"]
    }
  }

  dataOSA$frctn   <- dataOSA$nCum / infoOIS
  dataOSA$asub    <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataOSA$frctn))) / 2); dataOSA$asub <- ifelse(dataOSA$asub == "Inf", 10, dataOSA$asub)
  dataOSA$aslb    <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataOSA$frctn))) / 2); dataOSA$aslb <- ifelse(dataOSA$aslb == "-Inf", -10, dataOSA$aslb)
  dataOSA$bsub    <-  (qnorm(pnorm(qnorm((infoBeta)) / dataOSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataOSA$bsub <- ifelse(dataOSA$bsub < 0, 0, dataOSA$bsub)
  dataOSA$bslb    <- -(qnorm(pnorm(qnorm((infoBeta)) / dataOSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataOSA$bslb <- ifelse(dataOSA$bslb > 0, 0, dataOSA$bslb)
  #dataOSA$bsub    <-  (qnorm(pnorm(qnorm((infoBeta)) / dataOSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoBeta)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataOSA$bsub <- ifelse(dataOSA$bsub < 0, 0, dataOSA$bsub)
  #dataOSA$bslb    <- -(qnorm(pnorm(qnorm((infoBeta)) / dataOSA$frctn)) + (qnorm(pnorm(qnorm((1 - infoBeta)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataOSA$bslb <- ifelse(dataOSA$bslb > 0, 0, dataOSA$bslb)

  if (infoSAP == TRUE) {
    dataOSA$pwrCum  <- 1 - pnorm(qnorm(1-infoAlpha / 2) - dataOSA$zCum) + pnorm(-qnorm(1 - infoAlpha / 2) - dataOSA$zCum)
    #dataOSA$pwrSqnt <- 1 - pnorm(qnorm(1-infoAlpha / 2 * dataOSA$frctn) - dataOSA$zCum) + pnorm(-qnorm(1 - infoAlpha / 2 * dataOSA$frctn) - dataOSA$zCum)
    dataOSA$pwrSqnt <- 1 - pnorm(dataOSA$asub - dataOSA$zCum) + pnorm(-dataOSA$asub - dataOSA$zCum)
    dataOSA$pwrCum  <- ifelse(dataOSA$pwrCum > 1, 1, dataOSA$pwrCum)
    dataOSA$pwrSqnt <- ifelse(dataOSA$pwrSqnt > 1, 1, dataOSA$pwrSqnt)
  } else {
    dataOSA$pwrCum  <- NA
    dataOSA$pwrSqnt <- NA
  }
  #dataOSA$pwrCum <- 1 - pnorm(qnorm(1 - infoAlpha / 2) - dataOSA$zCum) + pnorm(-qnorm(1 - infoAlpha / 2) - dataOSA$zCum)
  #dataOSA$pwrCum <- 1 - pnorm(qnorm(1 - infoAlpha / 2) / sqrt(dataOSA$frctn)/2-dataOSA$zCum)+pnorm(-qnorm(1-alpha/2)/sqrt(dataOSA$frctn)/2-dataOSA$zCum);dataOSA$power


  dataOSA <- as.data.frame(dataOSA)

  dataOSA <- dataOSA[order(dataOSA$frctn), ]

  infoColorASB <- ifelse(dataOSA$nCum > infoOIS,
                            rgb(1, 1, 1, 1),
                            "red4")
  infoPosLabel <- ifelse(dataOSA$zCum > 0, 4, 2)
  infoPwrObs   <- dataOSA[nrow(dataOSA), "pwrSqnt"]


  if (max(dataOSA$frctn) < infoFrctnBSB) {
    dataOSA[nrow(dataOSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA, NA,
                                    round(infoOIS * infoFrctnBSB, 0), infoFrctnBSB,
                                    qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(infoFrctnBSB))) / 2),
                                    -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(infoFrctnBSB))) / 2),
                                    0, 0,
                                    (1 - infoBeta) * infoFrctnBSB,
                                    NA
                                    )
    } else {
    dataOSA <- dataOSA
    }

  dataOSA[nrow(dataOSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA, NA,
                                  round(infoOIS, 0), 1,
                                  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  1 - infoBeta,
                                  NA
                                  )


  ## 04.05 Adjusted MA -----
  lgcMAAdj <- abs(dataOSA$zCum[infoNumStud]) < abs(dataOSA$asub[infoNumStud])
  infoLgcMAAdj  <- ifelse(lgcMAAdj,
                         "Adjusted confidence interval is suggested to be performed.",
                         "Adjusted confidence interval is not necessary to be performed.")
  infoPValMAAdj <- pnorm(ifelse(dataOSA$asub[infoNumStud] > 5.3,
                                5.3,
                                dataOSA$asub[infoNumStud]))

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



  # 05. BUILD an DoOSA object -----
  lsDoOSA <- list(name      = "Observed sequential analysis",
                  OIS       = infoOIS,
                  studies   = infoNumStud,
                  AIS       = infoCases,
                  alpha     = infoAlpha,
                  beta      = infoBeta,
                  measure   = infoMeasure,
                  model     = infoModel,
                  method    = infoMethod,
                  pooling   = pooling,
                  prefer    = infoPrefer,
                  OES       = infoOES,
                  RRR       = ifelse(infoRRR < 0.001, "< 0.001", infoRRR),
                  variance  = infoOV,
                  zExpect   = infoZScrTrgt,
                  diversity = infoDivers,
                  adjust    = infoAdjust,
                  AF        = ifelse(infoAdjust == "none",
                                     "Undefined",
                                     infoAF),
                  OIS.org   = ceiling(infoOISOrg),
                  OIS.adj   = ifelse(infoAdjust == "none",
                                     "No adjusted",
                                     ceiling(infoOISAdj)),
                  power     = infoPwrObs
                  )
  class(lsDoOSA)    <- c("aides", "DoOSA")
  lsDoOSA$frctn      <- paste(round(dataOSA$frctn[infoNumStud], 4) * 100,
                            "%",
                            sep = "")
  lsDoOSA$weight    <- paste(round(dataOSA$weight[c(1:infoNumStud)], 4) * 100,
                            "%",
                            sep = "")
  lsDoOSA$es.cum    <- round(dataOSA$esCum[c(1:infoNumStud)], 3)
  lsDoOSA$se.cum    <- round(dataOSA$seCum[c(1:infoNumStud)], 3)
  lsDoOSA$zval.cum  <- round(dataOSA$zCum[c(1:infoNumStud)], 3)
  lsDoOSA$asb       <- round(dataOSA[c(1:infoNumStud), c("aslb", "asub")], 3)
  lsDoOSA$aslb      <- round(dataOSA$aslb[infoNumStud], 3)
  lsDoOSA$asub      <- round(dataOSA$asub[infoNumStud], 3)

  if (infoPlot == TRUE | infoSAP == TRUE) {
    lsDoOSA$group          <- infoGroup
    lsDoOSA$ref            <- infoRef
    lsDoOSA$color.ASB      <- infoColorASB
    lsDoOSA$position.label <- infoPosLabel
    lsDoOSA$data           <- dataOSA
    lsDoOSA$data.bounds    <- dataPlotOSA
    lsDoOSA$lsOutMA        <- outMA
    }



  # 06. RETURN summary of function `DoOSA()` -----
  #cat(paste("\n"), fill = TRUE, sep = "")
  cat(paste("Summary of observed sequential analysis (main information)\n",
            " Acquired sample size: ",
            infoCases,
            "\n Optimal sample size",
            ifelse(infoAdjust != "none",
                   " (heterogeneity adjusted): ",
                   " (without adjusted): "
                   ),
            ifelse(infoAdjust == "none",
                   ceiling(infoOISOrg),
                   ceiling(infoOISAdj)
                   ),
            "\n Cumulative z score: ",
            round(dataOSA$zCum[infoNumStud], 3),
            "\n Alpha-spending boundary: ",
            round(dataOSA$asub[infoNumStud], 3),
            " and ",
            round(dataOSA$aslb[infoNumStud], 3),
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
    cat(paste("\n",
              "Adjusted confidence interval based on type I error ",
              ifelse(dataOSA$asub[infoNumStud] > 5.3,
                     "< 0.000001",
                     1 - pnorm(dataOSA$asub[infoNumStud])
                     ),
              ": \n",
              round(infoLCIMAAdj, 3),
              " to ",
              round(infoUCIMAAdj, 3),
              sep = ""),
      fill = TRUE, sep = "")
  }

  cat(paste("\n",
            "Summary of observed sequential analysis (additional information)",
            "\n 1. Observed information",
            "\n 1.1. Defined type I error: ",
            infoAlpha,
            "\n 1.2. Defined type II error: ",
            infoBeta,
            "\n 1.3. Defined power: ",
            1 - infoBeta,
            "\n 1.4. Observed effect size ",
            round(infoOES, 3),
            ifelse(lgcReq3,
                   paste("\n      (risks in group 1 and 2 were ",
                         round(infoProp1, 10) * 100,
                         "% and ",
                         round(infoProp2, 10) * 100,
                         "% respectively; ",
                         trnsfrm, " transformation with ", infoPoolProp,
                         " method for pooling the proportion; RRR ",
                         ifelse(infoRRR < 0.001, "< 0.001)",
                                paste("= ", round(infoRRR, 3), ")",
                                      sep = "")),
                         sep = ""),
                   ""
                   ),
            "\n 1.5. Observed variance: ",
            round(infoOV, 3),

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
            paste("The optimal information size is calculated ",
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
            "\n 3.3. Adjustment factor: ",
            ifelse(infoAdjust == "none",
                   "Undefined",
                   round(infoAF, 3)),
            sep = ""),
      fill = TRUE, sep = "")



  # 07. ILLUSTRATE proportion of alpha-spending monitoring plot -----

  if (infoPlot == TRUE) {

    plot(dataOSA$frctn * 1.2, # nCum
         dataOSA$asub,
         type = "l", frame = F,
         xlim = c(0, max(dataOSA$frctn) * 1.2), # nCum
         ylim = c(ceiling(min(dataOSA$aslb)) * (-10) / ceiling(min(dataOSA$aslb)),
                ceiling(max(dataOSA$asub)) * 10 / ceiling(max(dataOSA$asub)) + 1),
         col = rgb(1, 1, 1, 0),
         xlab = "Information size",
         yaxt = "n", xaxt="n", #"darkred"
         #ylab=paste("Favors", Txs[1], "   (Z score)   Favors",Txs[2]),
         ylab = "",
         main = "Observed sequential analysis")
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
         at = c(0, 0.25, 0.5, 0.75, 1, max(dataOSA$frctn) * 1.2),
         labels = c(0,
                    ceiling(max(dataOSA$nCum) * 0.25),
                    ceiling(max(dataOSA$nCum) * 0.5),
                    ceiling(max(dataOSA$nCum) * 0.75),
                    ceiling(max(dataOSA$nCum)),
                    ceiling(max(dataOSA$nCum) * 1.2)),
         cex.axis = 1)
    axis(side = 2, at = c(seq(ceiling(min(dataOSA$aslb)) * (-10) / ceiling(min(dataOSA$aslb)),
                          ceiling(max(dataOSA$asub)) * 10 / ceiling(max(dataOSA$asub)), 2)),
         padj = 0, hadj = 1, las = 1)
    mtext("Cumulative\n z score", side = 3, line = 0, at = -0.05) # -infoOIS * 0.05
    mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[2], infoGroup[1])),
          side = 2, line = 2, at = 5,
          cex = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)) #(1/sqrt(seq(11,100,by=1)))^2*10
    mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[1], infoGroup[2])),
          side = 2, line = 2, at = -5,
          cex = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)) #(1/sqrt(seq(11,100,by=1)))^2*10

    #lines(dataOSA$nCum,
    #      dataOSA$asub,
    #      lwd = 1, col = "darkred", lty = 1)
    lines(dataPlotOSA$frctn, # sample
          dataPlotOSA$asub,
          lwd = 1, col = "darkred", lty = 2)


    points(dataOSA[which(!is.na(dataOSA[, "source"]) & dataOSA[, "nCum"] < infoOIS & dataOSA[, "asub"] < 9), ]$frctn, # nCum
           dataOSA[which(!is.na(dataOSA[, "source"]) & dataOSA[, "nCum"] < infoOIS & dataOSA[, "asub"] < 9), ]$asub,
           col = infoColorASB,
           pch = 21, bg = rgb(1, 1, 1, 1),
           cex = 0.8)
    #lines(dataOSA$nCum,
    #      dataOSA$aslb,
    #      lwd = 1, col = "darkred", lty = 1)
    lines(dataPlotOSA$frctn, # sample
          dataPlotOSA$aslb,
          lwd = 1, col = "darkred", lty = 2)
    points(dataOSA[which(!is.na(dataOSA[, "source"]) & dataOSA[, "nCum"] < infoOIS & dataOSA[, "aslb"] > -9), ]$frctn, # nCum
           dataOSA[which(!is.na(dataOSA[, "source"]) & dataOSA[, "nCum"] < infoOIS & dataOSA[, "aslb"] > -9), ]$aslb,
           col = infoColorASB,
           pch = 21, bg = rgb(1, 1, 1, 1),
           cex = 0.8)
    #lines(dataOSA$nCum,
    #      dataOSA$bsub,
    #      lwd = 1, col = "darkred", lty = 1)
    #lines(dataOSA$nCum,
    #      dataOSA$bslb,
    #      lwd = 1, col = "darkred", lty = 1)

    segments(c(0),
             c(-2, 0, 2),
             c(max(dataOSA$frctn) * 1.1), # nCum
             c(-2, 0, 2),
             lty = c(2, 1, 2), lwd = 1, col = "gray25")
    lines(dataOSA$frctn, # nCum
          dataOSA$zCum,
          col = "blue3", lwd = 2)
    segments(c(0),
             c(0),
             dataOSA[1, "frctn"], # nCum
             dataOSA[1, "zCum"],
             lty = c(1), lwd = 2, col = 'blue3')
    points(dataOSA$frctn, # nCum
           dataOSA$zCum,
           col = "gray25", cex = 1 + dataOSA$weight^2, pch = 15)

    arrows(max(dataOSA$frctn), # nCum
           0,
           max(dataOSA$frctn) * 1.1, # nCum
           0,
           lty = 1, length = 0.1)

    #text(dataOSA$time, dataOSA$zCum - 0.5,
    #     c(round(dataOSA$zCum, 2)),
    #     col = c("gray20"))

    rect(0,
         10,
         max(dataOSA$frctn) * 0.99, # infoOIS * 0.8,
         8,
         lty = 0, col = rgb(1, 1, 1, 0.5))
    #points(dataOSA[which(!is.na(dataOSA[, "source"])), ]$nCum,
    #       dataOSA[which(!is.na(dataOSA[, "source"])), ]$aslb,
    #       col = infoColorASB,
    #       pch = 21, bg = rgb(1, 1, 1, 1),
    #       cex = 0.8)
    segments(c(0.03),
             c(10),
             0.05, # c(infoOIS / 20),
             c(10),
             lty = c(1), lwd = 1, col = 'blue3')
    #text(0.1, -8.5,
          #paste("Observed z score; observed power:",
                #round(dataOSA$power[length(dataOSA$power) - 1], 2)),
          #pos = 4, cex = 0.8)
    text(0.08, # infoOIS / 15,
         10,
         paste("Cumulative z score", sep = ""),
         pos = 4, cex = 0.7)
    segments(0.03,
             c(9),
             0.05, # c(infoOIS / 20),
             c(9),
             lty = c(2), lwd = 1, col = "darkred")
    text(0.08, # infoOIS / 15,
         9,
         paste("Alpha-spending boundary"),
         pos = 4, cex = 0.7)
    text(0.08, # infoOIS
         8.3,
         paste("(",
               ifelse(infoMeasure %in% c("MD", "SMD"),
                      infoMeasure,
                      "Observed effect"),
               ifelse(abs(infoOES) < 0.001,
                      paste(" < 0.001", sep = ""),
                      paste(" = ", round(infoOES, 3), sep = "")),
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
               "; alpha: ", infoAlpha,
               "; power: ", round(1 - infoBeta, 2),
               ifelse(infoAdjust == "none",
                      "; no adjustment factor)",
                      paste("; ",
                            ifelse(infoAdjust == "D2",
                                   "diversity-based AF: ",
                                   ifelse(infoAdjust == "I2",
                                          "I-squared-based AF: ",
                                          paste(infoAdjust, "-based AF: ",
                                                sep = "")
                                          )
                                   ),
                            round(infoAF, 3), ")",
                            sep = "")
                      ),
               sep = ""),
         pos = 4, cex = 0.7)

    segments(1, # c(infoOIS),
             c(-9),
             1, # c(infoOIS),
             c(9),
             lty = c(3), col = "darkred")
    text(ifelse((infoCases / infoOIS) < 1,
                1.05,
                0.95), # 1,  infoOIS
         -10,
         paste("OIS = ", ceiling(infoOIS), sep = ""),
         pos = ifelse((infoCases / infoOIS) < 1, 2, 4),
         cex = 0.7)

    segments(dataOSA$frctn[nrow(dataOSA[!is.na(dataOSA$source), ])],
             dataOSA$zCum[nrow(dataOSA[!is.na(dataOSA$source), ])],
             dataOSA$frctn[nrow(dataOSA[!is.na(dataOSA$source), ])],
             c(-8.5),
             lty = c(3), col = "blue3")
    text(ifelse((infoCases / infoOIS) < 0.5,
                dataOSA$frctn[nrow(dataOSA[!is.na(dataOSA$source), ])] * 0.95,
                dataOSA$frctn[nrow(dataOSA[!is.na(dataOSA$source), ])] * 1.05), #dataOSA$frctn[nrow(dataOSA[!is.na(dataOSA$source), ])], # infoOIS nCum
         -9, # 9,
         paste("AIS = ",
               ceiling(max(dataOSA[which(!is.na(dataOSA[, "source"])), ]$nCum)),
               ifelse(infoSAP == TRUE,
                      paste("\n",
                            "(SAP = ",
                            round(infoPwrObs, 3),
                            ")",
                            sep = ""),
                      ""),
               sep = ""),
         pos = ifelse((infoCases / infoOIS) < 0.5, 4, 2),
         cex = 0.7)
  }

  output <- lsDoOSA

}
