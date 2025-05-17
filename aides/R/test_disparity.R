#' @title Test assumption of disparities in sample size.
#'
#' @author Enoch Kang
#'
#' @description
#' **TestDisparity()** is a function for disparities in sample size analysis.
#'
#' @param n       NUMERIC values for sample size (n) of each study.
#' @param data    DATA FRAME consists of columns for study label, study
#'                year, and sample size.
#' @param study   CHARACTER for study labels.
#' @param time    NUMERIC values of time sequence.
#' @param outlier CHARACTER for method of outlier detection. Current version
#'                consists of four methods, and three of them can be used for
#'                normal distribution. The rest one method can be used for data
#'                with non-normal distribution. For normal distribution data,
#'                outlier detection can be performed using 1.5 interquartile range
#'                method ("IQR"), z score method ("Z"), and generalized extreme
#'                studentized deviate method ("GESD"). For data with non-normal
#'                distribution, package *aides* detects outliers using median
#'                absolute deviation method ("MAD"). Parameter `outlier` with
#'                argument "Default" automatically takes "GESD" or "MAD" based on
#'                data distribution.
#' @param ctf     NUMERIC value of cutoff point for proportion of excessive cases
#'                in outlier-based disparity test, and the value should be larger
#'                than 0.
#' @param vrblty  CHARACTER for method of variability detection. Current version
#'                consists of two methods in terms of coefficient of variation
#'                (CV) and robust CV (RCV) using MAD. For normal distribution data,
#'                variability detection can be performed common CV method, and
#'                MAD based RCV could be used for data with non-normal distribution.
#'                Default argument for parameter `vrblty` is "CV" in order to
#'                detect variability.
#' @param ctfLwr  NUMERIC value of cutoff value for lower boundary of variability
#'                that should be larger than 0.
#' @param ctfUpr  NUMERIC value of cutoff value for upper boundary of variability
#'                that should be larger than `ctfLwr`.
#' @param rplctns INTEGER value of bootstrap replications for obtaining probability
#'                of variability-based disparity test, and the integer must be equal
#'                or larger than 1,000.
#' @param plot    LOGIC value for indicating whether to illustrate proportion of
#'                excessive cases plot.
#' @param sort    CHARACTER of data sorting reference for disparity plot. Current
#'                version consists of "time", "size", and "excessive" for
#'                displaying observations on disparity plot of outlier(s).
#' @param color   CHARACTER of a color name for emphasizing the significant
#'                disparities in sample size.
#'
#'
#' @return
#' **TestDisparity()** returns a summary of result regarding disparities in sample
#' size, and can be stored as an object in `disparity` class. Explanations of returned
#' information are listed as follows:
#'
#' \item{disparity}{String to return the overall judgement of disparity test.}
#' \item{w.normality}{A numeric value of statistics of normality test to show whether
#'       sample sizes among studies are distributed normally.}
#' \item{p.normality}{A numeric value of p-value of normality test to show whether
#'       sample sizes among studies are distributed normally.}
#' \item{outlier.method}{String shows outlier detection method used in disparity test.}
#' \item{vrblty.method}{String shows variability detection method used in disparity test.}
#' \item{outlier}{A data frame to show details of identified outliers (studies).}
#' \item{prop.outlier}{A numeric value to show proportion of outliers among all studies.}
#' \item{n.excessive}{A numeric value of excessive cases among all samples.}
#' \item{p.prop.outlier}{A numeric value of p-value of disparity outlier test.}
#' \item{lci.prop.outlier}{A numeric value for lower limit of 95% confidence interval
#'       of disparity outlier test.}
#' \item{uci.prop.outlier}{A numeric value for upper limit of 95% confidence interval
#'       of disparity outlier test.}
#' \item{variability}{A numeric value to show variability among all studies.}
#' \item{p.variability}{A numeric value of p-value of disparity variability test.}
#' \item{lci.variability}{A numeric value for lower limit of 95% confidence interval of
#'       disparity variability test.}
#' \item{uci.variability}{A numeric value for lower limit of 95% confidence interval of
#'       disparity variability test.}
#'
#'
#' @references
#' Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for
#' normality (complete samples). **Biometrika**, *52(3)*, 591-611.
#'
#' Rosner, B. (1983). Percentage Points for a Generalized ESD Many-Outlier
#' Procedure. **Technometrics**, *25(2)*, 165-172.
#'
#' Leys, C., Ley, C., Klein, O., Bernard, P., & Licata, L. (2013). Detecting
#' outliers: Do not use standard deviation around the mean, use absolute deviation
#' around the median. **Journal of experimental social psychology**, *49(4)*, 764-766.
#'
#' Rousseeuw, P. J.  & Croux C. (1993). Alternatives to the Median Absolute
#' Deviation, **Journal of the American Statistical Association**, *88(424)*,
#' 1273-1283. http://dx.doi.org/10.1080/01621459.1993.10476408
#'
#' Hendricks, W. A., & Robey, K. W. (1936). The sampling distribution of the
#' coefficient of variation. **The Annals of Mathematical Statistics**, *7(3)*,
#' 129-132.
#'
#' Sokal, R. R., & Braumann, C. A. (1980). Significance tests for coefficients
#' of variation and variability profiles. **Systematic Biology**, *29(1)*, 50-66.
#'
#'
#' @seealso \code{\link{TestDiscordance}}, \code{\link{PlotDisparity}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Olkin (1995)
#' library(meta)
#' data("Olkin1995")
#' data <- Olkin1995
#'
#' # 2. Calculate total sample size and standard error of each study
#' data$n  <- data$n.exp + data$n.cont
#'
#' # 3. Test disparities in sample sizes
#' output <- TestDisparity(n, data, author, year)
#'
#' # 4. Illustrate disparity plot
#' TestDisparity(n, data, author, year, plot = TRUE)
#'
#' ## End(Not run)
#'
#' @export TestDisparity



TestDisparity <- function(n,
                          data    = NULL,
                          study   = NULL,
                          time    = NULL,
                          outlier = NULL,
                          ctf     = 0.20,
                          vrblty  = NULL,
                          ctfLwr  = 0.10,
                          ctfUpr  = 0.30,
                          rplctns = 1000,
                          plot    = FALSE,
                          sort    = NULL,
                          color   = "firebrick3") {

  # 01. DEFINE core data -----
  if (is.null(data)) {
    dataCases  <- n

    if (base::isFALSE(is.null(study))) {
      vctStudy <- study
    } else {
      vctStudy <- paste(rep("Study ", length(n)),
                         c(1:length(n)),
                         sep = "")
    }

    if (base::isFALSE(is.null(time))) {
      vctTime  <- time
    } else {
      vctTime  <- c(1:length(n))
      }

    } else {

      n         <- deparse(substitute(n))
      study     <- deparse(substitute(study))
      time      <- deparse(substitute(time))
      dataCases <- data[, n]

      if (study == "NULL") {
        vctStudy <- paste(rep("Study ", length(dataCases)),
                          c(1:length(dataCases)),
                          sep = "")
      } else {
        vctStudy  <- data[, study]
      }

      if (time == "NULL") {
        vctTime  <- c(1:length(dataCases))
      } else {
        vctTime   <- data[, time]
      }

    }

  dataDisparity <- data.frame(study = vctStudy,
                              time = vctTime,
                              n = dataCases)

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))
  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))



  # 02. CHECK arguments -----
  lgcData     <- ifelse(is.null(data),
                        FALSE,
                        ifelse(base::isFALSE(length(data) >= 3),
                               TRUE, FALSE)
  )

  lgcN        <- ifelse(is.null(dataCases),
                        TRUE,
                        ifelse(base::isFALSE(is.numeric(dataCases)),
                               TRUE,
                               ifelse(base::isFALSE(min(dataCases) > 0),
                                      TRUE,
                                      ifelse("FALSE" %in% names(table(dataCases %% 1 == 0)),
                                             TRUE, FALSE)
                               )
                        )
  )

  lgcStudy    <- ifelse(length(vctStudy) == length(dataCases),
                        FALSE,
                        TRUE
  )

  lgcTime     <- ifelse(length(vctTime) == length(dataCases),
                        FALSE,
                        TRUE
  )

  lgcOtlr     <- ifelse(is.null(outlier), FALSE,
                        ifelse(outlier %in% c("Default", "IQR", "Z", "GESD", "MAD"),
                               FALSE, TRUE)
  )

  lgcCutoff   <- ifelse(is.null(ctf), FALSE,
                        ifelse(is.numeric(ctf),
                               ifelse((ctf > 0) == TRUE,
                                      FALSE, TRUE),
                               TRUE)
  )

  lgcVrblty     <- ifelse(is.null(vrblty), FALSE,
                          ifelse(vrblty %in% c("CV", "MAD"),
                                 FALSE, TRUE)
  )

  lgcCutoffL  <- ifelse(is.null(ctfLwr), FALSE,
                        ifelse(is.numeric(ctfLwr),
                               ifelse((ctfLwr > 0) == TRUE,
                                      FALSE, TRUE),
                               TRUE)
  )

  lgcCutoffH  <- ifelse(is.null(ctfUpr), FALSE,
                        ifelse(is.numeric(ctfUpr),
                               ifelse(ctfUpr > ctfLwr,
                                      FALSE, TRUE),
                               TRUE)
  )

  lgcReplicate  <- ifelse(is.null(rplctns), FALSE,
                        ifelse(is.numeric(rplctns),
                               ifelse(rplctns >= 1000,
                                      FALSE, TRUE),
                               TRUE)
  )

  lgcPlot     <- ifelse(is.null(plot), FALSE,
                        ifelse(is.logical(plot), FALSE, TRUE))

  lgcSort     <- ifelse(is.null(sort), FALSE,
                        ifelse(length(sort) == 1,
                               ifelse(sort %in% c("time", "size", "excessive"),
                                      FALSE, TRUE),
                               TRUE)
  )

  lgcColor    <- ifelse(is.null(color), FALSE,
                        ifelse(length(color) == 1,
                               ifelse(color %in% colors(), FALSE, TRUE),
                               TRUE)
  )


  if (lgcData) {
    infoStopData  <- 'Argument "data" must be a data frame consisting of three columns for study label, study year, and sample size.'
    }

  if (lgcN) {
    infoStopN     <- 'Argument "n" must be a integer vector for sample size of each study.'
    }

  if (lgcStudy) {
    infoStopStudy <- 'Argument "study" must be a vector for study label of each study, and length of the vector should be the same with length of the vector for sample size.'
    }

  if (lgcTime) {
    infoStopTime  <- 'Argument "time" must be a vector for time of each study, and length of the vector should be the same with length of the vector for sample size.'
  }

  if (lgcOtlr) {
    infoStopOtlr  <- 'Argument "outlier" must be characters ("Default", "IQR", "Z", "GESD", or "MAD") for indicating the method of outlier detection.'
  }

  if (lgcCutoff) {
    infoStopCutoff  <- 'Argument "ctf" must be a numeric value larger than 0 in order to determine a cutoff value of proportion of excessive cases in outlier-based disparity test.'
  }

  if (lgcVrblty) {
    infoStopVrblty <- 'Argument "vrblty" must be characters ("CV" or "MAD") for indicating the method of variability detection.'
  }

  if (lgcCutoffL) {
    infoStopCutoffL <- 'Argument "ctfLwr" must be a numeric value larger than 0 in order to determine a cutoff value for low variability.'
    }

  if (lgcCutoffH) {
    infoStopCutoffH <- 'Argument "ctfUpr" must be a numeric value larger than `ctfLwr` in order to determine a cutoff value for high variability.'
  }

  if (lgcReplicate) {
    infoStopReplicate <- 'Argument "rplctns" must be an integer (equal or larger than 1,000) for bootstrap replications for obtaining probability of variability-based disparity test.'
  }

  if (lgcPlot) {
    infoStopPlot  <- 'Argument "plot" must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to illustrate disparity plot.'
    }

  if (lgcSort) {
    infoStopSort  <- 'Argument "sort" must be characters ("time", "size", or "excessive") for indicating data sort reference in order to display disparity plot.'
    }

  if (lgcColor) {
    infoStopColor <- 'Argument "color" must be a color name in R for emphasizing the significant disparities in sample size.'
    }


  # 03. RETURN results of argument checking  -----
  if (lgcData | lgcN | lgcStudy | lgcTime |
      lgcOtlr | lgcCutoff |
      lgcVrblty | lgcCutoffL | lgcCutoffH | lgcReplicate |
      lgcPlot | lgcSort | lgcColor)
    stop(paste(ifelse(lgcData, paste(infoStopData, "\n", sep = ""), ""),
               ifelse(lgcN, paste(infoStopN, "\n", sep = ""), ""),
               ifelse(lgcStudy, paste(infoStopStudy, "\n", sep = ""), ""),
               ifelse(lgcTime, paste(infoStopTime, "\n", sep = ""), ""),
               ifelse(lgcOtlr, paste(infoStopOtlr, "\n", sep = ""), ""),
               ifelse(lgcCutoff, paste(infoStopCutoff, "\n", sep = ""), ""),
               ifelse(lgcVrblty, paste(infoStopVrblty, "\n", sep = ""), ""),
               ifelse(lgcCutoffL, paste(infoStopCutoffL, "\n", sep = ""), ""),
               ifelse(lgcCutoffH, paste(infoStopCutoffH, "\n", sep = ""), ""),
               ifelse(lgcReplicate, paste(infoStopReplicate, "\n", sep = ""), ""),
               ifelse(lgcPlot, paste(infoStopPlot, "\n", sep = ""), ""),
               ifelse(lgcSort, paste(infoStopSort, "\n", sep = ""), ""),
               ifelse(lgcColor, paste(infoStopColor, "\n", sep = ""), ""),
               sep = "")
         )


  # 04. ANALYZE
  ## 04.01. PREPARE data for testing disparities in sample size -----
  infoNumStdy          <- length(dataCases)
  infoAlpha            <- 0.05
  infoMethodOtlrOrg    <- ifelse(is.null(outlier),
                                 "Default",
                                 ifelse(outlier %in% c("Default", "IQR", "Z", "GESD", "MAD"),
                                        outlier,
                                        "Default")
                                 )
  infoMethodVrbltyOrg  <- ifelse(is.null(vrblty),
                                 "Default",
                                 ifelse(vrblty %in% c("CV", "MAD"),
                                        vrblty,
                                        "Default")
                                 )
  infoCases            <- sum(dataCases)
  infoMCases           <- mean(dataCases)
  infoSDCases          <- sd(dataCases)
  #infoCV0.1            <- infoMCases * 0.1
  #infoCV0.3            <- infoMCases * 0.3
  #infoCasesMSD3CV0.1   <- infoMCases - infoCV0.1 * 3  # minus SD 3
  #infoCasesPSD3CV0.1   <- infoMCases + infoCV0.1 * 3  # plus SD 3
  #infoCasesMSD3CV0.3   <- infoMCases - infoCV0.3 * 3  # minus SD 3
  #infoCasesPSD3CV0.3   <- infoMCases + infoCV0.3 * 3  # plus SD 3
  #infoCasesMSD3.5CV0.1 <- infoMCases - infoCV0.1 * 3.5  # minus SD 3.5
  #infoCasesPSD3.5CV0.1 <- infoMCases + infoCV0.1 * 3.5  # plus SD 3.5
  #infoCasesMSD3.5CV0.3 <- infoMCases - infoCV0.3 * 3.5  # minus SD 3.5
  #infoCasesPSD3.5CV0.3 <- infoMCases + infoCV0.3 * 3.5  # plus SD 3.5
  #infoCasesMSD4CV0.1   <- infoMCases - infoCV0.1 * 4  # minus SD 4
  #infoCasesPSD4CV0.1   <- infoMCases + infoCV0.1 * 4  # plus SD 4
  #infoCasesMSD4CV0.3   <- infoMCases - infoCV0.3 * 4  # minus SD 4
  #infoCasesPSD4CV0.3   <- infoMCases + infoCV0.3 * 4  # plus SD 4
  #infoMSDOrg           <- -infoCasesMSD3CV0.1 / infoSDCases
  #infoPSDOrg           <- infoCasesMSD3CV0.1 / infoSDCases
  #infoMSDCV0.3         <- -infoCasesMSD3CV0.1 / infoCV0.3 #(infoMCases - infoCV0.3)
  #infoPSDCV0.3         <- infoCasesMSD3CV0.1 / infoCV0.3 #(infoMCases - infoCV0.3)
  infoCutoff           <- ctf
  infoCutoffL          <- ctfLwr
  infoCutoffH          <- ctfUpr
  infoCVL              <- infoMCases * infoCutoffL
  infoCVH              <- infoMCases * infoCutoffH
  infoCasesMSD3CVL     <- infoMCases - infoCVL * 3  # minus SD 3
  infoCasesPSD3CVL     <- infoMCases + infoCVL * 3  # plus SD 3
  infoCasesMSD3CVH     <- infoMCases - infoCVH * 3  # minus SD 3
  infoCasesPSD3CVH     <- infoMCases + infoCVH * 3  # plus SD 3
  infoCasesMSD4CVL     <- infoMCases - infoCVL * 4  # minus SD 4
  infoCasesPSD4CVL     <- infoMCases + infoCVL * 4  # plus SD 4
  infoCasesMSD4CVH     <- infoMCases - infoCVH * 4  # minus SD 4
  infoCasesPSD4CVH     <- infoMCases + infoCVH * 4  # plus SD 4
  infoMSDCVL           <- -infoCasesMSD3CVL / infoSDCases
  infoPSDCVL           <- infoCasesMSD3CVL / infoSDCases
  infoMSDCVH           <- -infoCasesMSD3CVL / infoCVH #(infoMCases - infoCV0.3)
  infoPSDCVH           <- infoCasesMSD3CVL / infoCVH #(infoMCases - infoCV0.3)

  infoReplicate        <- round(rplctns, 0)
  infoPlot             <- plot
  infoSort             <- ifelse(is.null(sort), "excessive", sort)
  vctSeq               <- c(1:infoNumStdy)
  vctZVal              <- (dataCases - infoMCases) / infoSDCases
  vctCasesExpct        <- infoMCases + vctZVal * (infoSDCases / sqrt(infoNumStdy))

  dataDisparity$source       <- vctSeq
  dataDisparity$z.val        <- vctZVal
  dataDisparity$cases.expect <- ceiling(vctCasesExpct)

  dataDisparity              <- dataDisparity[, c("source", "study",
                                                  "n", "time",
                                                  "z.val", "cases.expect")]

  infoGESD                   <- ifelse(length(which(table(dataDisparity$n) > 1)) > 0,
                                       "GESD which cannot be used for disparity test due to multiple studies with eaqual sample size.",
                                       "GESD which can be used for disparity test.")
  lgcStopGESD                <- ifelse(length(which(table(dataDisparity$n) > 1)) > 0,
                                      TRUE, FALSE)

  dataOtlr                   <- dataDisparity
  dataOtlr$cases.excessive   <- 0
  dataOtlr$prop.excessive    <- 0
  infoCasesExcssv            <- 0



  ## 04.02. TEST normality -----
  outptNorm    <- shapiro.test(dataCases)
  infoPValNorm <- outptNorm$p.value

  if (infoMethodOtlrOrg == "Default") {
    if (infoPValNorm < 0.05) {
      infoMethodOtlr <- "MAD"
      #infoMethodOtlrOrg <- "MAD"
    } else if (lgcStopGESD) {

      infoMethodOtlr <- "IQR"

      } else {
        infoMethodOtlr <- "GESD"
        #infoMethodOtlrOrg <- "GESD"
      }
  } else {
    if (infoMethodOtlrOrg %in% c("IQR", "Z", "GESD", "MAD")) {

      if (infoMethodOtlrOrg == "GESD") {
        if (lgcStopGESD) {
          if (infoPValNorm < 0.05) {
            infoMethodOtlr    <- "MAD"
            infoMethodOtlrOrg <- infoGESD
          } else {
            infoMethodOtlr <- "IQR"
          }
        } else {
          infoMethodOtlr <- "GESD"
        }
      } else {
        infoMethodOtlr <- infoMethodOtlrOrg
      }

    } else {
      if (infoPValNorm < 0.05) {
        infoMethodOtlr    <- "MAD"
        infoMethodOtlrOrg <- paste(infoMethodOtlrOrg,
                                   " (unrecognized)", sep = "")
      } else {
        if (lgcStopGESD) {
          infoMethodOtlr    <- "IQR"
          infoMethodOtlrOrg <- paste(infoMethodOtlrOrg,
                                     " (unrecognized)", sep = "")
        } else {
          infoMethodOtlr    <- "GESD"
          infoMethodOtlrOrg <- paste(infoMethodOtlrOrg,
                                     " (unrecognized)", sep = "")
        }
      }
    }

    '
    if (infoPValNorm < 0.05) {
      infoMethodOtlr <- "MAD"
    } else {
      if (infoMethodOtlrOrg %in% c("IQR", "Z", "GESD")) {
        infoMethodOtlr <- infoMethodOtlrOrg
      } else {
        infoMethodOtlr <- "GESD"
      }
    }
    '
  }

  if (infoMethodVrbltyOrg == "Default") {
    infoMethodVrblty <- "CV"
  } else {
    infoMethodVrblty <- infoMethodVrbltyOrg
  }



  ## 04.03. TEST Outlier -----
  ### 04.03.01 IQR 1.5 -----
  infoQ1        <- quantile(dataCases, 0.25)
  infoQ3        <- quantile(dataCases, 0.75)
  infoIQR       <- IQR(dataCases)
  infoIQRLB     <- infoQ1 - 1.5 * infoIQR
  infoIQRUB     <- infoQ3 + 1.5 * infoIQR
  vctOutlierIQR <- ifelse(dataCases > infoIQRUB,
                          TRUE,
                          ifelse(dataCases < infoIQRLB,
                                 TRUE, FALSE
                                 )
                          )

  dataIQR           <- data.frame(vctSeq, dataCases, vctOutlierIQR)
  colnames(dataIQR) <- c("source", "cases", "outlier")
  dataIQR           <- dataIQR[order(dataIQR$source, decreasing = FALSE), ]

  dataDisparity$outlier.IQR         <- FALSE

  for (study.i in vctSeq) {
    if (study.i %in% dataIQR$source) {
      dataDisparity[study.i, "outlier.IQR"] <- dataIQR[dataIQR$source == study.i, "outlier"]
    }
  }

  dataDisparity$cases.excessive.IQR <- ifelse(dataDisparity$outlier.IQR == TRUE,
                                              dataDisparity$n - dataDisparity$cases.expect,
                                              0)
  infoCasesExcssvIQR                <- sum(abs(dataDisparity$cases.excessive.IQR))
  dataDisparity$prop.excessive.IQR  <- ifelse(dataDisparity$outlier.IQR == TRUE,
                                              dataDisparity$cases.excessive.IQR / infoCases,
                                              0)

  if (infoMethodOtlr == "IQR" | lgcStopGESD) {
    infoOutliers             <- sum(vctOutlierIQR == TRUE)
    infoCasesExcssv          <- infoCasesExcssvIQR
    dataOtlr$cases.excessive <- dataDisparity$cases.excessive.IQR
    dataOtlr$prop.excessive  <- dataDisparity$prop.excessive.IQR
  }


  ### 04.03.02 z-score threshold -----
  infoZValCOV  <- 3.29
  vctOutlierZ  <- ifelse(abs(scale(dataCases)) > 3.29,
                          TRUE, FALSE)

  dataZ           <- data.frame(vctSeq, dataCases, vctOutlierZ)
  colnames(dataZ) <- c("source", "cases", "outlier")
  dataZ           <- dataZ[order(dataZ$source, decreasing = FALSE), ]

  dataDisparity$outlier.Z <- FALSE

  for (study.i in vctSeq) {
    if (study.i %in% dataZ$source) {
      dataDisparity[study.i, "outlier.Z"] <- dataIQR[dataZ$source == study.i, "outlier"]
    }
  }

  dataDisparity$cases.excessive.Z <- ifelse(dataDisparity$outlier.Z == TRUE,
                                              dataDisparity$n - dataDisparity$cases.expect,
                                              0)
  infoCasesExcssvZ                <- sum(abs(dataDisparity$cases.excessive.Z))
  dataDisparity$prop.excessive.Z  <- ifelse(dataDisparity$outlier.Z == TRUE,
                                              dataDisparity$cases.excessive.Z / infoCases,
                                              0)

  if (infoMethodOtlr == "Z") {
    infoOutliers             <- sum(vctOutlierZ == TRUE)
    infoCasesExcssv          <- infoCasesExcssvZ
    dataOtlr$cases.excessive <- dataDisparity$cases.excessive.Z
    dataOtlr$prop.excessive  <- dataDisparity$prop.excessive.Z
  }



  ### 04.03.03 Generalized extreme Studentized deviate (GESD) -----
  ### Rosner, Bernard (May 1983), Percentage Points for a Generalized ESD Many-Outlier Procedure,Technometrics, 25(2), pp. 165-172.
  ### https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h3.htm

  if (lgcStopGESD) {

    dataDisparity$cases.excessive.GESD <- NA
    dataDisparity$prop.excessive.GESD  <- NA

  } else {
    vctLamda  <- c(1:ceiling(infoNumStdy / 2))
    vctRepeat <- c(1:ceiling(infoNumStdy / 2))
    vctSeq    <- c(1:ceiling(infoNumStdy / 2))
    vctOutlr  <- c(1:ceiling(infoNumStdy / 2))

    DoGESD <- function(vctGESD) {
      vctARES      <- abs(vctGESD - mean(vctGESD)) / sd(vctGESD)
      dataTempGESD <- data.frame(vctGESD, vctARES)
      infoRepeat   <- max(dataTempGESD$vctARES)
      list(infoRepeat, dataTempGESD)
    }

    for (i.rept in vctRepeat) {

      if (i.rept == 1) {
        vctGESD1          <- DoGESD(dataCases)
        dataTempGESD      <- data.frame(vctGESD1[2])
        vctRepeat[i.rept] <- unlist(vctGESD1[1])
        vctOutlr[i.rept]  <- dataTempGESD[which(dataTempGESD$vctARES == vctRepeat[1]), "vctGESD"]
        vctSeq[i.rept]    <- which(dataTempGESD$vctARES == vctRepeat[i.rept])
        dataGESD          <- dataTempGESD[dataTempGESD$vctARES != max(dataTempGESD$vctARES), ]
      } else if (i.rept != 1) {
        vctGESD1          <- DoGESD(dataGESD$vctGESD)
        dataTempGESD      <- as.data.frame(vctGESD1[2])
        vctRepeat[i.rept] <- unlist(vctGESD1[1])
        vctOutlr[i.rept]  <- dataTempGESD[which(dataTempGESD$vctARES == vctRepeat[i.rept]), "vctGESD"]
        vctSeq[i.rept]    <- which(dataCases == vctOutlr[i.rept])
        dataGESD          <- dataTempGESD[dataTempGESD$vctARES != max(dataTempGESD$vctARES), ]
      }

      ### Compute critical value.
      infoPval         <- 1 - infoAlpha / (2 * (infoNumStdy - i.rept + 1))
      infoTval         <- qt(infoPval, (infoNumStdy - i.rept - 1))
      vctLamda[i.rept] <- infoTval * (infoNumStdy - i.rept) / sqrt((infoNumStdy - i.rept - 1 + infoTval**2) * (infoNumStdy - i.rept + 1))
    }

    dataGESD         <- data.frame(vctSeq, vctOutlr, vctRepeat, vctLamda)
    names(dataGESD)  <- c("source", "cases", "statistics", "threshold") # threshold is critical value
    dataGESD$diff    <- dataGESD$statistics - dataGESD$threshold
    dataGESD$outlier <- ifelse(dataGESD$diff > 0,
                               TRUE, FALSE)
    dataGESD         <- dataGESD[order(dataGESD$source, decreasing = FALSE), ]

    dataDisparity$outlier.GESD <- FALSE

    for (study.i in vctSeq) {
      if (study.i %in% dataGESD$source) {
        dataDisparity[study.i, "outlier.GESD"] <- dataGESD[dataGESD$source == study.i, "outlier"]
      }
    }

    dataDisparity$cases.excessive.GESD <- ifelse(dataDisparity$outlier.GESD == TRUE,
                                                 dataDisparity$n - dataDisparity$cases.expect,
                                                 0)
    infoCasesExcssvGESD                <- sum(abs(dataDisparity$cases.excessive.GESD))
    dataDisparity$prop.excessive.GESD  <- ifelse(dataDisparity$outlier.GESD == TRUE,
                                                 dataDisparity$cases.excessive.GESD / infoCases,
                                                 0)

    if (infoMethodOtlr == "GESD") {
      infoOutliers             <- sum(dataGESD$outlier == TRUE)
      infoCasesExcssv          <- infoCasesExcssvGESD
      dataOtlr$cases.excessive <- dataDisparity$cases.excessive.GESD
      dataOtlr$prop.excessive  <- dataDisparity$prop.excessive.GESD
    }

  }




  ### 04.03.04 Median absolute deviation (MAD) -----
  ### Peter J. Rousseeuw & Christophe Croux (1993) Alternatives to the Median Absolute Deviation, Journal of the American Statistical Association, 88:424, 1273-1283. http://dx.doi.org/10.1080/01621459.1993.10476408
  ### Leys, C., Ley, C., Klein, O., Bernard, P., & Licata, L. (2013). Detecting outliers: Do not use standard deviation around the mean, use absolute deviation around the median. Journal of experimental social psychology, 49(4), 764-766.
  infoMedianOrg   <- median(dataCases)
  infoB           <- 1 / qnorm(0.75)
  vctDiffMedian   <- dataCases - infoMedianOrg
  vctMAD          <- abs(vctDiffMedian)
  infoMADStdz     <- infoB * median(vctMAD)
  infoC           <- 2.5 # conservative (Leys et al. 2013)
  #vctOutlierVal  <- abs(vctMAD - infoMedianOrg) / infoMADStdz
  vctOutlierVal   <- abs(dataCases - infoMedianOrg) / infoMADStdz
  vctOutlierMAD   <- ifelse(vctOutlierVal > infoC, TRUE, FALSE)
  dataOutlierMAD  <- data.frame(source = c(1:infoNumStdy),
                                cases = dataCases,
                                distance = vctDiffMedian,
                                mad = vctMAD,
                                critical = vctOutlierVal,
                                outlier = vctOutlierMAD)
  dataMAD         <- dataOutlierMAD
  infoCutCasesMAD <- c(-infoC * infoMADStdz + infoMedianOrg,
                       infoC * infoMADStdz + infoMedianOrg)

  dataDisparity$outlier.MAD <- FALSE

  for (study.i in vctSeq) {
    if (study.i %in% dataMAD$source) {
      dataDisparity[study.i, "outlier.MAD"] <- dataMAD[dataMAD$source == study.i, "outlier"]
    }
  }

  dataDisparity$cases.excessive.MAD <- ceiling(ifelse(dataDisparity$n < infoCutCasesMAD[1],
                                                      dataDisparity$n - infoCutCasesMAD[1],
                                                      ifelse(dataDisparity$n > infoCutCasesMAD[2],
                                                             dataDisparity$n - infoCutCasesMAD[2],
                                                             0)
                                                      )
                                               )
  infoCasesExcssvMAD                <- sum(abs(dataDisparity$cases.excessive.MAD))
  dataDisparity$prop.excessive.MAD  <- ifelse(dataDisparity$outlier.MAD == TRUE,
                                               dataDisparity$cases.excessive.MAD / infoCases,
                                               0)

  if (infoMethodOtlr == "MAD") {
    infoOutliers             <- sum(vctOutlierMAD == TRUE)
    infoCasesExcssv          <- infoCasesExcssvMAD
    dataOtlr$cases.excessive <- dataDisparity$cases.excessive.MAD
    dataOtlr$prop.excessive  <- dataDisparity$prop.excessive.MAD
  }



  ## 04.04. TEST significance of outlier -----
  ### Binomial test for detection the impact of outliers
  #### 5% loss to follow-up would be impact outcome
  #### Haynes, R. B., Sackett, D. L., Richardson, W. S., Rosenberg, W., & Langley, G. R. (1997). Evidence-based medicine: How to practice & teach EBM. Canadian Medical Association. Journal, 157(6), 788.
  #### 20% loss to follow-up would be impact outcome
  #### Kristman, V., Manno, M., & Côté, P. (2004). Loss to follow-up in cohort studies: how much is too much?. European journal of epidemiology, 19, 751-760.
  #### Kristman, V. L., Manno, M., & Côté, P. (2005). Methods to account for attrition in longitudinal data: do they work? A simulation study. European journal of epidemiology, 20, 657-662.

  rsltOtlrProp <- binom.test(x = infoCasesExcssv, # infoCasesOutlierIQR,
                             n = infoCases,
                             p = infoCutoff, # infoPropExpctIQR
                             alternative = "greater"
                             )

    infoOtlrProp        <- rsltOtlrProp$estimate
    infoOtlrExcssvCases <- rsltOtlrProp$statistic
    infoOtlrPval        <- rsltOtlrProp$p.value
    infoOtlrLCI         <- rsltOtlrProp$conf.int[1][1]
    infoOtlrUCI         <- rsltOtlrProp$conf.int[1][2]

    lsRsltOtlirProp     <- list(prop.outlier = infoOtlrProp,
                                cases.excssv = infoOtlrExcssvCases,
                                p.val = infoOtlrPval,
                                ci.lower = infoOtlrLCI,
                                ci.upper = infoOtlrLCI)



  ## 04.05. TEST variability -----
  ### 04.05.01 Coefficient of variation (normal distribution) -----

  infoOrgnCV     <- infoSDCases / infoMCases  # original coefficient of variance
  infoUnbsCV     <- infoOrgnCV * (1 + 1 / (4 * infoNumStdy))  # unbiased coefficient of variance
  infoSECV       <- infoUnbsCV / sqrt(2 * infoNumStdy) * (sqrt((1 + 2) * (infoUnbsCV / 100)^2))
  infoDiffCV     <- infoOrgnCV - infoCutoffH
  infoStatsT     <- infoDiffCV / infoSECV
  infoDFCV       <- infoNumStdy - 1
  infoCrtTCV     <- qt(infoAlpha, infoNumStdy)
  infoPValCV     <- pt(infoStatsT,
                        df = infoDFCV,
                        lower.tail = FALSE)
  infoLCICV      <- infoDiffCV - infoSECV * infoCrtTCV
  infoUCICV      <- infoDiffCV + infoSECV * infoCrtTCV
  vctCutCasesCV  <- c(floor(infoMCases * (1 - infoCutoffH)), ceiling(infoMCases * (1 + infoCutoffH)))
  vctExcCasesCV  <- ifelse(dataCases < vctCutCasesCV[1],
                            dataCases - vctCutCasesCV[1],
                            ifelse(dataCases > vctCutCasesCV[2],
                                   dataCases - vctCutCasesCV[2],
                                   0)
                            )

  dataDisparity$study.excessive.CV <- ifelse(dataDisparity$n < vctCutCasesCV[1],
                                             TRUE,
                                             ifelse(dataDisparity$n > vctCutCasesCV[2],
                                                    TRUE,
                                                    FALSE)
                                             )

  dataDisparity$cases.excessive.CV <- ceiling(ifelse(dataDisparity$n < vctCutCasesCV[1],
                                                     dataDisparity$n - vctCutCasesCV[1],
                                                     ifelse(dataDisparity$n > vctCutCasesCV[2],
                                                            dataDisparity$n - vctCutCasesCV[2],
                                                            0
                                                            )
                                                     )
                                              )
  infoCasesExcssvCV                <- sum(abs(dataDisparity$cases.excessive.CV))
  dataDisparity$prop.excessive.CV  <- ifelse(dataDisparity$study.excessive.CV == TRUE,
                                              dataDisparity$cases.excessive.CV / infoCasesExcssvCV,
                                              0)


  ### 04.05.02 Robust coefficient of variation (RCV, non-normal distribution) -----
  ### RCV based on MAD
  ### Arachchige CNPG, Prendergast LA, Staudte RG. Robust analogs to the coefficient of variation. J Appl Stat. 2020 Aug 20;49(2):268-290. doi: 10.1080/02664763.2020.1808599
  ### based on returns from 04.03.04 Median absolute deviation (MAD)

  infoMAD           <- median(vctMAD)
  infoRCVMAD        <- infoB * (infoMAD / infoMedianOrg)
  vctCutCasesRCVMAD <- c(floor(infoMCases * (1 - infoRCVMAD)),
                         ceiling(infoMCases * (1 + infoRCVMAD)))
  vctExcCasesRCVMAD <- ifelse(dataCases < vctCutCasesRCVMAD[1],
                              vctCutCasesRCVMAD[1] - dataCases,
                              ifelse(dataCases < vctCutCasesRCVMAD[2],
                                     dataCases - vctCutCasesRCVMAD[2],
                                     0)
                              )

  TestRCVMAD <- function(vctRCVMed1, i.boot) {
    vctRCVMed2         <- vctRCVMed1[i.boot]
    infoMedRCVBoot     <- median(vctRCVMed2)
    infoB              <- 1 / qnorm(0.75)
    vctDiffMedRCVBoot  <- vctRCVMed2 - infoMedRCVBoot
    vctMADRCVBoot      <- abs(vctDiffMedRCVBoot)
    #infoMADStdzRCVBoot <- infoB * median(vctMADRCVBoot)
    infoMADRCVBoot     <- median(vctMADRCVBoot)
    infoRCVMAD         <- infoB * (infoMADRCVBoot / infoMedRCVBoot)
    return(infoRCVMAD)
  }

  set.seed(101020)
  rsltRCVMADBoot    <- boot::boot(dataCases, TestRCVMAD, R = 1000)
  vctCIRCVMADBoot   <- boot::boot.ci(boot.out = rsltRCVMADBoot, type = c("norm", "basic", "perc", "bca"))
  infoLCIRCVMADBoot <- vctCIRCVMADBoot$bca[4]
  infoUCIRCVMADBoot <- vctCIRCVMADBoot$bca[5]

  #### P-value of bootstrap: pval = (1 + sum(s >= s0))/(N+1)
  #### Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (No. 1). Cambridge university press. P. 114
  infoPValRCVMADBoot <- (1 + sum(infoCutoffH >= rsltRCVMADBoot$t)) / (1000 + 1)



  # 05. BUILD a data frame of the disparity -----
  # 06. BUILD an disparity object -----
  ### 06.01 CREATE core list of disparity object -----

  #infoDisparaty <- ifelse(infoOtlrPval < 0.05,
  #                        "Suspected",
  #                        ifelse(infoPValCV < 0.05,
  #                               "Suspected",
  #                                "Undetected"
  #                               )
  #                        )

  infoDisparaty <- ifelse(infoOtlrPval < 0.05,
                          "Suspected",
                          ifelse(infoMethodVrblty == "CV",
                                 ifelse(infoPValCV < 0.05,
                                        "Suspected",
                                        "Undetected"),
                                 ifelse(infoPValRCVMADBoot < 0.05,
                                        "Suspected",
                                        "Undetected"))
                          )

  dataDiSS <- list(name      = "Disparities in sample size test",
                   disparity = infoDisparaty
                   )
  class(dataDiSS)             <- "disparity"
  dataDiSS$w.normality        <- outptNorm$statistic
  dataDiSS$p.normality        <- outptNorm$p.value
  dataDiSS$outlier.method.set <- infoMethodOtlrOrg
  dataDiSS$outlier.method     <- infoMethodOtlr
  dataDiSS$vrblty.method.set  <- infoMethodVrbltyOrg
  dataDiSS$vrblty.method      <- infoMethodVrblty
  dataDiSS$outlier            <- dataOtlr[dataOtlr$prop.excessive > 0, ] #dataOutlier
  dataDiSS$prop.outlier       <- infoOtlrProp
  dataDiSS$n.excessive        <- infoOtlrExcssvCases
  dataDiSS$p.prop.outlier     <- infoOtlrPval
  dataDiSS$lci.prop.outlier   <- infoOtlrLCI
  dataDiSS$uci.prop.outlier   <- infoOtlrUCI
  dataDiSS$ctf.lwr.cv         <- infoCutoffL
  dataDiSS$ctf.upr.cv         <- infoCutoffH
  dataDiSS$variability        <- ifelse(infoMethodVrblty == "CV",
                                        infoUnbsCV,
                                        ifelse(infoMethodVrblty == "MAD",
                                               infoRCVMAD,
                                               "???"))
  dataDiSS$p.variability      <- ifelse(infoMethodVrblty == "CV",
                                        infoPValCV,
                                        ifelse(infoMethodVrblty == "MAD",
                                               infoPValRCVMADBoot,
                                               "???"))
  dataDiSS$lci.variability    <- ifelse(infoMethodVrblty == "CV",
                                        infoLCICV,
                                        ifelse(infoMethodVrblty == "MAD",
                                               infoLCIRCVMADBoot,
                                               "???"))
  dataDiSS$uci.variability    <- ifelse(infoMethodVrblty == "CV",
                                        infoUCICV,
                                        ifelse(infoMethodVrblty == "MAD",
                                               infoUCIRCVMADBoot,
                                               "???"))
  dataDiSS$cv                 <- infoOrgnCV
  dataDiSS$cv.unbiased        <- infoUnbsCV
  dataDiSS$diff.cv            <- infoDiffCV
  dataDiSS$se.cv              <- infoSECV
  dataDiSS$crt.t.cv           <- infoCrtTCV
  dataDiSS$t.cv               <- infoStatsT #ifelse(outptNorm$p.value <= 0.05, infoStatsT, NA)
  dataDiSS$p.cv               <- infoPValCV # ifelse(outptNorm$p.value <= 0.05, infoPValCV, NA)
  dataDiSS$lci.cv             <- infoLCICV
  dataDiSS$uci.cv             <- infoUCICV
  dataDiSS$cov.cases          <- vctCutCasesCV
  dataDiSS$cv.robust.MAD      <- infoRCVMAD
  dataDiSS$boot.robust.MAD    <- rsltRCVMADBoot$t
  dataDiSS$p.cv.robust.MAD    <- infoPValRCVMADBoot # ifelse(infoPValRCVMADBoot <= 0.05, infoPValRCVMADBoot, NA)
  dataDiSS$lci.cv.robust.MAD  <- infoLCIRCVMADBoot
  dataDiSS$uci.cv.robust.MAD  <- infoUCIRCVMADBoot
  dataDiSS$n.excessive        <- sum(abs(dataOtlr$cases.excessive))
  dataDiSS$prop.excessive     <- sum(abs(dataOtlr$cases.excessive)) / infoCases
  #dataDiSS$data.plot    <- dataCasesPlot
  dataDiSS$sample.size        <- dataCases



  ### 06.02 LIST data in a frame for illustrating plot of disparity test -----

  if (infoPlot) {
    dataDiSS$data.disparity <- dataDisparity
  } else if (base::isFALSE(is.null(vrblty))) {
    dataDiSS$data.disparity <- dataDisparity
  }


  # 07. RETURN summary of function `TestDisparity()` -----
  #cat(paste("\n"), fill = TRUE, sep = "")

  cat(paste("Summary of disparities in sample size test:\n",
            "Number of outliers = ", infoOutliers,
            " (Excessive cases = ", round(infoOtlrExcssvCases, 3),
            "; P-value",
            ifelse(infoOtlrPval < 0.001,
                   " < 0.001",
                   paste(" = ",
                         round(infoOtlrPval, 3),
                         sep = ""
                         )
                   ),
            ")\n",
            "Variability = ", round(dataDiSS$variability, 3),
            " (P-value",
            ifelse(dataDiSS$p.variability < 0.001,
                   " < 0.001",
                   paste(" = ",
                         round(dataDiSS$p.variability, 3),
                         sep = ""
                         )
                   ),
            ")\n",
            "\n",
            paste("Outlier detection method: ", infoMethodOtlr,
                  ifelse(infoMethodOtlr != infoMethodOtlrOrg,
                         ifelse(infoMethodOtlrOrg == "Default",
                                "",
                                paste("\nPre-defined method: ", infoMethodOtlrOrg,
                                      "\n(Outlier detection method was automatically changed according to data distribution.)",
                                      sep = "")
                         ),
                         ""),
                  sep = ""),
            "\n",
            paste("Variability detection method: ", infoMethodVrblty,
                  ifelse(infoMethodVrblty != infoMethodVrbltyOrg,
                         ifelse(infoMethodVrbltyOrg == "Default",
                                "",
                                paste("\nPre-defined method: ", infoMethodVrbltyOrg,
                                      "\n(variability detection method was automatically changed according to data distribution.)",
                                      sep = "")
                         ),
                         ""),
                  sep = ""),
            "\n",        #    "\nNote: ",
        #    ifelse(infoDisparaty == "Suspected",
        #           paste("Suspected disparities in sample size due to ",
        #                 ifelse(infoOtlrPval < 0.05,
        #                        ifelse(infoPValCV < 0.05,
        #                               "outlier(s) and variability.",
        #                               "outlier(s)."),
        #                        ifelse(infoPValCV < 0.05,
        #                               "variability.",
        #                               "???")),
        #                 sep = ""),
        #           "No significant finding in the stepwised tests of disparities in sample size."
        #           ),
            sep = ""
            ),
      fill = TRUE, sep = ""
      )



  # 08. ILLUSTRATE proportion of excessive cases plot -----

  if (plot == TRUE) {

    if (infoSort == "time") {
      dataPlot <- dataOtlr[order(dataOtlr$time), ]
    } else if (infoSort == "size") {
      dataPlot <- dataOtlr[order(dataOtlr$n), ]
    } else {
      dataPlot <- dataOtlr[order(dataOtlr$prop.excessive), ]
    }

    dataPlot$source <- c(1:nrow(dataPlot))

    dataPlot$method <- infoMethodOtlr

    dataPlot$color  <- ifelse(dataPlot$prop.excessive == 0,
                              "skyblue",
                              color
                              )
    dataPlot$pos    <- ifelse(dataPlot$prop.excessive < 0, 3, 1)


    ### Disparity plot (Outlier)

    plot(dataPlot$source,
         dataPlot$prop.excessive,
         type = "n", frame = FALSE,
         xaxt = "n", yaxt = "n",
         ylim = c(ifelse(min(dataPlot$prop.excessive) > -0.5,
                         -0.5,
                         ifelse(min(dataPlot$prop.excessive) > -1,
                                -1.2,
                                min(dataPlot$prop.excessive) * 1.1
                                )
                         ),
                  ifelse(max(dataPlot$prop.excessive) < 0.5,
                         0.5,
                         ifelse(max(dataPlot$prop.excessive) < 1,
                                1.2,
                                max(dataPlot$prop.excessive) * 1.1
                                )
                         )
                  ),
         xlab = "",
         ylab = "")
    segments(0, 0,
             nrow(dataPlot), 0,
             col = "blue4")
    segments(dataPlot$source, 0,
             dataPlot$source, dataPlot$prop.excessive,
             col = "gray")
    points(dataPlot$source,
           dataPlot$prop.excessive,
           pch = 21, bg = dataPlot$color, col = "gray")

    ### TEXT legend
    text(1,
         par("usr")[4] * 0.6,
         paste("Disparities in sample size test (outlier detection based on ",
               infoMethodOtlr,
               "):\n",
               "Number of outliers = ", infoOutliers,
               " (Excessive cases = ", round(infoOtlrExcssvCases, 3),
               "; P-value",
               ifelse(infoOtlrPval < 0.001,
                      " < 0.001",
                      paste(" = ",
                            round(infoOtlrPval, 3),
                            sep = ""
                      )
               ),
               ")\n",
             #  "Variability",
            #   ifelse(dataDiSS$variability < 0.001,
            #          " < 0.001",
            #          paste(" = ",
            #                round(dataDiSS$variability, 3),
            #                sep = "")
            #  ),
            #   " (P-value",
            #   ifelse(dataDiSS$p.variability < 0.001,
            #          " < 0.001",
            #          paste(" = ", round(dataDiSS$p.variability, 3),
            #                sep = "")
            #   ),
            #   ")\n",
               sep = ""
         ),
         pos = 4, cex = 1.2)
    axis(2, las = 2)
    text(dataPlot$source,
         par("usr")[3],
         dataPlot$study,
         cex = ifelse(infoNumStdy < 11, 1,
                      1 / sqrt(infoNumStdy / 10)),
         xpd = TRUE, pos = 1, srt = 45)
    mtext("Disparity plot (outlier)", side = 3, cex = 2)
    mtext("Study", side = 1, cex = 1.2, line = 4)
    mtext("Proportion of excessive cases", side = 2, cex = 1.2, line = 3)


    ### Disparity plot (variability)
    '
    plot(infoMCases + infoCVL * c(0:4), #infoMCases + infoCV0.1 * c(0:4),
         c(0:4),
         type = "n", frame = FALSE,
         ylim = c(0, 3),
         xaxt = "n",
         yaxt = "n",
         xlab = "", ylab = "")
    rect(infoMCases,
         0,
         infoCasesPSD4CVL, #infoCasesPSD4CV0.1,
         3,
         col = "khaki1",
         lty = 0)

    polygon(c(infoMCases, infoMCases, infoCasesPSD3CVL), #infoCasesPSD3CV0.1
            c(0, 3, 3),
            col = "darkseagreen2", lty = 0)

    polygon(c(infoMCases,
              infoCasesPSD4CVL,  #infoCasesPSD4CV0.1
              infoCasesPSD4CVL), #infoCasesPSD4CV0.1
            c(0, 0, infoPSDCVH), #infoPSDCV0.3
            lty = 0,
            col = "lightpink1")

    rect(infoMCases, 3,
         infoCasesPSD4CVL, 4,
         lty = 0,
         col = "white")

    text(infoMCases + (infoCasesPSD4CVL - infoMCases) * 0.29, #infoCasesPSD4CV0.1
         c(1.5),
         c("Low variability zone"),
         pos = c(4),
         srt = c(33),
         cex = 0.8,
         col = "darkseagreen4")

    text(infoMCases + (infoCasesPSD4CVL - infoMCases) * 0.3, #infoCasesPSD4CV0.1
         c(1),
         c("Moderate variability zone"),
         pos = c(4),
         srt = c(26),
         cex = 0.8,
         col = "khaki4")

    text(infoMCases + (infoCasesPSD4CVL - infoMCases) * 0.3, #infoCasesPSD4CV0.1
         c(0.5),
         c("High variability zone"),
         pos = c(4),
         srt = c(20),
         cex = 0.8,
         col = "lightpink4")

    segments(infoCasesMSD4CVL, #infoCasesMSD4CV0.1,
             infoMSDCVL,#infoMSDOrg,
             infoCasesPSD4CVL, #infoCasesPSD4CV0.1,
             infoPSDCVL,#infoPSDOrg,
             lty = 1,
             lwd = 2.5,
             col = ifelse(infoSDCases > infoCVH, #infoCV0.3
                          "firebrick4",
                          "navyblue")
             )

    axis(1, las = 1)
    axis(2, at = c(0, 1, 2, 3), las = 2)

    ### TEXT legend
    text(infoMCases + (infoCasesPSD3CVL - infoMCases) * 0.05, #infoCasesPSD3CV0.1
         par("usr")[4] * 0.8,
         paste("Disparities in sample size test (outlier detection based on ",
               infoMethodOtlr,
               "):\n",
               "Number of outliers = ", infoOutliers,
               " (Excessive cases = ", round(infoOtlrExcssvCases, 3),
               "; P-value",
               ifelse(infoOtlrPval < 0.001,
                      " < 0.001",
                      paste(" = ",
                            round(infoOtlrPval, 3),
                            sep = "")
               ),
               ")\n",
               "Variability",
               ifelse(infoUnbsCV < 0.001,
                      " < 0.001",
                      paste(" = ",
                            round(infoUnbsCV, 3),
                            sep = "")
               ),
               " (mean cases = ", ceiling(infoMCases),
               "; SD = ", round(infoSDCases, 3),
               "; t-value",
               ifelse(infoStatsT < 0.001,
                      " < 0.001",
                      paste(" = ", round(infoStatsT, 3),
                            sep = "")
               ),
               "; P-value",
               ifelse(infoPValCV < 0.001,
                      " < 0.001",
                      paste(" = ",
                            round(infoPValCV, 3),
                            sep = "")
               ),
               ")\n",
               sep = ""
               ),
         pos = 4,
         cex = 0.8)

    mtext("Disparity plot (variability)",
          side = 3, cex = 2)
    mtext("Sample size",
          side = 1, line = 3, cex = 1.2)
    mtext("Number of tandrd deviations",
          side = 2, line = 3, cex = 1.2)
    '

  }

  output <- dataDiSS

}
