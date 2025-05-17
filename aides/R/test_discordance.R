#' @title Test assumption of discordance between theoretical and observed study scale.
#'
#' @author Enoch Kang
#'
#' @description
#' **TestDiscordance()** is a function for discordance in rank of study size analysis.
#'
#' @param data   DATA FRAME consists of three columns for study label, sample
#'               size, and standard error.
#' @param n      NUMERIC values for sample size (n) of each study.
#' @param se     NUMERIC values for standard error of each study.
#' @param study  CHARACTER for study label of each study.
#' @param method CHARACTER of "rank" or "prop" for indicating which method should
#'               be used.
#' @param coval  NUMERIC value of cutoff point ranged from 0 to 1 in order to
#'               detecting of discordance between theoretical and observed study
#'               scale.
#' @param tot    NUMERIC value of tolerate discordance in ranks between theoretical
#'               and observed study scale. The numeric value should be ranged
#'               from 0 to 1 / 4 number of studies.
#' @param plot   LOGIC value for indicating whether to illustrate discordance plot.
#' @param color  CHARACTER of a color name for emphasizing the studies with
#'               discordance in ranks between theoretical and observed study size.
#'
#'
#' @return
#' **TestDiscordance()** returns a summary of result of discordance in rank of study size.
#'
#'
#' @references
#' Howell, D. C. (2012). **Statistical methods for psychology (7th ed.)**.
#' Belmont, CA: Thomson. Available online:
#' https://labs.la.utexas.edu/gilden/files/2016/05/Statistics-Text.pdf.
#'
#'
#' @seealso \code{\link{TestDisparity}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993bin")
#' data <- Fleiss1993bin
#'
#' # 2. Calculate total sample size and standard error of each study
#' data$n  <- data$n.asp + data$n.plac
#' data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))
#'
#' # 3. Test discordance in ranks between theoretical and observed study size.
#' output <- TestDiscordance(n = n, se = se, study = study, data = data)
#'
#' # 4. Illustrate discordance plot
#' TestDiscordance(n = n, se = se, study = study, data = data, plot = TRUE)
#'
#' ## End(Not run)
#'
#' @export TestDiscordance



TestDiscordance <- function(n,
                            se,
                            data,
                            study  = NULL,
                            method = "prop",
                            coval  = 0.2,
                            tot    = 0,
                            plot   = FALSE,
                            color  = "lightpink") {

  # 01. DEFINE data -----
  if (is.null(data)) {
    dataCases   <- n
    dataSE      <- se

    if (base::isFALSE(is.null(study))) {
      dataStudy <- study
    } else {
      dataStudy <- paste(rep("Study ", length(n)),
                         c(1:length(n)),
                         sep = "")
    }

  } else {
    n     <- deparse(substitute(n))
    se    <- deparse(substitute(se))
    study <- deparse(substitute(study))

    dataCases <- data[, n]
    dataSE    <- data[, se]
    dataStudy <- data[, study]

  }

  infoNumStdy <- length(dataCases)
  infoMethod  <- method
  infoCOVal   <- coval
  infoTot     <- ifelse(tot == 999,
                        ifelse(infoNumStdy < 11, 1,
                               floor(infoNumStdy / 10)
                               ),
                        tot)

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))
  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))



  # 02. CHECK arguments -----
  lgcData   <- ifelse(is.null(data),
                      FALSE,
                      ifelse(base::isFALSE(length(data) >= 3),
                             TRUE, FALSE)
                      )

  lgcN      <- ifelse(is.null(dataCases),
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

  lgcSE     <- ifelse(is.null(dataSE),
                      TRUE,
                      ifelse(length(dataSE) != length(dataCases),
                             TRUE,
                             ifelse(base::isFALSE(is.numeric(dataSE)),
                                    TRUE,
                                    ifelse(min(dataCases) < 0,
                                           TRUE, FALSE)
                                    )
                             )
                      )

  lgcStudy  <- ifelse(is.null(dataStudy), FALSE,
                      ifelse(length(dataStudy) == length(dataCases),
                             FALSE, TRUE)
                      )

  lgcMethod <- ifelse(is.null(method), FALSE,
                      ifelse(method %in% c("rank", "prop"),
                             FALSE, TRUE)
                      )

  lgcCOVal  <- ifelse(is.null(coval), FALSE,
                      ifelse(is.numeric(coval),
                             ifelse((coval >= 0) == TRUE,
                                    ifelse((coval <= 1) == TRUE,
                                           FALSE, TRUE),
                                    TRUE),
                             TRUE)
                      )

  lgcTotVal <- ifelse(is.null(tot),
                      TRUE,
                      ifelse(base::isFALSE(is.numeric(tot)),
                             TRUE,
                             ifelse(base::isFALSE(tot >= 0),
                                    TRUE,
                                    ifelse(tot %% 1 == 0,
                                           FALSE, TRUE)
                                    )
                             )
                      )

  lgcPlot   <- ifelse(is.null(plot), FALSE,
                      ifelse(is.logical(plot), FALSE, TRUE)
                      )

  lgcColor  <- ifelse(is.null(color), FALSE,
                      ifelse(length(color) == 1,
                             ifelse(color %in% colors(), FALSE, TRUE),
                             TRUE)
                      )

  if (lgcData) {
    infoStopData  <- 'Argument "data" must be a data frame consisting of three columns for study label, sample size, and standard error.'
  }

  if (lgcN) {
    infoStopN  <- 'Argument "n" must be a integer vector for sample size of each study.'
  }

  if (lgcSE) {
    infoStopSE <- 'Argument "se" must be a numeric vector for standard error of each study, and length of the vector should be the same with length of the vector for sample size.'
  }

  if (lgcStudy) {
    infoStopStudy <- 'Argument "study" must be a vector for study label of each study, and length of the vector should be the same with length of the vector for sample size.'
  }

  if (lgcMethod) {
    infoStopMethod <- 'Argument "method" must be "rank" or "prop" for indicating which method should be used.'
  }

  if (lgcCOVal) {
    infoStopCOVal <- 'Argument "coval" must be a numeric value between 0 and 1 for a cutting-off value in overall discordance test'
  }

  if (lgcTotVal) {
    infoStopTotVal <- 'Argument "tot" must be a numeric value between 0 and 1 / 4 number of studies in order to avoid over-sensitive result by giving a tolerate difference in rank of each study between theoretical and observed study size.'
  }

  if (lgcPlot) {
    infoStopPlot  <- 'Argument "plot" must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to illustrate discordance plot.'
  }

  if (lgcColor) {
    infoStopColor <- 'Argument "color" must be a color name in R for emphasizing the studies with discordance in ranks between theoretical and observed study size.'
  }

  if (lgcData | lgcN | lgcSE | lgcStudy | lgcMethod | lgcCOVal | lgcTotVal | lgcPlot | lgcColor)
    stop(paste(ifelse(lgcData, paste(infoStopData, "\n", sep = ""), ""),
               ifelse(lgcN, paste(infoStopN, "\n", sep = ""), ""),
               ifelse(lgcSE, paste(infoStopSE, "\n", sep = ""), ""),
               ifelse(lgcStudy, paste(infoStopStudy, "\n", sep = ""), ""),
               ifelse(lgcMethod, paste(infoStopMethod, "\n", sep = ""), ""),
               ifelse(lgcCOVal, paste(infoStopCOVal, "\n", sep = ""), ""),
               ifelse(lgcTotVal, paste(infoStopTotVal, "\n", sep = ""), ""),
               ifelse(lgcPlot, paste(infoStopPlot, "\n", sep = ""), ""),
               ifelse(lgcColor, paste(infoStopColor, "\n", sep = ""), ""),
               sep = "")
         )



  # 03. PREPARE data before discordance test -----
  dataRankCases       <- rank(dataCases)
  dataRankSE          <- rank(dataSE)
  dataRankSEInvrs     <- rank(1 / dataSE)
  dataDiscordanceProp <- ifelse(abs(dataRankCases - dataRankSEInvrs) <= infoTot,
                            0, 1)
  dataDiscordanceRank <- abs(dataRankCases - dataRankSEInvrs) / 2



  # 04. TEST discordance in ranks by sample size and standard error -----
    rsltProp   <- binom.test(x = sum(dataDiscordanceProp),
                             n = infoNumStdy,
                             p = infoCOVal,
                             alternative = "greater")

    infoDscrdncProp <- rsltProp$estimate
    infoStatsChisq  <- rsltProp$statistic
    infoPvalProp    <- rsltProp$p.value
    infoLCIProp     <- rsltProp$conf.int[1][1]
    infoUCIProp     <- rsltProp$conf.int[1][2]


    rsltRank <- suppressWarnings(wilcox.test(dataDiscordanceRank, mu = 0,
                                             alternative = "greater",
                                             conf.int = TRUE,
                                             exact = FALSE,
                                             correct = TRUE))

    infoDscrdncRank <- rsltRank$estimate
    infoStatsV      <- rsltRank$statistic
    infoPvalRank    <- rsltRank$p.value
    infoLCIRank     <- rsltRank$conf.int[1]
    infoUCIRank     <- rsltRank$conf.int[2]



  # 05. BUILD a data frame of the DiSS -----
    dataRankPlot <- as.data.frame(cbind(seq         = c(1:infoNumStdy),
                                        study       = dataStudy,
                                        obs         = dataCases,
                                        se          = dataSE,
                                        seInvrs     = 1 / dataSE,
                                        rankObs     = dataRankCases,
                                        rankSE      = dataRankSE,
                                        rankSEInvrs = dataRankSEInvrs,
                                        dscrdncProp = dataDiscordanceProp,
                                        dscrdncRank = dataDiscordanceRank))

    dataRankPlot$seq          <- as.numeric(dataRankPlot$seq)
    dataRankPlot$obs          <- as.numeric(dataRankPlot$obs)
    dataRankPlot$se           <- as.numeric(dataRankPlot$se)
    dataRankPlot$seInvrs      <- as.numeric(dataRankPlot$seInvrs)
    dataRankPlot$rankObs      <- as.numeric(dataRankPlot$rankObs)
    dataRankPlot$rankSE       <- as.numeric(dataRankPlot$rankSE)
    dataRankPlot$rankSEInvrs  <- as.numeric(dataRankPlot$rankSEInvrs)
    dataRankPlot$dscrdncProp  <- as.numeric(dataRankPlot$dscrdncProp)
    dataRankPlot$xLRankTheory <- dataRankPlot$rankSEInvrs - 0.5
    dataRankPlot$yBRankTheory <- ifelse(dataRankPlot$rankSE - infoTot - 0.5 < 1,
                                        0.5,
                                        dataRankPlot$rankSE - infoTot - 0.5)
    dataRankPlot$xRRankTheory <- dataRankPlot$rankSEInvrs + 0.5
    dataRankPlot$yTRankTheory <- ifelse(dataRankPlot$rankSE + infoTot + 0.5 > infoNumStdy,
                                        infoNumStdy + 0.5,
                                        dataRankPlot$rankSE + infoTot + 0.5)
    dataRankPlot$colorObs     <- ifelse(abs(dataRankPlot$rankObs - dataRankPlot$rankSEInvrs) <= infoTot,
                                       "dodgerblue2", color)



  # 06. BUILD an discordance object -----
    dataDiRoSS <- list(name    = "Discordance in ranks test",
                       diross  = ifelse(infoPvalProp < 0.05,
                                    "Suspected","Unsuspected")
                       )
    class(dataDiRoSS)                <- "discordance"
    dataDiRoSS$measure               <- ifelse(infoMethod == "prop",
                                               "Discordance rate",
                                               "Discordance rank")
    dataDiRoSS$smry.test.prop        <- rsltProp
    dataDiRoSS$dscrdnc.rate          <- infoDscrdncProp
    dataDiRoSS$statistics.name.prop  <- "Bernoulli (exact test)"
    dataDiRoSS$statistics.value.prop <- infoStatsChisq
    dataDiRoSS$p.diross.prop         <- infoPvalProp
    dataDiRoSS$lci.diross.prop       <- infoLCIProp
    dataDiRoSS$uci.diross.prop       <- infoUCIProp
    dataDiRoSS$smry.test.rank        <- rsltRank
    dataDiRoSS$dscrdnc.rank          <- infoDscrdncRank
    dataDiRoSS$statistics.name.rank  <- "Wilcoxon signed rank exact test"
    dataDiRoSS$statistics.value.rank <- infoStatsV
    dataDiRoSS$p.diross.rank         <- infoPvalRank
    #dataDiRoSS$lci.diross.rank       <- infoLCIRank
    #dataDiRoSS$uci.diross.rank       <- infoUCIRank
    #dataDiRoSS$data.plot             <- dataRankPlot
    dataDiRoSS$sample.size           <- dataCases
    dataDiRoSS$se                    <- dataSE



  # 07. RETURN information of function `TestDiscordance()` -----
    #cat(paste("\n"), fill = TRUE, sep = "")
    cat(paste("Summary of discordance in ranks test:\n",
              ifelse(infoMethod == "prop",
                     " Statistics (Bernoulli exact): ",
                     " Statistics (Wilcoxon signed rank exact):"),
              ifelse(infoMethod == "prop",
                     ifelse(infoStatsChisq < 0.001,
                            "< 0.001",
                            round(infoStatsChisq, 3)),
                     ifelse(infoStatsV < 0.001,
                            "< 0.001",
                            round(infoStatsV, 3))
                     ),
              "\n P-value: ",
              ifelse(infoMethod == "prop",
                     ifelse(infoPvalProp < 0.001,
                            "< 0.001",
                            round(infoPvalProp, 3)),
                     ifelse(infoPvalRank < 0.001,
                            "< 0.001",
                            round(infoPvalRank, 3))
                     ),
              "\n Note: ",
              ifelse(infoMethod == "prop",
                     ifelse(infoPvalProp < 0.05,
                            "Suspected discordance in ranks between theoretical and observed study size.",
                            "No significant finding in the test of discordance in study size ranks."),
                     ifelse(infoPvalRank < 0.05,
                            "Suspected discordance in ranks between theoretical and observed study size.",
                            "No significant finding in the test of discordance in  study size ranks.")
                     ),
              sep = ""),
        fill = TRUE, sep = "")



  # 08. ILLUSTRATE discordance plot -----
    if (plot == TRUE) {

      plot(c(1, infoNumStdy * 1.25),
           c(-infoNumStdy/10, infoNumStdy),
           type = "n", frame = FALSE,
           xaxt = "n", yaxt = "n",
           xlab = "", ylab = "")
      # Discordance area
      rect(0, #min(dataRankPlot$xLRankTheory),
           0.5, #min(dataRankPlot$yBRankTheory),
           max(dataRankPlot$xRRankTheory),
           max(dataRankPlot$yTRankTheory),
           col = "lavenderblush", border = NA)
      # Accordance area
      rect(dataRankPlot$xLRankTheory, dataRankPlot$yBRankTheory,
           dataRankPlot$xRRankTheory, dataRankPlot$yTRankTheory,
           col = "lightcyan", border = NA)

      # Observations
      points(dataRankPlot$rankSE, dataRankPlot$rankObs,
             cex = ifelse(infoNumStdy < 11, 3,
                          1/infoNumStdy * 30),
             col = dataRankPlot$colorObs,
             pch = 22, bg = dataRankPlot$colorObs)

      axis(side = 3, at = dataRankPlot$rankObs,
           labels = as.integer(dataRankPlot$rankObs))
      axis(side = 2, at = dataRankPlot$rankSE,
           labels = dataRankPlot$rankSEInvrs, las = 2)
      text(c(1:infoNumStdy), 0.3,
           dataRankPlot$study,
           cex = ifelse(infoNumStdy < 11, 1,
                        1 / sqrt(infoNumStdy / 10)),
           pos = 1, srt = 45)
      mtext("Discordance plot", side = 1, cex = 1.5, font = 2,
            line = ifelse(max(nchar(dataRankPlot$study)) < 11, 2,
                          ifelse(max(nchar(dataRankPlot$study)) / 5 < 4,
                                 max(nchar(dataRankPlot$study)) / 5,
                                 4)
            ),
            at = infoNumStdy / 2 + ifelse(infoNumStdy %% 2 == 0,
                                          0, 0.5)
      )
      mtext("Rank of observed study size", side = 3, cex = 1.2,
            line = 3, at = infoNumStdy / 2 + ifelse(infoNumStdy %% 2 == 0,
                                                    0, 0.5)
      )
      mtext("Rank of theoretical study size", side = 2, cex = 1.2,
            line = 3, at = infoNumStdy / 2 + ifelse(infoNumStdy %% 2 == 0,
                                                    0.5, 0)
      )

      # Legend
      points(infoNumStdy * 1.07, infoNumStdy,
             cex = ifelse(infoNumStdy < 11, 1,
                          1 / infoNumStdy * 10),
             col = "dodgerblue2",
             pch = 22, bg = "dodgerblue2")
      text(infoNumStdy * 1.08, infoNumStdy,
           "Observed accordance", pos = 4)

      points(infoNumStdy * 1.07, infoNumStdy * 0.8,
             cex = ifelse(infoNumStdy < 11, 1,
                          1 / infoNumStdy * 10),
             col = "lightcyan",
             pch = 22, bg = "lightcyan")
      text(infoNumStdy * 1.08, infoNumStdy * 0.8,
           "Accordance area", pos = 4)

      points(infoNumStdy * 1.07, infoNumStdy * 0.6,
             cex = ifelse(infoNumStdy < 11, 1,
                          1 / infoNumStdy * 10),
             col = color,
             pch = 22, bg = color)
      text(infoNumStdy * 1.08, infoNumStdy * 0.6,
           "Observed discordance", pos = 4)

      points(infoNumStdy * 1.07, infoNumStdy * 0.4,
             cex = ifelse(infoNumStdy < 11, 1,
                          1 / infoNumStdy * 10),
             col = "lavenderblush",
             pch = 22, bg = "lavenderblush")
      text(infoNumStdy * 1.08, infoNumStdy * 0.4,
           "Discordance area", pos = 4)

      text(infoNumStdy * 1.08, infoNumStdy * 0.2,
           paste("Summary \n",
                 "discordance test:\n",
                 " Statistics ",
                 ifelse(infoMethod == "prop",
                        "(B): ",
                        "(V): "),
                 ifelse(infoMethod == "prop",
                        ifelse(infoStatsChisq < 0.001,
                               "< 0.001",
                               round(infoStatsChisq, 3)),
                        ifelse(infoStatsV < 0.001,
                               "< 0.001",
                               round(infoStatsV, 3))
                        ),
                 "\n P-value: ",
                 ifelse(infoMethod == "prop",
                        ifelse(infoPvalProp < 0.001,
                               "< 0.001",
                               round(infoPvalProp, 3)),
                        ifelse(infoPvalRank < 0.001,
                               "< 0.001",
                               round(infoPvalRank, 3))
                        ),
                 sep = ""),
           pos = 4, cex = 1)

    }

    output <- dataDiRoSS

}

