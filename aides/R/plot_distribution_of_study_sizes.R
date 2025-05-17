#' @title Illustrate distribution of study sizes.
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotDistrSS()** is a function for illustrating graphics of distribution of study sizes.
#'
#' @param n       NUMERIC values for sample size (n) of each study.
#' @param data    DATA FRAME consists of three columns for study label, study
#'                year, and sample size.
#' @param study   CHARACTER for study labels.
#' @param time    NUMERIC values of time sequence.
#' @param method  CHARACTER for indicating which method should be used for testing
#'                normality.
#'
#'
#' @return
#' **PlotDistrSS()** returns a plot of distribution of sample sizes.
#'
#'
#' @references
#' Rosenblatt, M. (1956). Remarks on Some Nonparametric Estimates of a Density
#' Function. **The Annals of Mathematical Statistics**, *27(3)*, 832–837.
#' doi:10.1214/aoms/1177728190.
#'
#' Parzen, E. (1962). On Estimation of a Probability Density Function and Mode.
#' **The Annals of Mathematical Statistics**, *33(3)*, 1065–1076.
#' doi:10.1214/aoms/1177704472. JSTOR 2237880.
#'
#'
#' @seealso \code{\link{TestDisparity}}, \code{\link{PlotDisparity}}
#'
#'
#' @export PlotDistrSS



PlotDistrSS <- function(n,
                        data  = NULL,
                        study = NULL,
                        time  = NULL,
                        method = "default") {

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

  dataDistr <- data.frame(study = vctStudy,
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

  lgcMethod   <- ifelse(length(method) != 1,
                        TRUE,
                        ifelse(method %in% c("default",
                                             "shapiro",
                                             "ks"),
                               FALSE, TRUE)
  )


  if (lgcData) {
    infoStopData  <- 'Argument of parameter "data" must be a data frame consisting of three columns for study label, study year, and sample size.'
  }

  if (lgcN) {
    infoStopN     <- 'Argument of parameter "n" must be a integer vector for sample size of each study.'
  }

  if (lgcStudy) {
    infoStopStudy <- 'Argument of parameter "study" must be a vector for study label of each study, and length of the vector should be the same with length of the vector for sample size.'
  }

  if (lgcTime) {
    infoStopTime  <- 'Argument of parameter "time" must be a vector for time of each study, and length of the vector should be the same with length of the vector for sample size.'
  }

  if (lgcMethod) {
    infoStopMethod <- 'Argument of parameter "method" must be a string for indicating which method should be used for testing normality.'
  }

  # 03. RETURN results of argument checking  -----

  if (lgcData | lgcN | lgcStudy | lgcTime | lgcMethod)

    stop(paste(ifelse(lgcData, paste(infoStopData, "\n", sep = ""), ""),
               ifelse(lgcN, paste(infoStopN, "\n", sep = ""), ""),
               ifelse(lgcStudy, paste(infoStopStudy, "\n", sep = ""), ""),
               ifelse(lgcTime, paste(infoStopTime, "\n", sep = ""), ""),
               ifelse(lgcMethod, paste(infoStopMethod, "\n", sep = ""), ""),
               sep = "")
    )



  # 04. PROCESS additive setting -----
  infoNmbrStdy   <- length(dataDistr$n)
  vctVlnPltY     <- density(dataDistr$n)$x
  vctVlnPltXRght <- density(dataDistr$n)$y
  vctVlnPltXLft  <- -density(dataDistr$n)$y
  infoVlnPltYMax <- max(vctVlnPltY)
  infoVlnPltYMin <- min(vctVlnPltY)
  infoVlnPltXMax <- max(vctVlnPltXRght)
  infoVlnPltXMin <- min(vctVlnPltXLft)
  vctVlnPltYAll  <- c(vctVlnPltY, vctVlnPltY[c(length(vctVlnPltY):1)])
  vctVlnPltXAll  <- c(vctVlnPltXRght, vctVlnPltXLft[c(length(vctVlnPltXLft):1)])

  infoMethodOrg  <- method
  if (infoMethodOrg == "default") {
    if (infoNmbrStdy >= 50) {
      infoMethod <- "ks"
    } else {
      infoMethod <- "shapiro"
    }
  } else {
    infoMethod   <- infoMethodOrg
  }

  infoPvalShpr   <- shapiro.test(dataDistr$n)$p
  infoPvalKS     <- ks.test(dataDistr$n, 'pnorm')$p

  if (infoMethod == "shapiro") {
    infoPvalNrml <- infoPvalShpr
  } else if (infoMethod == "ks") {
    infoPvalNrml <- infoPvalKS
  }

  plotBox        <- boxplot(dataDistr$n, plot = FALSE)

  infoMCase    <- mean(dataDistr$n)
  infoSDCase   <- sd(dataDistr$n)
  set.seed(101020)
  vctNrmlDstr  <- round(rnorm(n = infoNmbrStdy * 10000, mean = infoMCase, sd = infoSDCase), 0)
  vctNrmlDstr  <- rnorm(n = infoNmbrStdy * 1000, mean = infoMCase, sd = infoSDCase)
  #plot(density(dnorm(vctNrmlDstr)))
  vctNrmlDnstY <- density(vctNrmlDstr)$x
  vctNrmlDnstX <- density(vctNrmlDstr)$y
  infoNrmlDnstYMax <- max(vctNrmlDnstY)

  vctNrmlDnstY <- seq(-4, 4, length = 1000) * infoSDCase + infoMCase
  vctNrmlDnstX <- dnorm(vctNrmlDnstY, infoMCase, infoSDCase)


  # 05. ILLUSTRATE distribution of sample sizes -----

  plot(c(-max(vctVlnPltXRght, vctNrmlDnstX), max(vctVlnPltXRght, vctNrmlDnstX)),
       c(0, ceiling(max(infoVlnPltYMax, infoNrmlDnstYMax))),
       frame = FALSE, xaxt = "n", yaxt = "n",
       col = rgb(1, 1, 1, 0),
       xlab = "", ylab = "")

  polygon(vctVlnPltXRght,
          vctVlnPltY,
          col = ifelse(infoPvalNrml > 0.1,
                       rgb(0.8, 0.9, 0.85, 1),
                       ifelse(infoPvalNrml > 0.5,
                              rgb(0.75, 0.85, 0.9, 1),
                              ifelse(infoPvalNrml > 0.01,
                                     rgb(0.85, 0.85, 0.80, 1),
                                     rgb(0.85, 0.7, 0.75, 1)
                              ))),
          lty = 3,
          border = "gray"
  )
  polygon(-vctNrmlDnstX,
          vctNrmlDnstY,
          lty = 3, col = "lightsteelblue",
          border = "gray"
  )
  text(c(-max(vctVlnPltXRght, vctNrmlDnstX) / 2, max(vctVlnPltXRght, vctNrmlDnstX) / 2),
       rep(ceiling(max(infoVlnPltYMax, infoNrmlDnstYMax)), 2) * 0.9,
       c("Expected distribution", "Observed distribution"),
       col = c("steelblue",
               ifelse(infoPvalNrml > 0.1,
                      rgb(0.5, 0.6, 0.55, 1),
                      ifelse(infoPvalNrml > 0.5,
                             rgb(0.45, 0.55, 0.6, 1),
                             ifelse(infoPvalNrml > 0.01,
                                    rgb(0.55, 0.55, 0.50, 1),
                                    rgb(0.55, 0.4, 0.45, 1)
                             )))
       ),
       cex = 1.2,
       font = 2
  )
  points(0, infoMCase, pch = 22,
         col = "gray20", bg = "gray20",
         cex = 2)
  segments(0, infoMCase - infoSDCase,
           0, infoMCase + infoSDCase,
           col = "gray20",
           lwd = 2)
  text(0,
       infoMCase,
       paste("Mean = ", round(infoMCase, 0),"\n",
             "SD = ", round(infoSDCase, 3), sep = ""),
       pos = 4,
       cex = 0.8
  )
  mtext("Sample size",
        side = 2, line = 3, font = 2, cex = 1.2)
  mtext("Density",
        side = 1, line = 2, font = 2, cex = 1.2)
  mtext(paste("(normality test: ",
              "p-value",
              ifelse(infoPvalNrml < 0.001,
                     " < 0.001)",
                     paste(" = ",
                           round(infoPvalNrml, 3),
                           ")",
                           sep = "")
              ),
              sep = ""),
        side = 1,
        line = 3,
        font = 1,
        cex = 0.8)
  axis(side = 2, las = 2, cex.axis = 0.8)
  axis(side = 1,
       at = c(-max(vctVlnPltXRght, vctNrmlDnstX), 0, max(vctVlnPltXRght, vctNrmlDnstX)),
       labels = c(round(max(vctVlnPltXRght, vctNrmlDnstX), 3), 0, round(max(vctVlnPltXRght, vctNrmlDnstX), 3)),
       cex.axis = 0.8
  )
}

