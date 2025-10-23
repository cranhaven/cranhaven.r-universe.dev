#' Life traits from Thermal Performance Curve
#'
#' Compute life traits from a Thermal Performance Curve
#'
#' @param nlsDR The object obtained from the \code{devRateModel} function.
#' @param prec The precision for the temperature (default = 0.1 degree
#'   celsius).
#' @param lowTempLim The minimum temperature for the metrics (default = 0
#'   degree celsius).
#' @param highTempLimit The maximum temperature for the metrics (default = +60
#'   degree celsius).
#' @param devLimit The development rate considered as null (default = 0.01).
#' @param printOut A logical to print the result (default = FALSE).
#' @return A matrix with one column and one row for each metric. The metrics
#'   names are the row names.
#' @export
#' @examples
#' rawDevEggs <- matrix(
#'    c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5,
#'    0.066, 13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20,
#'    0.143, 25, 0.171, 25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001
#' ), ncol = 2, byrow = TRUE)
#' mEggs <- devRateModel(
#'    eq = taylor_81,
#'    temp = rawDevEggs[,1],
#'    devRate = rawDevEggs[,2],
#'    startValues = list(Rm = 0.05, Tm = 30, To = 5)
#' )
#' myMetrics <- dRGetMetrics(nlsDR = mEggs, printOut = TRUE)
dRGetMetrics <- function(
    nlsDR, prec = 0.1, lowTempLim = 0, highTempLimit = 60,
    devLimit = 0.01, printOut = FALSE){
  devT <- seq(from = lowTempLim, to = highTempLimit, by = prec)
  devRT <- stats::predict(nlsDR, newdata = list(T = devT))
  devRT[devRT < devLimit] <- 0 # for asymptotic functions
  topt <- devT[devRT == max(devRT)]
  cTmin <- max(devT[devRT < devLimit & devT < topt])
  cTmax <- min(devT[devRT < devLimit & devT > topt])
  tolRange <- cTmax - cTmin
  tbMin <- max(devT[devRT <= max(devRT)/2 & devT < topt])
  tbMax <- min(devT[devRT <= max(devRT)/2 & devT > topt])
  tbr <- tbMax - tbMin
  tsm <- cTmax - topt
  dfMetrics <- t(data.frame(
    "CTmin" = cTmin,
    "CTmax" = cTmax,
    "TrTmax" = topt,
    "TrangeCTmaxCTmin" = tolRange,
    "Tmin0.5rT" = tbMin,
    "Tmax0.5rT" = tbMax,
    "Trange0.5rT" = tbr,
    "TSM" = tsm))
  if(printOut == TRUE){print(dfMetrics)}
  return(dfMetrics)
}

#' Life traits from the ectotherm database
#'
#' @param eq The name of the equation.
#' @param prec The precision for the temperature (default = 0.1 degree
#'   celsius).
#' @param lowTempLim The minimum temperature for the metrics (default = 0
#'   degree celsius).
#' @param highTempLimit The maximum temperature for the metrics (default =
#'   +60 degree celsius).
#' @param devLimit The development rate considered as null (default = 0.01).
#' @param devThresh The threshold in development rate to compute min and max
#'   temperature (default = 0.1).
#' @param lifeStage The life stage on which the life traits should be
#'   computed (default = "all"; specify "" to take into account all life
#'   stages).
#' @param colId The organism information for each column (default = genSp;
#'    choices = "ordersp" for Order, "familysp" for Family, "genussp" for
#'    Genus, "species" for species, and "gensp" for Genus and species).
#' @param printOut A logical to print the result (default = FALSE).
#' @return A matrix with one column per organism and one row for each
#'   metric. The metrics names are the names of each row.
#' @examples
#' dRGetMetricsInfo(eq = taylor_81)
#' dRGetMetricsInfo(eq = taylor_81, devThresh = 0.1)
#' @export
dRGetMetricsInfo <- function(
    eq, prec = 0.1, lowTempLim = 0, highTempLimit = 60, devLimit = 0.01,
    devThresh = 0.1, lifeStage = "all", colId = "genSp", printOut = FALSE){
  devT <- seq(from = lowTempLim, to = highTempLimit, by = prec)
  parameters <- eq$startVal[ , grepl( "param." , names( eq$startVal ) ) ]
  parameters <- parameters[grep(
    pattern = lifeStage,
    x = as.vector(unlist(eq$startVal["stage"]))), ]
  colNamesId <- as.character(eq$startVal[, colId])[grep(
    pattern = lifeStage,
    x = as.vector(unlist(eq$startVal["stage"])))]
  parametersNames <- sapply(
    strsplit(names(parameters), split = "\\."), "[[", 2
  )
  dfMetrics <- suppressWarnings(sapply(1:nrow(parameters), function(j){
    for(i in 1:ncol(parameters)){
      assign(x = parametersNames[i], value = parameters[j, i])
    }
    x <- devT
    devRT <- eval(parse(text = eq$eqAlt))
    topt <- stats::na.omit(devT[devRT == max(devRT, na.rm = TRUE)])[1]
    cTmin <- max(devT[devRT < devLimit & devT < topt], na.rm = TRUE)
    cTmax <- min(devT[devRT < devLimit & devT > topt], na.rm = TRUE)
    cTXmin <- max(devT[devRT <= max(devRT, na.rm = TRUE) * devThresh &
                         devT < topt], na.rm = TRUE)
    cTXmax <- min(devT[devRT <= max(devRT, na.rm = TRUE) * devThresh &
                         devT > topt], na.rm = TRUE)
    tolRange <- cTmax - cTmin
    tolXRange <- cTXmax - cTXmin
    tbMin <- max(
      devT[devRT <= max(devRT, na.rm = TRUE)/2 & devT < topt],
      na.rm = TRUE)
    tbMax <- min(
      devT[devRT <= max(devRT, na.rm = TRUE)/2 & devT > topt],
      na.rm = TRUE)
    tbr <- tbMax - tbMin
    tsm <- cTmax - topt
    tsfX <- cTXmax - topt
    return(c(
      cTmin, cTmax, topt, tolRange, tbMin, tbMax, tbr, tsm, cTXmin,
      cTXmax, tolXRange, tsfX
    ))
  }))
  dfMetrics[dfMetrics == Inf] <- NA
  dfMetrics[dfMetrics == -Inf] <- NA
  rownames(dfMetrics) <- c(
    "CTmin",
    "CTmax",
    "TrTmax",
    "TrangeCTmaxCTmin",
    "Tmin0.5rT",
    "Tmax0.5rT",
    "Trange0.5rT",
    "TSM",
    "CTminX",
    "CTmaxX",
    "TrangeCTmaxXCTminX",
    "TSMX")
  colnames(dfMetrics) <- colNamesId
  return(dfMetrics)
}
