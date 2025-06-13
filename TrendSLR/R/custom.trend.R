#' Isolate trend component from mean sea level records via customised input
#' parameters and analysis
#'
#'
#' @param object an annual average mean sea level time series (refer \code{\link[stats]{ts}})
#' with NO missing data or an object of class "gap.fillview" (refer \code{\link{gap.fillview}})
#' with water levels (in millimetres).
#'
#'
#' \strong{Warning: } If input data files do not conform to these pre-conditions,
#' the analysis will be terminated. It should be further noted that the existence
#' of long period oscillations in global mean sea level have been well recognised
#' in the literature (eg. Chambers et al. (2012); Minobe (1999)). Therefore, in
#' order to be effective for climate change and sea level research, time series
#' input files are recommended to have a minimum length of at least 80 years
#' in order that the analysis can identify and isloate such signals. Time series
#' less than 80 years in length will be analysed but a warning will be displayed.
#' Time series less than 30 years are not permitted though.
#'
#' @param station_name character string, providing the name of the data record.
#'
#' \strong{Note: }This field can be left blank, however, it is retained for use
#' in banner labelling of graphical outputs.
#'
#' @param iter numeric, enables a user defined number of iterations for
#' bootstrapping to determine error margins. The user range is [500 to 10000]
#' where 10000 is the default setting.\cr
#'
#' \strong{Warning: }Although the default setting provides a more accurate basis
#' for estimating error margins, the degree of iterations slows the analysis and
#' can take several minutes to run.
#'
#' @param trend numeric, enables the user to select the trend components
#' directly in the form of a single component or multiple components (eg.,
#' c(1) or c(1,2,3)). The default setting is c(1) as the first component will
#' always be trend, however, other components might also have trend characteristics
#' which can be diagnostically observed and optimised via the \code{\link{check.decomp}}
#' function.
#'
#' @param DOF numeric, enables the user to optimise the degrees of freedom
#' for the fitted cubic smoothing spline to be applied to the trend. The default
#' setting is based on 1 degree of freedom every 8 years (Watson, 2018) and
#' this default is written to the console to enable the user to directly compare
#' with manually entered DOF settings. The DOF can be diagnostically observed and
#' optimised via the \code{\link{check.decomp}} function.
#'
#' @param vlm numeric, enables a user defined quantum for vertical land motion
#' in mm/year within the range [-20 to 20]. This rate is used to convert the rate
#' of relative sea level rise to an estimate of geocentric sea level rise. Positive
#' rates of vlm are associated with land uplift, while conversely negative rates
#' of vlm are associated with subsidence. This can be left blank in which case
#' only estimates of relative mean sea level will be determined.
#'
#' @param plot logical, if \dQuote{TRUE} then the original time series is
#' plotted to the screen along with the trend component and the result of gap
#' filling (where necessary). 95\% confidence intervals have also been applied.
#' Default = \dQuote{TRUE}.
#'
#' @param wdir character string, providing the name of the directory to send
#' output files (e.g., \dQuote{C:/myproject/}) when the save_summary argument is set to \dQuote{TRUE}.
#' If this field is left blank the save_summary argument is switched off and a message
#' will be sent to the console.
#'
#' @param save_summary logical, if \dQuote{TRUE} the object$Summary portion
#' of the returned value is exported direct to the defined  directory (wdir) and
#' saved as "detailed_summary_output.csv". Default = \dQuote{FALSE}.
#'
#' @details This function permits the customisation of key input parameters to
#' enable improved isolation of trend components (mean sea level) and estimated
#' associated velocities and accelerations. This function provides more flexibility
#' for the expert analyst than the \code{\link{msl.trend}} function which has fixed
#' inbuilt parameterisation based on the recommendations espoused in Watson (2018).
#' The selection of the "trend" and "DOF" parameters would be undertaken following
#' diagnostic analysis of the input time series via the \code{\link{check.decomp}}
#' function. The trend is isolated using Singular Spectrum Analysis, in particular,
#' aggregating components whose spectral energy in the low frequency bands exhibit
#' trend-like characteristics. Associated velocities and accelerations are
#' determined through the fitting of a cubic smoothing spline to the trend.
#'
#' @return An object of class \dQuote{custom.trend} is returned with the following
#' elements:
#' \describe{
#'  \item{\strong{$Station.Name: }}{the name of the data record.}
#'  \item{\strong{$Summary: }}{a summary data frame of the relevant attributes
#'  relating to the trend and
#'   the inputted annual average data set, including:}
#'  \itemize{
#'   \item{$Year: input data; }
#'   \item{$MSL: input data; }
#'   \item{$Trend: mean sea level trend; }
#'   \item{$TrendSD: standard deviation of the determined mean sea level
#'   trend; }
#'   \item{$Vel: relative velocity (or first derivative) of mean sea level trend
#'   (mm/year); }
#'   \item{$VelSD: standard deviation of the velocity of the mean sea level
#'   trend; }
#'   \item{$Acc: acceleration (or second derivative) of mean sea level trend
#'   (mm/year/year); }
#'   \item{$AccSD: standard deviation of the acceleration of the mean sea level
#'   trend; }
#'   \item{$Resids: time series of uncorrelated residuals; and }
#'   \item{$FilledTS: gap-filled time series (where necessary). }
#'   \item{$VelGeo: geocentric velocity (or first derivative) of mean sea level
#'   trend (mm/year)(only where vertical land motion has been supplied). }
#'    }
#' }
#'
#' \describe{
#'  \item{\strong{$Relative.Velocity: }}{outputs the peak relative velocity
#'  and the year in which it occurred.}
#'  \item{\strong{$Vertical.Land.Motion: }}{outputs the vertical land motion
#'  used to convert relative to geocentric velocity (user supplied input).}
#'  \item{\strong{$Geocentric.Velocity: }}{outputs the peak geocentric velocity
#'  and the year in which it occurred (if vertical land motion supplied).}
#'  \item{\strong{$Acceleration: }}{outputs the peak acceleration and the
#'  year in which it occurred.}
#'  \item{\strong{$Record.Length: }}{outputs details of the start, end and
#'  length of the input data set.}
#'  \item{\strong{$Fillgaps: }}{outputs the extent of missing data (years) in
#'  the original record and the gap filling method used (where necessary).}
#'  \item{\strong{$Bootstrapping.Iterations: }}{outputs the number of iterations
#'  used to generate the respective standard deviations for error margins.}
#'  \item{\strong{$Changepoints: }}{outputs the number and time at which
#'  changepoints in the variance of the uncorrelated residuals occur (if any).
#'  Where changepoints are identified, block bootstrapping procedures are used
#'  with residuals quarantined between changepoints.}
#'  \item{\strong{$Trend.Components: }}{outputs the components from the SSA
#'  decomposition of the original time series used to estimate the trend.}
#'  \item{\strong{$DOF.Fitted.Spline: }}{outputs the degrees of freedom used
#'  to fit the cubic smoothing spline to the trend in order to estimate velocity
#'  and acceleration.}
#'  }
#'
#' @references Chambers, D.P., Merrifield, M.A., and Nerem, R.S., 2012. Is there
#' a 60 year oscillation in global mean sea level? \emph{Geophysical Research Letters}, 39(18).
#'
#' Minobe, S., 1999. Resonance in bidecadal and pentadecadal climate oscillations
#' over the North Pacific: Role in climatic regime shifts. \emph{Geophysical Research Letters},
#' 26(7), pp.855-858.
#'
#' Watson, P.J., 2018. \emph{Improved Techniques to Estimate Mean Sea Level,
#' Velocity and Acceleration from Long Ocean Water Level Time Series to Augment
#' Sea Level (and Climate Change) Research.} PhD Thesis, University of New South
#' Wales, Sydney, Australia.
#'
#' @seealso \code{\link{msl.trend}}, \code{\link{gap.fillview}}, \code{\link{check.decomp}},
#' \code{\link{t}}, \code{\link[stats]{ts}}, \code{\link{msl.fileplot}},
#' \code{\link{msl.screenplot}}, \code{\link{summary}}, \code{\link{Balt}}.
#'
#' @examples
#'
#' data(Balt) # Baltimore mean sea level record
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA gap fill
#' \donttest{t <- custom.trend(g, station_name = "Baltimore (USA)", iter = 500, trend = c(1,2),
#' vlm = 0.6)}
#'
#' data(t)
#' str(t) # check structure of object
#'
#' @export
custom.trend <- function(object, station_name = " ", iter = 10000, trend = c(1), DOF = " ",
                      vlm = " ", plot = "TRUE", wdir = " ", save_summary = "FALSE") {
  # -----------------------------------------------------------------
  grDevices::graphics.off()  # close active graphics windows
  # -----------------------------------------------------------------
  # error handling for input file (must be time series or gap.fillview object)
  if (class(object) == "ts") {

    df_diag <- data.frame(V1 = as.vector(stats::time(object)), object)
    p <- 1 # conditioning parameter no gaps
    n <- length(df_diag[,2]) ## length of time series
    Year <- df_diag[,1]
    MSL <- df_diag[,2]
    TSfill <- vector(mode = "numeric", length = n)
    TSfill[TSfill == 0] <- NA
    lab1 <- paste("zero")
    lab2 <- paste("NA")

  } else if (class(object) == "gap.fillview") {

    lab2 <- object$Fillgaps
    object <- object$Summary; df_diag <- data.frame(cbind(object$Year,object$FilledTS)); names(df_diag) <- c("V1","V2")
    p <- 0 # conditioning parameter required gap-filling
    n <- length(df_diag[,2]) ## length of time series
    Year <- object$Year
    MSL <- object$MSL
    TSfill <- object$FilledTS
    lab1 <- sum(is.na(MSL))

  } else {
    stop("input object is not a time series or gap.fillview object: program terminated. Check
         manual for input file format requirements.")
  }
  # -----------------------------------------------------------------
  # error handling (missing values)
  if (anyNA(df_diag[,2]) == TRUE ) {
    # If there are missing values terminate
    stop("Missing values are not permitted within this function: program
         terminated. Check manual for input file format requirements and where necessary
         fill missing values first using the gap.fillview function in this package.")
  }
  # -----------------------------------------------------------------
  # error handling (time series too short < 30 years)
  if (length(df_diag[,2]) < 30) {
    # If time series is considered short
    message("Time series less than 30 years in length are considered too short
          for meaningful mean sea level diagnostic analysis and are not permitted
          within this function. Refer manual for more details.")
  }
  # -----------------------------------------------------------------
  # error handling (time series less than annual)
  if (min(diff(df_diag[[1]]), na.rm = TRUE) < 1) {
    # If time steps less than annual
    stop("dataset must be an annual time series: program terminated. Check
         manual for input file format requirements.")
  }
  # -----------------------------------------------------------------
  # error handling (iter outside range of 500 to 10000)
  if (iter < 500 | iter > 10000) {
    message("default setting applied iter = 10,000")
    iter <- 10000
  } else {
    iter <- iter
  }
   # -----------------------------------------------------------------
  # If station name not entered
  if (station_name == " ") {
    station_name <- " "  # empty
    lab5 <- paste("Station Name not entered")
    message("no station name entered, field left blank")
  } else {
    station_name <- station_name
    lab5 <- station_name
  }
  # -----------------------------------------------------------------
  # Trend
  trend <- trend
  # -----------------------------------------------------------------
  # If DOF (degrees of freedom for fitted smoothing spline) not entered
  DOF_A <- round(length(df_diag[,2])/8, 0)
  lab10a <- paste0("DOF  = ", DOF_A)

  if (DOF == " ") {
    ddd <- 1 # marker for default DOF
    DOF <- DOF_A
    lab10a <- paste0("Default applied. DOF  = ", DOF_A)
    lab15 <- lab10a
    message(lab10a)

  } else {
    ddd <- 0 # marker for self input DOF
    DOF <- DOF
    lab10b <- paste0("Self input DOF  = ", DOF)
    lab15 <- lab10b
    message(lab10b)

  }
  # -----------------------------------------------------------------
  # If vlm not entered
  if (vlm == " ") {
    vlm <- " "  # empty
    p2 <- 0 # conditioning parameter for NO vlm
    message("no Vertical Land Motion entered, field left blank")
    VelGeo <- vector(mode = "numeric", length = n)
    VelGeo[VelGeo == 0] <- NA
  } else {
    if (vlm <= -20 | vlm >= 20) {
      # vlm outside normal range
      stop("vertical land motion entered is outside normal range: program terminated.")
    } else {
      vlm <- vlm
      p2 <- 1 # conditioning parameter for including geocentric velocity
    }
  }

  # -----------------------------------------------------------------
  # Isolate Trend
  # -----------------------------------------------------------------

  if (requireNamespace("Rssa", quietly = TRUE)) {
    Rssa::reconstruct
    Rssa::ssa
  }
  t2 <- df_diag[,2] # Column [2] contains the time series of interest (annual filled readings)
  model.ssa <- Rssa::ssa(t2, kind = "1d-ssa", svd.method = "auto")  # default 1D-ssa settings
  recon <- Rssa::reconstruct(model.ssa, groups = list(trend)) ## trend components from input, default = c(1)
  Trend <- recon$F1 ## reconstructed trend
  # -----------------------------------------------------------------

  # fit cubic smooting spline to trend to determine velocity and acceleration
  if (requireNamespace("stats", quietly = TRUE)) {
    stats::smooth.spline
    stats::predict
  }
  ss <- stats::smooth.spline(Trend, df = DOF)
  ssVEL <- stats::predict(ss, type = "response", deriv = 1)
  ssVEL <- ssVEL$y  # Velocity vector
  ssACC <- stats::predict(ss, type = "response", deriv = 2)
  ssACC <- ssACC$y  # Acceleration vector
  # -----------------------------------------------------------------

  # determine standard errors
  # remove auto-correlation from residuals
  res <- Trend - t2  # correlated residuals
  if (requireNamespace("forecast", quietly = TRUE)) {
    forecast::auto.arima
  }
  aa <- forecast::auto.arima(res, seasonal = FALSE)  # auto.arima fit non-season
  noncor <- aa$residuals  # time series of non-correlated residuals
  # -----------------------------------------------------------------

  # detect changepoints in variance of non-correlated residuals
  if (requireNamespace("changepoint", quietly = TRUE)) {
    changepoint::cpt.var
    changepoint::cpts
  }
  cpt <- changepoint::cpt.var(noncor, minseglen = 15)  # min segment 15 years (default)
  ff <- changepoint::cpts(cpt)  # point in time series at which changepoint is detected
  # -----------------------------------------------------------------
  # bootsrapping routines
  TS <- vector(mode = "list", length = iter)
  VEL <- vector(mode = "list", length = iter)
  ACC <- vector(mode = "list", length = iter)
  # -----------------------------------------------------------------
  # no changepoints in the variance of residuals
  if (length(ff) == 0) {
    lab3 <- paste("zero")  # number of changepoints
    lab4 <- paste("NA")  # changepoint year
    for (i in 1:iter) {
      samp <- sample(noncor, replace = TRUE)
      newTS <- Trend + samp
      model2.ssa <- Rssa::ssa(newTS, kind = "1d-ssa", svd.method = "auto")  # 1D-ssa
      recon2 <- Rssa::reconstruct(model2.ssa, groups = list(trend))
      trend2 <- recon2$F1
      TS[[i]] <- trend2
      ss2 <- stats::smooth.spline(trend2, df = DOF)
      ssVEL2 <- stats::predict(ss2, type = "response", deriv = 1)
      VEL[[i]] <- ssVEL2$y  ## Isolate Velocity Vector
      ssACC2 <- stats::predict(ss2, type = "response", deriv = 2)
      ACC[[i]] <- ssACC2$y  ## Isolate acceleration Vector
    }
  }
  # -----------------------------------------------------------------
  # 1 changepoint in variance of residuals detected
  if (length(ff) == 1) {
    lab3 <- 1  # number of changepoints
    lab4 <- df_diag[ff, 1]  # changepoint year
    for (i in 1:iter) {
      q1 <- noncor[1:ff - 1]
      r1 <- sample(q1, replace = TRUE)
      q2 <- noncor[ff:n]
      r2 <- sample(q2, replace = TRUE)
      samp <- as.numeric(samp <- paste(c(r1, r2)))
      newTS <- Trend + samp
      model2.ssa <- Rssa::ssa(newTS, kind = "1d-ssa", svd.method = "auto")  # 1D-ssa
      recon2 <- Rssa::reconstruct(model2.ssa, groups = list(trend))
      trend2 <- recon2$F1
      TS[[i]] <- trend2
      ss2 <- stats::smooth.spline(trend2, df = DOF)
      ssVEL2 <- stats::predict(ss2, type = "response", deriv = 1)
      VEL[[i]] <- ssVEL2$y  ## Isolate Velocity Vector
      ssACC2 <- stats::predict(ss2, type = "response", deriv = 2)
      ACC[[i]] <- ssACC2$y  ## Isolate acceleration Vector
    }
  }
  # -----------------------------------------------------------------
  # standard deviations
  TSboot <- as.data.frame(do.call(cbind, TS))
  TSsd <- apply(TSboot[, c(1:iter)], 1, sd)
  VELboot <- as.data.frame(do.call(cbind, VEL))
  VELsd <- apply(VELboot[, c(1:iter)], 1, sd)
  ACCboot <- as.data.frame(do.call(cbind, ACC))
  ACCsd <- apply(ACCboot[, c(1:iter)], 1, sd)
  # -----------------------------------------------------------------
  # Determine Geocentric Velocity
  if (p2 == 1) {
    VelGeo <- ssVEL + vlm # add relative velocity to vlm
  } else {
    VelGeo <- VelGeo
  }
  # -----------------------------------------------------------------
  # summary dataframe
  summ <- as.data.frame(cbind(Year, MSL, Trend, TSsd, ssVEL, VELsd,
                              ssACC, ACCsd, noncor, TSfill, VelGeo))
  colnames(summ) <- c("Year", "MSL", "Trend", "TrendSD", "Vel", "VelSD", "Acc",
                      "AccSD", "Resids", "FilledTS", "VelGeo")
  st <- c(1:3, n - 2, n - 1, n)
  summ$Acc[st] <- NA
  # NA's at first and last 3 acceleration values (not accurate with spline)
  summ$AccSD[st] <- NA


  # -----------------------------------------------------------------
  # summary object
  summ2 <- summ[4:(n - 3), ]  # non NA portion of summ
  object <- NULL
  object$Station.Name <- lab5
  object$Summary <- summ
  object$Relative.Velocity <- list(Peak.mm.yr = max(summ$Vel),
                                   Year = with(summ, Year[Vel == max(Vel)]))
  if (p2 == 0) {
    object$Vertical.Land.Motion <- list(mm.yr = "NA") # no vlm
    object$Geocentric.Velocity <- list(Peak.mm.yr = "NA",
                                       Year = "NA")
  } else {
    object$Vertical.Land.Motion <- list(mm.yr = vlm) # with vlm
    object$Geocentric.Velocity <- list(Peak.mm.yr = max(summ$VelGeo),
                                       Year = with(summ, Year[VelGeo == max(VelGeo)]))
  }
  object$Acceleration <- list(Peak.mm.yr.yr = max(summ2$Acc),
                              Year = with(summ2, Year[Acc == max(summ2$Acc)]))
  object$Record.Length <- list(Start = summ$Year[1], End = summ$Year[n], Years = n)
  object$Fillgaps <- list(Gaps = lab1, Method = lab2)
  object$Bootstrapping.Iterations <- iter
  object$Changepoints <- list(Number = lab3, Year = lab4)
  object$Trend.Components <- trend
  object$DOF.Fitted.Spline <- list(lab15)
  class(object) <- "custom.trend"
  # -----------------------------------------------------------------
  # wdir = " "
  # wdir not entered or entered outside range
  if (wdir == " ") {
    TH <- 1 # parameter for NO file save
  } else {
    TH <- 2 # parameter for YES file save
    wdir <- wdir
  }

  # -----------------------------------------------------------------

  # save summary file
  # save_summary option not entered or entered outside range
  if (save_summary == "TRUE" | save_summary == "FALSE") {
    save_summary <- save_summary
  } else {
    message("default save_summary = FALSE setting applied")
    save_summary <- "FALSE"
  }

  # -----------------------------------------------------------------
  # check wdir and save_summary = "TRUE"

  if (TH == 1 && save_summary == "TRUE") {
    message("File has not been saved because no working directory was advised. Refer wdir argument and manual for more details.")
  } else if (TH == 2 && save_summary == "FALSE"){
    TH <- 1
  }

  if (TH == 2 && save_summary == "TRUE")  {
    message("detailed_summary_output.csv file saved to defined directory")
    utils::write.csv(object$Summary, file.path(wdir, "detailed_summary_output.csv"), row.names = FALSE)
  }

  # -----------------------------------------------------------------
  # plot routines
  # plot option not entered or entered outside range
  if (plot == "TRUE" | plot == "FALSE") {
    plot <- plot
  } else {
    message("default TYPE setting applied")
    plot <- "TRUE"
  }
  if (summ$Trend[n] - summ$Trend[1] < 0) {
    # check slope of graph for locating chart 1 legends
    laba <- paste("bottomleft")  # Chart key to bottomleft
  } else {
    # default setting
    laba <- paste("bottomright")  # Chart key to bottomright
  }
  # -----------------------------------------------------------------
  if (plot == "TRUE") {
    # set plotting limits
    if (requireNamespace("plyr", quietly = TRUE)) {
      plyr::round_any
    }
    if (n < 100) {
      # setting up x-axis plotting parameters
      xtic = 10  # year ticks on x-axis
      xlo <- plyr::round_any(min(summ[, 1]), 10, floor)
      xhi <- plyr::round_any(max(summ[, 1]), 10, ceiling)
    } else {
      # default
      xtic = 20
      xlo <- plyr::round_any(min(summ[, 1]), 20, floor)
      xhi <- plyr::round_any(max(summ[, 1]), 20, ceiling)
    }
    xlim = c(xlo, xhi)
    # -----------------------------------------------------------------
    # set y-axis scale
    if (p == 0) {
      # gap filling routine required
      M1 <- max(max(summ[, 2], na.rm = TRUE), max(summ[, 10]))
      M2 <- min(min(summ[, 2], na.rm = TRUE), min(summ[, 10]))
    }
    if (p == 1) {
      # no gaps in record
      M1 <- max(summ[, 2])
      M2 <- min(summ[, 2])
    }
    ylen <- M1 - M2
    if (ylen < 200) {
      # setting up y-axis plotting parameters
      ytic = 20  # year ticks on y-axis
      ylo <- plyr::round_any(M2, 20, floor)
      yhi <- plyr::round_any(M1, 20, ceiling)
    } else {
      # default
      ytic = 50
      ylo <- plyr::round_any(M2, 50, floor)
      yhi <- plyr::round_any(M1, 50, ceiling)
    }
    ylim = c(ylo, yhi)
    # -----------------------------------------------------------------
    opar <- graphics::par(no.readonly = TRUE)  # capture current settings
    on.exit(par(opar))
    graphics::par(mar = c(3.6, 5.1, 2, 0.5), las = 1)
    graphics::plot(summ$Year, summ$MSL, type = "l", xaxt = "n", yaxt = "n", lty = 0,
         xlim = xlim, ylim = ylim, xlab = " ", ylab = " ", main = station_name)
    graphics::rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col = "lightcyan1")
    graphics::title(ylab = "Relative Mean Sea Level (mm)", font.lab = 2, line = 3.5)
    graphics::title(xlab = "Year", font.lab = 2, line = 2.5)
    graphics::axis(side = 1, at = seq(xlo, xhi, by = xtic))
    graphics::axis(side = 2, at = seq(ylo, yhi, by = ytic))
    # -----------------------------------------------------------------
    if (p == 0) {
      # gap filling routine required
      graphics::legend(laba, bg = "white", legend = c("KEY", "Annual Average Data",
                                            "MSL Trend", "95% Confidence Intervals",
                                            "Gap Filling"),
             text.font = c(2, 1, 1, 1, 1), lty = c(0, 1, 1, 2, 1),
             lwd = c(1, 1, 3, 1, 1), col = c("black", "darkgrey", "black",
                                             "black", "red"),
             cex = c(0.7, 0.7, 0.7, 0.7, 0.7))
      graphics::lines(summ$Year, summ$FilledTS, col = "red")
      graphics::lines(summ$Year, summ$MSL, col = "darkgrey")
      graphics::lines(summ$Year, summ$Trend, lwd = 2)
      graphics::lines(summ$Year, summ$Trend + 1.96 * summ$TrendSD, lty = 2)
      graphics::lines(summ$Year, summ$Trend - 1.96 * summ$TrendSD, lty = 2)
    }
    # -----------------------------------------------------------------
    if (p == 1) {
      # no gaps in record
      graphics::legend(laba, bg = "white", legend = c("KEY", "Annual Average Data",
                                            "MSL Trend", "95% Confidence Intervals"),
             text.font = c(2, 1, 1, 1, 1), lty = c(0, 1, 1, 2), lwd = c(1, 1, 3, 1),
             col = c("black", "darkgrey", "black", "black"),
             cex = c(0.7, 0.7, 0.7, 0.7))
      graphics::lines(summ$Year, summ$MSL, col = "darkgrey")
      graphics::lines(summ$Year, summ$Trend, lwd = 2)
      graphics::lines(summ$Year, summ$Trend + 1.96 * summ$TrendSD, lty = 2)
      graphics::lines(summ$Year, summ$Trend - 1.96 * summ$TrendSD, lty = 2)
    }
  }
  # -----------------------------------------------------------------
  # no plotting
  if (plot == "FALSE") {
    message("no plotting required")
  }
  return(object)
}

#' @importFrom utils write.csv
NULL

