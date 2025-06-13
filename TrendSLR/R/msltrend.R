#' Isolate trend component from mean sea level records.
#'
#' @param object an annual average mean sea level time series (refer \code{\link[stats]{ts}})
#' with water levels (in millimetres). Missing data and maximum missing data gap
#' are limited to 15\% and 5\%, respectively, of the data record.\cr
#'
#'
#' \strong{Warning: } If input data files do not conform to these pre-conditions,
#' the analysis will be terminated. It should be further noted that the existence
#' of long period oscillations in global mean sea level have been well recognised
#' in the literature (eg. Chambers et al. (2012); Minobe (1999)). Therefore, in
#' order to be effective for climate change and sea level research, time series
#' input files are recommended to have a minimum length of at least 80 years
#' in order that the package can identify and isloate such signals. Time series
#' less than 80 years in length will be analysed but a warning will be displayed.
#'
#' @param station_name character string, providing the name of the data record.
#'
#' \strong{Note: }This field can be left blank, however, it is retained for use
#' in banner labelling of plotting and associated outputs.
#'
#' @param fillgaps numeric, provides 5 alternative gap filling procedures for
#' missing data. The following options are available:
#'
#' \itemize{
#'   \item 1: The default procedure is based on iterative gap filling using Singular Spectrum Analysis (refer \code{\link[Rssa]{igapfill}});
#'   \item 2: linear interpolation (refer \code{\link[zoo]{na.approx}});
#'   \item 3: Cubic spline interpolation (refer \code{\link[zoo]{na.approx}});
#'   \item 4: Stineman's interpolation (refer \code{\link[imputeTS]{na.interpolation}}); and
#'   \item 5: Weighted moving average (refer \code{\link[imputeTS]{na.ma}}).
#' }
#'
#' \strong{Note: }Gap filled portions of the time series are denoted in red on
#' the default screen plot. This is done specifically to provide ready visual
#' observation to discern if the selected gap filling method provides an
#' appropriate estimate within the gaps in keeping with the remainder of the
#' historical record. Depending on the nature of the record and extent of gaps,
#' some trial and error between alternatives might be necessary to optimise gap
#' filling. This is best achieved via the \code{\link{gap.fillview}} function
#' which permits visual screen checking of various gap-filling options prior to
#' undertaking the full trend analysis.
#'
#' @param iter numeric, enables a user defined number of iterations for
#' bootstrapping to determine error margins. The user range is [500 to 10000]
#' where 10000 is the default setting.\cr
#'
#' \strong{Warning: }Although the default setting provides a more accurate basis
#' for estimating error margins, the degree of iterations slows the analysis and
#' can take several minutes to run.
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
#' output files (e.g., \dQuote{C:/myproject/}) when the save_summary argument is set to "TRUE".
#' If this field is left blank the save_summary argument is switched off and a message
#' will be sent to the console.
#'
#' @param save_summary logical, if \dQuote{TRUE} the object$Summary portion
#' of the returned value is exported direct to the defined  directory (wdir) and
#' saved as "detailed_summary_output.csv". Default = \dQuote{FALSE}.
#'
#' @details This function deconstructs annual average time series data into a trend
#' and associated velocities and accelerations, filling necessary internal structures
#' to facilitate all other functions in this package. The trend is isloated using
#' Singular Spectrum Analysis, in particular, aggregating components whose low
#' frequency band [0 to 0.01] exceed a threshold contribution of 75\%. Associated
#' velocities and accelerations are determined through the fitting of a cubic
#' smoothing spline to the trend with 1 degree of freedom per every 8 years of
#' record length. The fixed settings built into this function are based on the
#' detailed research and development summarised in Watson (2016a,b; 2018).
#'
#' @return An object of class \dQuote{msl.trend} is returned with the following
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
#'    }
#'
#' @references Chambers, D.P., Merrifield, M.A., and Nerem, R.S., 2012. Is there
#' a 60 year oscillation in global mean sea level? \emph{Geophysical Research Letters}, 39(18).
#'
#' Minobe, S., 1999. Resonance in bidecadal and pentadecadal climate oscillations
#' over the North Pacific: Role in climatic regime shifts. \emph{Geophysical Research Letters},
#' 26(7), pp.855-858.
#'
#' Watson, P.J., 2016a. Identifying the best performing time series
#' analytics for sea-level research. In: \emph{Time Series Analysis and
#' Forecasting, Contributions to Statistics}, pp. 261-278, ISBN 978-3-319-28725-6.
#' Springer International Publishing.
#'
#' Watson, P.J., 2016b. How to improve estimates of real-time acceleration in
#' the mean sea level signal. In: Vila-Concejo, A., Bruce, E., Kennedy, D.M.,
#' and McCarroll, R.J. (eds.), Proceedings of the 14th International Coastal
#' Symposium (Sydney, Australia). \emph{Journal of Coastal Research},
#' Special Issue, No. 75, pp. 780-785. Coconut Creek (Florida), ISSN 0749-0208.
#'
#' Watson, P.J., 2018. \emph{Improved Techniques to Estimate Mean Sea Level,
#' Velocity and Acceleration from Long Ocean Water Level Time Series to Augment
#' Sea Level (and Climate Change) Research.} PhD Thesis, University of New South
#' Wales, Sydney, Australia.
#'
#' @seealso \code{\link{custom.trend}}, \code{\link{gap.fillview}}, \code{\link{check.decomp}},
#' \code{\link{s}}, \code{\link[stats]{ts}}, \code{\link{msl.fileplot}},
#' \code{\link{msl.screenplot}}, \code{\link{summary}}, \code{\link{Balt}},
#' \code{\link[zoo]{na.approx}}, \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.ma}}.
#'
#' @examples
#'
#' data(Balt) # Baltimore mean sea level record
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
#' \donttest{s <- msl.trend(ts1, fillgaps = 3, iter = 500, 'BALTIMORE, USA')}
#'
#' data(s)
#' str(s) # check structure of object
#' msl.screenplot(s) # check screen output
#'
#' @export
msl.trend <- function(object, station_name = " ", fillgaps = 1, iter = 10000,
                      vlm = " ", plot = TRUE, wdir = " ", save_summary = "TRUE") {
  # -----------------------------------------------------------------
  grDevices::graphics.off()  # close active graphics windows
  # -----------------------------------------------------------------
  # error handling for input file (must be time series object)
  if (class(object) != "ts") {
    stop("input object is not a time series object: program terminated. Check
         manual for input file format requirements.")
  }
  # -----------------------------------------------------------------
  ts1 <- object
  y <- data.frame(V1 = as.vector(stats::time(ts1)), ts1)
  n <- length(ts1)
  n1 <- sum(is.na(ts1))
  L1 <- ceiling(0.2 * n)  # round up to full number window for ssa shape
  p <- 0  # conditioning parameter (assumes missing data to start)
  # -----------------------------------------------------------------
  # error handling
  if (n < 80) {
    # If time series is considered short
    message("Time series less than 80 years in length are not generally recommended
          for analysis. Refer manual for more details.")
  }
  if (min(diff(y[[1]]), na.rm = TRUE) < 1) {
    # If time steps less than annual
    stop("dataset must be an annual time series: program terminated. Check
         manual for input file format requirements.")
  }
  # if gaps larger than 15% of record length, terminate analysis
  if (n1/n > 0.15) {
    # If missing values > 15% stop
    stop("missing values larger than 15% of time series: program terminated. Check
         manual for input file format requirements.")
  }
  w <- y[, 2]  # check that max sequence of NA's is not greater than 5%
  w[is.na(w)] <- 0  # replace NA's with 0
  w <- rle(w)  # sequences (runs of equal values)
  w1 <- max(w$lengths[w$values == 0])  # max sequence of NA's (now 0)
  if (w1/n > 0.05) {
  # Continuous sequence of missing values >5% - gaps too large
    stop("maximum gap lengths too large (> 5% of time series length): program
         terminated. Check manual for input file format requirements.")
  }
  if (is.na(y[1, 2]) == TRUE | is.na(y[n, 2]) == TRUE) {
    # If time series starts or ends with missing values terminate
    stop("cannot fill missing values at start or end of time series : program
         terminated.")
  }
  # -----------------------------------------------------------------
    # if 'fillgaps' not entered set default
  if (fillgaps == 1 | fillgaps == 2 | fillgaps == 3 | fillgaps == 4 |
      fillgaps == 5 | fillgaps == 6) {
    fillgaps <- fillgaps
  } else {
    # If 'fillgaps' not entered or entered outside range
    message("default fillgaps setting applied")
    fillgaps = 1
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
  # where there are no gaps in the record
  # or if fillgaps argument entered when not needed
  if ((any(is.na(y[, 2])) == FALSE) && (fillgaps == 1 | fillgaps == 2 |
                                        fillgaps == 3 | fillgaps == 4 |
                                        fillgaps == 5 | fillgaps == 6)) {
    ts1 <- ts1
    lab1 <- paste("zero")  # number of filled gaps
    lab2 <- paste("NA")  # gap filling method
    message("no missing data in time series, gap-filling not required")
    TSfill <- vector(mode = "numeric", length = n)
    TSfill[TSfill == 0] <- NA
    p <- 1  # conditioning parameter (no gaps in record)
  }
  # -----------------------------------------------------------------
  # fill gaps using SSA (Package 'Rssa')
  if ((fillgaps == 1) && (p == 0)) {
    message("gapfilling using iterative SSA setting applied")
    if (requireNamespace("Rssa", quietly = TRUE)) {
      Rssa::grouping.auto
      Rssa::igapfill
      Rssa::reconstruct
      Rssa::ssa
    }
    if (requireNamespace("tseries", quietly = TRUE)) {
      tseries::na.remove
    }
    lab1 <- n1  # number of filled gaps
    lab2 <- paste("SSA")  # gap filling method
    shape <- Rssa::ssa(ts1, L = L1, kind = "1d-ssa", svd.method = "auto")  # SSA shape
    kk <- sum(shape$sigma > 0.1)
    recon <- Rssa::reconstruct(shape, groups = c(1:kk))
    lt <- NULL
    reps <- kk
    for (i in 1:reps) {
      f <- tseries::na.remove(recon[[i]])
      ss <- stats::spec.pgram(f, spans = 3, detrend = FALSE, log = "no", plot = FALSE)
      freq1 <- ss$freq[ss$spec == max(ss$spec)]  # max freq from max spectogram
      if (freq1 <= 0.2) {
        newel <- i
        lt <- c(lt, newel)
      }
    }
    newTS <- Rssa::igapfill(shape, groups = list(lt))
    ts1 <- newTS
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::na.approx
      zoo::na.spline
    }
    TSfill <- newTS[, 1]  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using SSA (Package 'Rssa') (Forecast onto signal subspace)
  #if ((fillgaps == 6) && (p == 0)) {
  #  message("gapfilling using forecast SSA setting applied")
  #  if (requireNamespace("Rssa", quietly = TRUE)) {
  #    Rssa::grouping.auto
  #    Rssa::gapfill
  #    Rssa::reconstruct
  #    Rssa::ssa
  #  }
  #  if (requireNamespace("tseries", quietly = TRUE)) {
  #    tseries::na.remove
  #  }
  #  lab1 <- n1  # number of filled gaps
  #  lab2 <- paste("SSA Forecast")  # gap filling method
  #  shape <- Rssa::ssa(ts1, L = L1, kind = "1d-ssa", svd.method = "auto")  # SSA shape
  #  kk <- sum(shape$sigma > 0.1)
  #  recon <- Rssa::reconstruct(shape, groups = c(1:kk))
  #  lt <- NULL
  #  reps <- kk
  #  for (i in 1:reps) {
  #    f <- tseries::na.remove(recon[[i]])
  #    ss <- stats::spec.pgram(f, spans = 3, detrend = FALSE, log = "no", plot = FALSE)
  #    freq1 <- ss$freq[ss$spec == max(ss$spec)]  # max freq from max spectogram
  #    if (freq1 <= 0.2) {
  #      newel <- i
  #      lt <- c(lt, newel)
  #    }
  #  }
  #  newTS <- Rssa::gapfill(shape, groups = list(lt), method = "simultaneous")
  #  ts1 <- newTS
  #  if (requireNamespace("zoo", quietly = TRUE)) {
  #    zoo::na.approx
  #    zoo::na.spline
  #  }
  #  TSfill <- newTS[, 1]  # extract TSfill values
  #}
  # -----------------------------------------------------------------
    # fill gaps using linear interpolation from Package 'zoo')
  if ((fillgaps == 2) && (p == 0)) {
    message("gapfilling using linear interpolation setting applied")
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::na.approx
      zoo::na.spline
    }
    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Linear Interpolation")  # gap filling method
    TSfill <- zoo::na.approx(ts1, na.rm = FALSE)
    ts1 <- TSfill
    TSfill <- TSfill[, 1]  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using cubic spline interpolation from Package 'zoo')
  if ((fillgaps == 3) && (p == 0)) {
    message("gapfilling using cubic spline interpolation setting applied")
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::na.approx
      zoo::na.spline
    }
    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Cubic Spline Interpolation")  # gap filling method
    TSfill <- zoo::na.spline(ts1, na.rm = FALSE)
    ts1 <- TSfill
    TSfill <- TSfill[, 1]  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using Stineman interpolation from Package 'imputeTS')
  if ((fillgaps == 4) && (p == 0)) {
    message("gapfilling using Stineman interpolation setting applied")
    if (requireNamespace("imputeTS", quietly = TRUE)) {
      imputeTS::na.interpolation
    }
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::coredata
    }

    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Stineman Interpolation")  # gap filling method
    TSfill <- imputeTS::na.interpolation(ts1, option = "stine")
    ts1 <- TSfill
    TSfill <- zoo::coredata(TSfill)  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using moving average from Package 'imputeTS')
  if ((fillgaps == 5) && (p == 0)) {
    message("gapfilling using weighted moving average setting applied")
    if (requireNamespace("imputeTS", quietly = TRUE)) {
      imputeTS::na.ma
    }
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::coredata
    }

    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Weighted Moving Average")  # gap filling method
    TSfill <- imputeTS::na.ma(ts1)
    ts1 <- TSfill
    TSfill <- zoo::coredata(TSfill)  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # trend isolation using SSA in package (Rssa)
  if (requireNamespace("Rssa", quietly = TRUE)) {
    Rssa::grouping.auto
    Rssa::igapfill
    Rssa::reconstruct
    Rssa::ssa
  }
  model.ssa <- Rssa::ssa(ts1, kind = "1d-ssa", svd.method = "auto")  # default 1D-ssa
  # Auto-detection of trend components from ssa decomposition low frequency interval
  # [0, 0.01], threshold contribution = 0.75, 30 leading components
  ssa.auto <- Rssa::grouping.auto(model.ssa, base = "series", groups = list(1:30),
                            freq.bins = list(0.01), threshold = 0.75)
  recon <- Rssa::reconstruct(model.ssa, groups = ssa.auto)
  trend <- recon$F1
  # -----------------------------------------------------------------
  # fit cubic smooting spline to trend to determine velocity and acceleration
  k <- round((length(ts1)/8), 0)  # DoF for smooth spline (1 every 8 years)
  ss <- stats::smooth.spline(trend, df = k)
  ssVEL <- stats::predict(ss, type = "response", deriv = 1)
  ssVEL <- ssVEL$y  # Velocity vector
  ssACC <- stats::predict(ss, type = "response", deriv = 2)
  ssACC <- ssACC$y  # Acceleration vector
  # -----------------------------------------------------------------
  # determine standard errors
  # remove auto-correlation from residuals
  res <- trend - ts1  # correlated residuals
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
      newTS <- trend + samp
      model2.ssa <- Rssa::ssa(newTS, kind = "1d-ssa", svd.method = "auto")  # 1D-ssa
      ssa2.auto <- Rssa::grouping.auto(model2.ssa, base = "series",
                                       groups = list(1:30), freq.bins = list(0.01),
                                       threshold = 0.75)
      recon2 <- Rssa::reconstruct(model2.ssa, groups = ssa2.auto)
      trend2 <- recon2$F1
      TS[[i]] <- trend2
      ss2 <- stats::smooth.spline(trend2, df = k)
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
    lab4 <- y[ff, 1]  # changepoint year
    for (i in 1:iter) {
      q1 <- noncor[1:ff - 1]
      r1 <- sample(q1, replace = TRUE)
      q2 <- noncor[ff:n]
      r2 <- sample(q2, replace = TRUE)
      samp <- as.numeric(samp <- paste(c(r1, r2)))
      newTS <- trend + samp
      model2.ssa <- Rssa::ssa(newTS, kind = "1d-ssa", svd.method = "auto")  # 1D-ssa
      ssa2.auto <- Rssa::grouping.auto(model2.ssa, base = "series",
                                       groups = list(1:30), freq.bins = list(0.01),
                                       threshold = 0.75)
      recon2 <- Rssa::reconstruct(model2.ssa, groups = ssa2.auto)
      trend2 <- recon2$F1
      TS[[i]] <- trend2
      ss2 <- stats::smooth.spline(trend2, df = k)
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
  summ <- as.data.frame(cbind(y[, 1], y[, 2], as.vector(trend), TSsd, ssVEL, VELsd,
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
  object$Record.Length <- list(Start = y[1, 1], End = y[n, 1], Years = n)
  object$Fillgaps <- list(Gaps = lab1, Method = lab2)
  object$Bootstrapping.Iterations <- iter
  object$Changepoints <- list(Number = lab3, Year = lab4)
  class(object) <- "msl.trend"
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
  if (plot == TRUE | plot == FALSE) {
    plot <- plot
  } else {
    message("default TYPE setting applied")
    plot <- TRUE
  }
  if (summ$Trend[n] - summ$Trend[1] < 0) {
    # check slope of graph for locating chart 1 legends
    laba <- paste("bottomleft")  # Chart key to bottomleft
  } else {
    # default setting
    laba <- paste("bottomright")  # Chart key to bottomright
  }
  # -----------------------------------------------------------------
  if (plot == TRUE) {
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
  if (plot == FALSE) {
    message("no plotting required")
  }
  return(object)
}

#' Inspect gap-filling options for mean sea level records.
#'
#' @param object an annual average mean sea level time series (refer \code{\link[stats]{ts}})
#' with water levels (in millimetres). Missing data must be denoted by \dQuote{NA}.
#' Missing data and maximum missing data gap are limited to 15\% and 5\%, respectively,
#' of the data record. These data input constraints have been based on the research
#' work of Watson (2018) to best preserve the integrity and primary characteristics
#' of the data set for mean sea level analysis. To ensure maximum flexibility for the
#' data analyst, gaps beyond these margins are permitted to be filled but a warning
#' will appear at the console when these advised limits are exceeded.  \cr
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
#'
#' @param station_name character string, providing the name of the data record.
#'
#' \strong{Note: }This field can be left blank, however, it is retained for use
#' in banner labelling of all plotting and pdf outputs.
#'
#' @param fillgaps numeric, provides 5 alternative gap filling procedures for
#' missing data. The following options are available:
#'
#' \itemize{
#'   \item 1: The default procedure is based on iterative gap filling using
#'   Singular Spectrum Analysis (refer \code{\link[Rssa]{igapfill}});
#'   \item 2: linear interpolation (refer \code{\link[zoo]{na.approx}});
#'   \item 3: Cubic spline interpolation (refer \code{\link[zoo]{na.approx}});
#'   \item 4: Stineman's interpolation (refer \code{\link[imputeTS]{na.interpolation}}); and
#'   \item 5: Weighted moving average (refer \code{\link[imputeTS]{na.ma}}).
#' }
#'
#' \strong{Note: }Gap filled portions of the time series are denoted in red on
#' the default screen plot. This is done specifically to provide ready visual
#' observation to discern if the selected gap filling method provides an
#' appropriate estimate within the gaps in keeping with the remainder of the
#' historical record. Depending on the nature of the record and extent of gaps,
#' some trial and error between alternatives might be necessary to optimise gap
#' filling.
#'
#' @details This function permits visual screen checking of various gap-filling
#' options prior to undertaking the full trend analysis (refer \code{\link{msl.trend}}).
#' The returned object can also be used directly as input to the \code{\link{custom.trend}}
#' function.
#'
#' @return An object of class \dQuote{gap.fillview} is returned with the following
#' elements:
#' \describe{
#'  \item{\strong{$Station.Name: }}{the name of the data record.}
#'  \item{\strong{$Summary: }}{a summary data frame of relevant parameters
#'  relating to the inputted annual average data set and filled time series, including:}
#'  \itemize{
#'   \item{$Year: input data; }
#'   \item{$MSL: input data; }
#'   \item{$FilledTS: gap-filled time series. }
#'   }
#' }
#'
#'  \describe{
#'  \item{\strong{$Fillgaps: }}{the procedure used to fill the time series.}
#'  }
#'
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
#' @seealso \code{\link{msl.trend}}, \code{\link[Rssa]{igapfill}},
#' \code{\link[zoo]{na.approx}}, \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.ma}}, \code{\link[stats]{ts}}.
#'
#' @examples
#' # -------------------------------------------------------------------------
#' # View different options for filling the Baltimore annual mean sea level record.
#' # -------------------------------------------------------------------------
#'
#' data(Balt) # Baltimore mean sea level record
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
#'
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 2) # Linear interpolation
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 3) # Cubic spline interpolation
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 4) # Stineman's interpolation
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 5) # Weighted moving average
#'
#' str(g) # Check structure of outputted object
#'
#' @export
gap.fillview <- function(object, station_name = " ", fillgaps = 1) {
  # -----------------------------------------------------------------
  grDevices::graphics.off()  # close active graphics windows
  # -----------------------------------------------------------------
  # error handling for input file (must be time series object)
  if (class(object) != "ts") {
    stop("input object is not a time series object: program terminated. Check
         manual for input file format requirements.")
  }
  # -----------------------------------------------------------------
  ts1 <- object
  y <- data.frame(V1 = as.vector(stats::time(ts1)), ts1)
  n <- length(ts1)
  n1 <- sum(is.na(ts1))
  L1 <- ceiling(0.2 * n)  # round up to full number window for ssa shape
  p <- 0  # conditioning parameter (assumes missing data to start)
  # -----------------------------------------------------------------
  # error handling
  if (n < 80) {
    # If time series is considered short
    message("Time series less than 80 years in length are not generally recommended
          for analysis. Refer manual for more details.")
  }
  if (min(diff(y[[1]]), na.rm = TRUE) < 1) {
    # If time steps less than annual
    stop("dataset must be an annual time series: program terminated. Check
         manual for input file format requirements.")
  }
  # if gaps larger than 15% of record length
  if (n1/n > 0.15) {
    # If missing values > 15% stop
    message("Time series where missing values are larger than 15% of time series
    are not generally recommended for gap filling analysis. Refer manual for
    more details.")
  }
  w <- y[, 2]  # check that max sequence of NA's is not greater than 5%
  w[is.na(w)] <- 0  # replace NA's with 0
  w <- rle(w)  # sequences (runs of equal values)
  w1 <- max(w$lengths[w$values == 0])  # max sequence of NA's (now 0)
  if (w1/n > 0.05) {
    # Continuous sequence of missing values >5% - gaps too large
    message("Time series where maximum gap lengths > 5% of the time series length
    are not generally recommended for gap filling analysis. Refer manual for
    more details.")
  }
  if (is.na(y[1, 2]) == TRUE | is.na(y[n, 2]) == TRUE) {
    # If time series starts or ends with missing values terminate
    stop("cannot fill missing values at start or end of time series : program
         terminated.")
  }
  # -----------------------------------------------------------------
  # if 'fillgaps' not entered set default
  if (fillgaps == 1 | fillgaps == 2 | fillgaps == 3 | fillgaps == 4 |
      fillgaps == 5) {
    fillgaps <- fillgaps
  } else {
    # If 'fillgaps' not entered or entered outside range
    message("default fillgaps setting applied")
    fillgaps = 1
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
  # where there are no gaps in the record
  # or if fillgaps argument entered when not needed
  if ((any(is.na(y[, 2])) == FALSE) && (fillgaps == 1 | fillgaps == 2 |
                                        fillgaps == 3 | fillgaps == 4 |
                                        fillgaps == 5)) {
    ts1 <- ts1
    lab1 <- paste("zero")  # number of filled gaps
    lab2 <- paste("NA")  # gap filling method
    lab6 <- paste("No gap filling required")
    message("no missing data in time series, gap-filling not required")
    TSfill <- vector(mode = "numeric", length = n)
    TSfill[TSfill == 0] <- NA
    p <- 1  # conditioning parameter (no gaps in record)
  }
  # -----------------------------------------------------------------
  # fill gaps using SSA (Package 'Rssa')
  if ((fillgaps == 1) && (p == 0)) {
    message("gapfilling using iterative SSA setting applied")
    if (requireNamespace("Rssa", quietly = TRUE)) {
      Rssa::grouping.auto
      Rssa::igapfill
      Rssa::reconstruct
      Rssa::ssa
    }
    if (requireNamespace("tseries", quietly = TRUE)) {
      tseries::na.remove
    }
    lab1 <- n1  # number of filled gaps
    lab2 <- paste("SSA")  # gap filling method
    lab6 <- paste("Gap filling via SSA")
    shape <- Rssa::ssa(ts1, L = L1, kind = "1d-ssa", svd.method = "auto")  # SSA shape
    kk <- sum(shape$sigma > 0.1)
    recon <- Rssa::reconstruct(shape, groups = c(1:kk))
    lt <- NULL
    reps <- kk
    for (i in 1:reps) {
      f <- tseries::na.remove(recon[[i]])
      ss <- stats::spec.pgram(f, spans = 3, detrend = FALSE, log = "no", plot = FALSE)
      freq1 <- ss$freq[ss$spec == max(ss$spec)]  # max freq from max spectogram
      if (freq1 <= 0.2) {
        newel <- i
        lt <- c(lt, newel)
      }
    }
    newTS <- Rssa::igapfill(shape, groups = list(lt))
    ts1 <- newTS
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::na.approx
      zoo::na.spline
    }
    TSfill <- newTS[, 1]  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using SSA (Package 'Rssa') (Forecast onto signal subspace)
  #if ((fillgaps == 6) && (p == 0)) {
  #  message("gapfilling using forecast SSA setting applied")
  #  if (requireNamespace("Rssa", quietly = TRUE)) {
  #    Rssa::grouping.auto
  #    Rssa::gapfill
  #    Rssa::reconstruct
  #    Rssa::ssa
  #  }
  #  if (requireNamespace("tseries", quietly = TRUE)) {
  #    tseries::na.remove
  #  }
  #  lab1 <- n1  # number of filled gaps
  #  lab2 <- paste("SSA Forecast")  # gap filling method
  #  lab6 <- paste("Gap filling via SSA Forecast")
  #  shape <- Rssa::ssa(ts1, L = L1, kind = "1d-ssa", svd.method = "auto")  # SSA shape
  #  kk <- sum(shape$sigma > 0.1)
  #  recon <- Rssa::reconstruct(shape, groups = c(1:kk))
  #  lt <- NULL
  #  reps <- kk
  #  for (i in 1:reps) {
  #    f <- tseries::na.remove(recon[[i]])
  #    ss <- stats::spec.pgram(f, spans = 3, detrend = FALSE, log = "no", plot = FALSE)
  #    freq1 <- ss$freq[ss$spec == max(ss$spec)]  # max freq from max spectogram
  #    if (freq1 <= 0.2) {
  #      newel <- i
  #      lt <- c(lt, newel)
  #    }
  #  }
  #  newTS <- Rssa::gapfill(shape, groups = list(lt), method = "simultaneous")
  #  ts1 <- newTS
  #  if (requireNamespace("zoo", quietly = TRUE)) {
  #    zoo::na.approx
  #    zoo::na.spline
  #  }
  #  TSfill <- newTS[, 1]  # extract TSfill values
  #}
  # -----------------------------------------------------------------
  # fill gaps using linear interpolation from Package 'zoo')
  if ((fillgaps == 2) && (p == 0)) {
    message("gapfilling using linear interpolation setting applied")
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::na.approx
      zoo::na.spline
    }
    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Linear Interpolation")  # gap filling method
    lab6 <- paste("Gap filling via Linear Interpolation")
    TSfill <- zoo::na.approx(ts1, na.rm = FALSE)
    ts1 <- TSfill
    TSfill <- TSfill[, 1]  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using cubic spline interpolation from Package 'zoo')
  if ((fillgaps == 3) && (p == 0)) {
    message("gapfilling using cubic spline interpolation setting applied")
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::na.approx
      zoo::na.spline
    }
    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Cubic Spline Interpolation")  # gap filling method
    lab6 <- paste("Gap filling via Cubic Spline Interpolation")
    TSfill <- zoo::na.spline(ts1, na.rm = FALSE)
    ts1 <- TSfill
    TSfill <- TSfill[, 1]  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using Stineman interpolation from Package 'imputeTS')
  if ((fillgaps == 4) && (p == 0)) {
    message("gapfilling using Stineman interpolation setting applied")
    if (requireNamespace("imputeTS", quietly = TRUE)) {
      imputeTS::na.interpolation
    }
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::coredata
    }

    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Stineman Interpolation")  # gap filling method
    lab6 <- paste("Gap filling via Stineman Interpolation")
    TSfill <- imputeTS::na.interpolation(ts1, option = "stine")
    ts1 <- TSfill
    TSfill <- zoo::coredata(TSfill)  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # fill gaps using moving average from Package 'imputeTS')
  if ((fillgaps == 5) && (p == 0)) {
    message("gapfilling using weighted moving average setting applied")
    if (requireNamespace("imputeTS", quietly = TRUE)) {
      imputeTS::na.ma
    }
    if (requireNamespace("zoo", quietly = TRUE)) {
      zoo::coredata
    }

    lab1 <- n1  # number of filled gaps
    lab2 <- paste("Weighted Moving Average")  # gap filling method
    lab6 <- paste("Gap filling via Weighted Moving Average")
    TSfill <- imputeTS::na.ma(ts1)
    ts1 <- TSfill
    TSfill <- zoo::coredata(TSfill)  # extract TSfill values
  }
  # -----------------------------------------------------------------
  # summary dataframe
  summGAP <- as.data.frame(cbind(y[, 1], y[, 2], TSfill))
  colnames(summGAP) <- c("Year", "MSL", "FilledTS")
  # -----------------------------------------------------------------

  # plot routines

  if (summGAP$MSL[n] - summGAP$MSL[1] <= 0) {
    # check slope of graph for locating chart 1 legends
    laba <- paste("bottomleft")  # Chart key to bottomleft
    labf <- paste("topright")  # Gap-filling label to topright
  } else {
    # default setting
    laba <- paste("bottomright")  # Chart key to bottomright
    labf <- paste("topleft")  # Gap-filling label to topleft
  }
    # set plotting limits
    if (requireNamespace("plyr", quietly = TRUE)) {
      plyr::round_any
    }
    if (n < 100) {
      # setting up x-axis plotting parameters
      xtic = 10  # year ticks on x-axis
      xlo <- plyr::round_any(min(summGAP[, 1]), 10, floor)
      xhi <- plyr::round_any(max(summGAP[, 1]), 10, ceiling)
    } else {
      # default
      xtic = 20
      xlo <- plyr::round_any(min(summGAP[, 1]), 20, floor)
      xhi <- plyr::round_any(max(summGAP[, 1]), 20, ceiling)
    }
    xlim = c(xlo, xhi)
    # -----------------------------------------------------------------
    # set y-axis scale
    if (p == 0) {
      # gap filling routine required
      M1 <- max(max(summGAP[, 2], na.rm = TRUE), max(summGAP[, 3]))
      M2 <- min(min(summGAP[, 2], na.rm = TRUE), min(summGAP[, 3]))
    }
    if (p == 1) {
      # no gaps in record
      M1 <- max(summGAP[, 2])
      M2 <- min(summGAP[, 2])
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
    graphics::plot(summGAP$Year, summGAP$MSL, type = "l", xaxt = "n", yaxt = "n", lty = 0,
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
      graphics::legend(laba, bg = "white", legend = c("KEY", "Annual Average Data","Gap Filling"),
                       text.font = c(2, 1, 1), lty = c(0, 1, 1),
                       lwd = c(1, 2, 2), col = c("black", "black", "red"),
                       cex = c(1.0, 1.0, 1.0))
      graphics::legend(labf, legend = lab6, bty = "n", text.font = 2, cex = 1.1)
      graphics::lines(summGAP$Year, summGAP$FilledTS, col = "red", lwd = 1.5)
      graphics::lines(summGAP$Year, summGAP$MSL, col = "black", lwd = 1.5)
    }
    # -----------------------------------------------------------------
    if (p == 1) {
      # no gaps in record
      graphics::legend(laba, bg = "white", legend = c("KEY", "Annual Average Data"),
                       text.font = c(2, 1), lty = c(0, 1), lwd = c(1, 2),
                       col = c("black", "black"), cex = c(1.0, 1.0))
      graphics::legend(labf, legend = lab6, bty = "n", text.font = 2, cex = 1.1)
      graphics::lines(summGAP$Year, summGAP$MSL, col = "black", lwd = 1.5)
    }
    # -----------------------------------------------------------------
    object <- NULL

    object$Station.Name <- lab5
    object$Summary <- summGAP
    object$Fillgaps <- lab6
    class(object) <- "gap.fillview"
    return(object)
  }

#' Summary outputs of decomposed time series.
#'
#' @param object of class \dQuote{msl.trend} (see \code{\link{msl.trend}}) or
#' \dQuote{custom.trend} (see \code{\link{custom.trend}}).
#'
#' @param wdir character string, providing the name of the directory to send
#' output files (e.g., \dQuote{C:/myproject/}) when the save_summary argument is set to "TRUE".
#' If this field is left blank the save_summary argument is switched off and a message
#' will be sent to the console.
#'
#' @param save_summary logical, if \dQuote{TRUE} the printed summary of the
#' returned object is exported direct to the working directory and saved
#' as "summary_information.txt". Default = \dQuote{FALSE}.
#'
#' @details This routine provides a screen summary of the respective outputs
#' from a \code{\link{msl.trend}} or \code{\link{custom.trend}} object. The
#' summary produced is identical to str( ) for an object of class
#' \dQuote{msl.trend} (see \code{\link{msl.trend}}) or \dQuote{custom.trend}
#' (see \code{\link{custom.trend}}).
#'
#' @seealso \code{\link{msl.trend}}, \code{\link{custom.trend}},
#' \code{\link{Balt}}, \code{\link{s}}, \code{\link{t}}.
#'
#' @examples
#'
#' data(s) # msl.trend object
#' data(t) # custom.trend object
#' summary(s) # summary for object of class 'msl.trend'
#' summary(t) # summary for object of class 'custom.trend'
#'
#' @export
summary <- function(object, wdir = " ", save_summary = "FALSE") {
    # -----------------------------------------------------------------
    # summary for msl.trend or custom.trend objects
    # -----------------------------------------------------------------
    # If not msl.trend or custom.trend object
    if (class(object) == "msl.trend" | class(object) == "custom.trend") {
        class(object) <- class(object)
    } else {
        stop("object is not an msl.trend or custom.trend object: no summary
             available")
    }
    # -----------------------------------------------------------------
    print(utils::str(object))
    # -----------------------------------------------------------------

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
    message("summary_information.txt file saved to defined directory")
    file_name <- paste0(wdir,"summary_information.txt")  # output file name
    capture.output(str(object), file = file_name)
  }

}

#' @importFrom changepoint cpt.var cpts
NULL

#' @importFrom forecast auto.arima
NULL

#' @importFrom plyr round_any
NULL

#' @importFrom Rssa grouping.auto igapfill reconstruct ssa
NULL

#' @importFrom tseries na.remove
NULL

#' @importFrom zoo na.approx na.spline coredata
NULL

#' @importFrom grDevices dev.off graphics.off pdf
NULL

#' @importFrom graphics abline axis legend lines par plot polygon rect title
NULL

#' @importFrom stats predict sd smooth.spline spec.pgram ts
NULL

#' @importFrom utils capture.output read.csv str write.csv
NULL

#' @importFrom imputeTS na.ma na.interpolation
NULL
