#' Diagnostic tools to inspect SSA decomposition for mean sea level records.
#'
#' @param object an annual average mean sea level time series (refer \code{\link[stats]{ts}})
#' with water levels (in millimetres). Objects of class "gap.fillview" can also be parsed
#' directly to this function (refer \code{\link{gap.fillview}}).
#'
#' \strong{Warning: } Input data files are not permitted to contain missing values
#' in order to perform necessary Singular Spectrum Analysis (SSA) or other spectral
#' functions. If the input files contain missing data, the analysis will be
#' terminated. For this reason, this function permits the direct use of objects
#' from the \code{\link{gap.fillview}} function. Similarly, the analysis will be
#' terminated if input files are less than 30 years in length. Whilst it is not
#' generally recommended to use time series less than 80 years in length for mean
#' sea level analyses (Watson, 2018)(and a warning will appear accordingly), it
#' is recognised that this collection of diagnostic tools are valuable for decomposing
#' and understanding the type of signals present in shorter datasets.
#'
#' @param station_name character string, providing the name of the data record.
#'
#' \strong{Note: }This field can be left blank, however, it is retained for use
#' in banner labelling of plotting and associated outputs.
#'
#' @param option numeric, provides a range of diagnostic tools for inspecting
#' mean sea level time series. Available options include:
#'
#' \itemize{
#'   \item 1: A tabular summary is displayed of the spectral density distribution within
#'   each component from an SSA decomposition of the time series (refer \code{\link[Rssa]{ssa}}
#'   with default settings). Values are based on a spectogram of individual components
#'   (refer \code{\link[stats]{spec.pgram}}). This table can be used to readily
#'   identify the frequency bands associated with the peak spectral density
#'   for each component. Trends are readily identfiable as components in
#'   which the peak spectral density resides in the lowest frequency band.
#'   This tabular summary can be exported direct to the working directory
#'   by setting the "save_file" argument to "TRUE" where the file will be
#'   saved as "spec_summary.csv". Option 1 is the default setting;
#'   \item 2: A tabular summary is displayed based on the spectral density
#'   distribution observed via Option 1 (above) converted to percentages which are
#'   based on the sum of the spectral density for each component. This analysis
#'   can be used to gain an insight into the proportion of energy residing
#'   in each frequency band for each component. This tabular summary can be
#'   exported direct to the working directory by setting the "save_file"
#'   argument to "TRUE" where the file will be saved as "spec_summary_percent.csv";
#'   \item 3: A screen plot of the components from the SSA decomposition.
#'   A tabular summary of the component time series can be exported direct to
#'   the working directory by setting the "save_file" argument to "TRUE" where
#'   the file will be saved as "comps_timeseries.csv";
#'   \item 4: A screen plot of the relative contribution of each component
#'   from the SSA decomposition in the low frequency band [0-0.01]. A tabular summary
#'   of the relative contributions can be exported direct to the working
#'   directory by setting the "save_file" argument to "TRUE" where the file
#'   will be saved as "low_freq_contributions.csv";
#'   \item 5: A screen plot to look at the sensitivity of the smoothing
#'   parameter for the cubic smoothing spline fitted to the trend in order
#'   to estimate mean sea level velocity and acceleration over the
#'   length of the record. The function requires the trend components to
#'   be advised via the "trend" argument and similarly, the degrees of
#'   freedom for the fitted spline to be input via the "DOF" argument.
#'   Defaults are provided for both the "trend" and "DOF" if not supplied.
#'   Refer further details on each argument. A tabular summary
#'   of the time series of both the trend and the fitted smooth spline can
#'   be exported direct to the working directory by setting the "save_file"
#'   argument to "TRUE" where the file will be saved as "trend_smooth.csv";
#' }
#'
#' @param comps numeric, enables the user to specify the number of components to be
#' considered or displayed with this range of diagnostic tools. The default is the
#' maximum number available within the \code{\link[Rssa]{ssa}} function of the
#' \code{\link{Rssa}} package, which in turn is governed by the length of the
#' time series (maximum possible is 50). \strong{This parameter is only
#' used in options 1 to 3.}
#'
#' @param trend numeric, enables the user to select the trend components
#' directly in the form of a single component or multiple components (eg.,
#' c(1) or c(1,2,3)). The default setting is c(1) as the first component will
#' always be trend, however, other components might also have trend characteristics
#' which can be diagnostically observed via options 1 to 4. \strong{This parameter is only
#' used in option 5.}
#'
#' @param DOF numeric, enables the user to optimise the degrees of freedom
#' for the fitted cubic smoothing spline applied to the trend. The default
#' setting is based on 1 degree of freedom every 8 years (Watson, 2018) and
#' this default is written to the console to enable the user to directly compare
#' with manually entered DOF. \strong{This parameter is only used in option 5.}
#'
#' @param wdir character string, providing the name of the directory to send
#' output files (e.g., \dQuote{C:/myproject/}) when the save_file argument is set to "TRUE".
#' If this field is left blank the save_file argument is switched off and a message
#' will be sent to the console.
#'
#' @param save_file logical, if "TRUE". Default setting is "FALSE". Refer individual
#' option setting for detail on the respective files that are saved.
#'
#' @details This function provides a range of visual diagnostic tools to screen
#' check SSA decomposition of the time series prior to undertaking the customised
#' trend analysis (refer \code{\link{custom.trend}}). This function permits
#' inspection of the components from the SSA decomposition to inform
#' selection of appropriate components to comprise the trend and to optimise
#' selection of the degrees of freedom (DOF) for the fitted cubic smoothing
#' spline which estimates velocity and acceleration for use in \code{\link{custom.trend}}.
#'
#' @references Watson, P.J., 2018. \emph{Improved Techniques to Estimate Mean Sea Level,
#' Velocity and Acceleration from Long Ocean Water Level Time Series to Augment
#' Sea Level (and Climate Change) Research.} PhD Thesis, University of New South
#' Wales, Sydney, Australia.
#'
#' @seealso \code{\link{custom.trend}}, \code{\link[stats]{ts}}, \code{\link{gap.fillview}},
#' \code{\link[Rssa]{ssa}}, \code{\link[stats]{spec.pgram}}.
#'
#' @examples
#' # -------------------------------------------------------------------------
#' # View application of different diagnostic tools for Baltimore mean sea level record.
#' # -------------------------------------------------------------------------
#'
#' data(Balt) # Baltimore mean sea level record
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
#'
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA filled gap
#'
#' check.decomp(g, option = 3) # check screen plot, default settings
#' check.decomp(g, option = 3, comps = 10) # check screen plot
#' check.decomp(g, option = 4) # check screen plot
#' check.decomp(g, option = 5) # check screen plot, default settings
#' check.decomp(g, option = 5, trend = c(1,2), DOF = 20) # check screen plot
#' check.decomp(g, option = 5, trend = c(1,2,3), DOF = 30) # check screen plot
#'
#' @export
check.decomp <- function(object, station_name = " ", option = 1, comps = " " , trend = c(1), DOF = " ", wdir = " ", save_file = "FALSE") {
  # -----------------------------------------------------------------
  grDevices::graphics.off()  # close active graphics windows
  # -----------------------------------------------------------------
  # error handling for input file (must be time series or gap.fillview object)
  if (class(object) == "ts") {

    df_diag <- data.frame(V1 = as.vector(stats::time(object)), object)

  } else if (class(object) == "gap.fillview") {

    object <- object$Summary; df_diag <- data.frame(cbind(object$Year,object$FilledTS)); names(df_diag) <- c("V1","V2")

  } else {
    stop("input object is not a time series or gap.fillview object: program terminated. Check
         manual for input file format requirements.")
  }
  # -----------------------------------------------------------------
  # error handling (missing values)
  if (anyNA(df_diag[,2]) == TRUE ) {
    # If there are missing values terminate
    stop("Missing values are not permitted within this suite of diagnostic tools: program
         terminated. Check manual for input file format requirements and where necessary
         fill missing values using the gap.fillview function in this package.")
  }
  # -----------------------------------------------------------------
  # error handling (time series too short < 30 years)
  if (length(df_diag[,2]) < 30) {
    # If time series is considered short
    message("Time series less than 30 years in length are considered too short
          for meaningful mean sea level diagnostic analysis and are not permitted within this
          function. Refer manual for more details.")
  }
  # -----------------------------------------------------------------
  # error handling (time series less than annual)
  if (min(diff(df_diag[[1]]), na.rm = TRUE) < 1) {
    # If time steps less than annual
    stop("dataset must be an annual time series: program terminated. Check
         manual for input file format requirements.")
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
    lab10a <- paste0("DOF  = ", DOF_A)

  } else {
    ddd <- 0 # marker for self input DOF
    DOF <- DOF
    lab10b <- paste0("Self input DOF  = ", DOF)

  }
  # -----------------------------------------------------------------
  # if 'option' not entered set default
  if (option == 1 | option == 2 | option == 3 | option == 4 | option == 5) {
    option <- option
  } else {
    # If 'option' not entered or entered outside range
    message("default option setting (1) applied")
    option <- 1
  }
  # -----------------------------------------------------------------
  # set general parameters for Rssa model, comps, etc, prepare summary data frame
  # -----------------------------------------------------------------
  if (requireNamespace("Rssa", quietly = TRUE)) {
    Rssa::reconstruct
    Rssa::ssa
    Rssa::grouping.auto
  }
  if (requireNamespace("stats", quietly = TRUE)) {
    stats::spec.pgram
  }
  t2 <- df_diag[,2] # Column [2] contains the time series of interest (annual filled readings)
  model.ssa <- Rssa::ssa(t2, kind = "1d-ssa", svd.method = "auto")  # default 1D-ssa settings
  len_com <- length(model.ssa$sigma) ## maximum amount of components resolved
  # -----------------------------------------------------------------
  # If comps not entered
  if (comps == " ") {
    comps <- len_com
    lab6a <- paste0("Comps not entered, default setting applied, number = ", len_com)
    message(lab6a)
  } else if (comps > len_com) {
    comps <- len_com
    lab6b <- paste0("Comps greater than max permitted, default setting applied, number = ", len_com)
    message(lab6b)
  } else {
    comps <- comps
  }

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
  # save_file = "FALSE"
  # save_file not entered or entered outside range
  if (save_file == "FALSE" | save_file == "TRUE") {
    save_file <- save_file
  } else {
    message("default save_file = FALSE setting applied")
    save_file <- "FALSE"
  }

  # -----------------------------------------------------------------
  # check wdir and save_file = "TRUE"

  if (TH == 1 && save_file == "TRUE") {
    message("File has not been saved because no working directory was advised. Refer wdir argument and manual for more details.")
  } else if (TH == 2 && save_file == "FALSE"){
    TH <- 1
  }

  # -----------------------------------------------------------------
  # prepare summary data frame
  # -----------------------------------------------------------------
  vec <- NULL

  for (i in 1:comps) {
    vec <- c(vec,i)
  }
  recon <- Rssa::reconstruct(model.ssa, groups = vec)
  ##----------------------------------------------------
  SPEC <- NULL
  LAB  <- NULL
  for (i in 1:comps) {
    t <- recon[[i]]
    ss <- stats::spec.pgram(t, detrend = FALSE, log = "no", plot = FALSE)
    SPEC <- as.data.frame(cbind(SPEC, round(ss$spec,6)))
    lab10 <- paste0("comp",i)
    LAB <- c(LAB, lab10)
  }
  freq <- round(ss$freq,6)
  ret <- 1/ss$freq
  SPEC <- as.data.frame(cbind(freq,ret,SPEC))
  colnames(SPEC) <- c("freq", "period(years)",LAB)

  op <- options()
  on.exit(options(op))
    options(scipen = 999) ## remove scientific notation

  # -----------------------------------------------------------------
  # Option 1: A tabular summary of the spectral density distribution
  # -----------------------------------------------------------------

  if (option == 1) {
    message("Tabular summary of spectral density distribution from SSA decomposition")
    utils::View(SPEC)
  }
  # -----------------------------------------------------------------
  if (option == 1 && TH == 2) {
    message("Tabular summary saved to defined directory as spec_summary.csv")
    utils::write.csv(SPEC, file.path(wdir, "spec_summary.csv"), row.names = FALSE)
  }

  # -----------------------------------------------------------------
  # Option 2: A tabular summary of the spectral density distribution (% of components)
  # -----------------------------------------------------------------

  if (option == 2) {
    message("Tabular summary of spectral density distribution from SSA decomposition as % of component")
    df_perc <- SPEC[, -c(1:2)] ## dataframe of only components
    df_perc <- as.matrix(df_perc) ## convert to matrix for prop.table
    df_perc <- round(prop.table(df_perc, margin = 2)*100,4) ## convert to %
    df_perc <- as.data.frame(df_perc)
    df_perc <- cbind(SPEC[,1], SPEC[,2],df_perc)
    colnames(df_perc) <- c("freq", "period(years)",LAB)
    utils::View(df_perc)
  }
  # -----------------------------------------------------------------
  if (option == 2 && TH == 2) {
    message("Tabular summary saved to defined directory as spec_summary_percent.csv")
    utils::write.csv(df_perc, file.path(wdir, "spec_summary_percent.csv"), row.names = FALSE)
  }

  # -----------------------------------------------------------------
  # Option 3: Display components of SSA decomposition
  # -----------------------------------------------------------------

  if (option == 3) {
    message("Time series display of individual components from SSA decomposition")
    p <- plot(model.ssa, type =  "series", groups = c(1:comps)) ## all groups
    plot(p) # print(p) also works
  }
  # -----------------------------------------------------------------
  if (option == 3 && TH == 2) {
    message("Tabular summary saved to defined directory as comps_timeseries.csv")
    TSER <- NULL
    for (i in 1:comps) {
      t <- round(recon[[i]],3) ## Round 3 decimal points
      TSER <- as.data.frame(cbind(TSER, t))
    }
    TSER <- as.data.frame(cbind(df_diag[,1],TSER))
    colnames(SPEC) <- c("freq", "period(years)",LAB)
    colnames(TSER) <- c("Year",LAB)

    op <- options()
    on.exit(options(op))
    options(scipen = 999) ## remove scientific notation
    utils::write.csv(TSER, file.path(wdir,"comps_timeseries.csv"), row.names = FALSE)
  }

  # -----------------------------------------------------------------
  # Option 4: Display relative contributions of each components of SSA decomposition
  # in low frequency band [0-0.01]
  # -----------------------------------------------------------------

  if (option == 4) {
    message("Relative contributions of each component of SSA decomposition in low frequency band [0-0.01]")
    low_freq <- Rssa::grouping.auto(model.ssa, base = "series", freq.bins = list(0.01))
    p <- plot(low_freq, main = "Frequency band [0 - 0.01]", type = "b")
    plot(p) # print(p) also works
  }
  # -----------------------------------------------------------------
  if (option == 4 && TH == 2) {
    message("Summary saved to defined directory as low_freq_contributions.csv")
    ttt <- attr(low_freq, "contributions")
    ttt <- round(ttt[,1],6)
    attr(ttt,"names")  <- NULL
    ttt_df <- as.data.frame(cbind(low_freq$F1,ttt))
    colnames(ttt_df) <- c("Component", "Contribution")
    utils::write.csv(ttt_df, file.path(wdir,"low_freq_contributions.csv"), row.names = FALSE)
  }

  # -----------------------------------------------------------------
  # Option 5: Display trend and cubic smoothing spline with variable input DOF
  # -----------------------------------------------------------------

  if (option == 5) {

    if (ddd == 1) { # marker for default DOF
      message("Default degrees of freedom applied to fitted cubic smoothing spline based on 1 DOF every 8 years")
      message(lab10a)
    }
    if (ddd == 0) { # marker for self input DOF
      message("Default degrees of freedom applied to fitted cubic smoothing spline based on 1 DOF every 8 years")
      message(lab10a)
      message(lab10b)
    }

    trd <- Rssa::reconstruct(model.ssa, groups = list(trend)) ## trend components from input, default = c(1)
    trd <- trd$F1
    ss_tr <- smooth.spline(trd, df = DOF)
    ssTS <- predict(ss_tr, type = "response", deriv = 0)
    ssTS <- ssTS$y
    tr_df <- as.data.frame(cbind(df_diag[,1],trd,ssTS))
    colnames(tr_df) <- c("Year","Trend","Smooth")
    plot(tr_df$Year, tr_df$Trend, type = "l", xlab = "Year", ylab = "Mean Sea Level")
    lines(tr_df$Year, tr_df$Smooth, col = "red")
    legend('bottomright', bg = "white", legend = c("KEY", "Trend", "Smooth Spline"),
           text.font = c(2, 1, 1), lty = c(0, 1, 1), col = c("black", "black", "red"), cex = c(0.8,0.8,0.8))
  }
  # -----------------------------------------------------------------
  if (option == 5 && TH == 2) {
    message("Summary file of trend and smoothed spline with inpoted DOF saved to defined directory as trend_smooth.csv")
    utils::write.csv(tr_df, file.path(wdir,"trend_smooth.csv"), row.names = FALSE)
  }

}

