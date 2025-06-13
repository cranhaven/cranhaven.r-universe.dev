#' Get list of weather stations from NOAA
#'
#' Download the list of all available weather stations from NOAA, or only those for
#' a specific country and period
#'
#' @param force_retrieve If \code{TRUE} download the list again from NOAA (even if
#'   it was already downloaded previously). Defaults to \code{FALSE}, so that download
#'   happens only once per session.
#' @param fips 2-letter country FIPS ID (full list of codes at
#'   \url{ftp://ftp.ncdc.noaa.gov/pub/data/gsod/country-list.txt}).
#' @param from Lower limit of reporting period (as class \code{Date}). Only retrieve
#'   stations whose period of record ends at or after this date.
#' @param to Upper limit of reporting period (as class \code{Date}). Only retrieve
#'   stations whose period of record begins at or before this date.
#'
#' @details \code{NOAA_allStations()} downloads the list of all available weather
#' stations from NOAA, found in
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv}, and returns it
#' as a \code{data.frame}. The data are downloaded only once per R session,
#' the first time this function is used, and are then stored internally for
#' further retrievals.
#'
#' \code{NOAA_countryStations()} retrieves the list for a specific country only
#' (or several countries, if \code{length(ctry)>1}), and possibly only for a specific
#' period of record.
#'
#' @return A \code{data.frame} with the following columns (copy-pasted from NOAA):
#'   \describe{
#'     \item{usaf}{Air Force station ID. May contain a letter in the first position.}
#'     \item{wban}{NCDC WBAN number}
#'     \item{ctry}{FIPS country ID}
#'     \item{st}{State for US stations}
#'     \item{icao}{ICAO ID}
#'     \item{lat}{Latitude in thousandths of decimal degrees}
#'     \item{lon}{Longitude in thousandths of decimal degrees}
#'     \item{elev.m.}{Elevation in meters}
#'     \item{begin}{Beginning Period Of Record. There may be reporting gaps within the P.O.R.}
#'     \item{end}{Ending Period Of Record. There may be reporting gaps within the P.O.R.}
#'   }
#'
#' Note that columns \code{begin} and \code{end} in the output are of class \code{Date}.
#'
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#'
#' @export
NOAA_allStations <- function(force_retrieve=FALSE) {
  # `internal_state` is an environment object found in sysdata.rda
  # see data-raw/setup_sysdata.R
  if (is.null(internal_state$NOAAsites) || force_retrieve) {
    message("Downloading list of weather stations from NOAA (waiting for server)... ")
    NOAAsites <- (function() {
      sites <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv",
          stringsAsFactors=FALSE)
      names(sites) <- tolower(names(sites))
      sites
    })()
    message("Download finished.\n")
    # Explicitly set names
    # Formatting and cleaning
      NOAAsites$begin <- as.Date(as.character(NOAAsites$begin), format="%Y%m%d")
      NOAAsites$end <- as.Date(as.character(NOAAsites$end), format="%Y%m%d")
      NOAAsites <- NOAAsites[with(NOAAsites, !(elev.m.<0 & lon==0 & lat==0) & !is.na(lat) & !is.na(lon)),]
      NOAAsites <- NOAAsites[-grep("BOGUS", NOAAsites$station.name),]
      rownames(NOAAsites) <- NULL
    # Saving to `internal_state` environment
      internal_state$NOAAsites <- NOAAsites
  }
  return(internal_state$NOAAsites)
}



#' @rdname NOAA_allStations
#' @export
NOAA_countryStations <- function(fips, from=NULL, to=NULL) {
  sel <- NOAA_allStations()
  sel <- sel[which(sel$ctry %in% fips),]
  if (nrow(sel)==0) stop("No stations found (is argument `fips` wrong?)")
  if (!is.null(from))
    sel <- sel[which(sel$end>=from),]
  if (!is.null(to))
    sel <- sel[which(sel$begin<=to),]
  return(sel)
}



#' Get daily weather summaries from NOAA
#'
#' Downloads the daily weather summaries for a set of weather stations
#' and a set of years.
#'
#' @param stations A \code{data.frame} with the stations for which data are to be
#'   retrieved. It can be a subset of the \code{data.frame}s returned from
#'   \code{\link{NOAA_allStations}} or \code{\link{NOAA_countryStations}}.
#'   At a minimum it should contain columns 'usaf' and 'wban'.
#' @param years An integer vector of years (from 1901 to current year)
#'   for which data are to be retrieved.
#' @param match.columns \code{NULL} or (optionally) a vector of column names that
#'   can be found in \code{stations}. If given, these are included in the output
#'   after matching with the respective weather stations.
#' @param progress If \code{TRUE} (the default), a progress bar appears if more
#'   than three files are to be downloaded from NOAA. Set to \code{FALSE} to suppress
#'   the progress bar.
#'
#'   For example, one can include a grouping variable in \code{stations} (such as
#'   region code) and give its name in \code{match.columns} for it to be included in
#'   the function output. This facilitates aggregating the output by the grouping variable.
#'
#' @return A \code{data.frame} with the following columns (adapted from NOAA):
#'   \describe{
#'     \item{usaf}{Air Force station ID}
#'     \item{wban}{NCDC WBAN number}
#'     \item{date}{Date (of class \code{Date}}
#'     \item{temp}{Mean temperature for the day, in degrees Celsius to tenths.}
#'     \item{tempC}{Number of observations used in calculating mean temperature.}
#'     \item{dewp}{Dew point for the day, in degrees Celsius to tenths.}
#'     \item{dewpC}{Number of observations used in calculating mean dew point.}
#'     \item{slp}{Mean sea level pressure for the day,in millibars to tenths.}
#'     \item{slpC}{Number of observations used in calculating mean sea level pressure.}
#'     \item{stp}{Mean station pressure for the day in millibars to tenths.}
#'     \item{stpC}{Number of observations used in calculating mean station pressure.}
#'     \item{visib}{Mean visibility for the day in miles to tenths.}
#'     \item{visibC}{Number of observations used in calculating mean visibility.}
#'     \item{wdsp}{Mean wind speed for the day in knots to tenths.}
#'     \item{wdspC}{Number of observations used in calculating mean wind speed.}
#'     \item{mxspd}{Maximum sustained wind speed reported for the day, in knots to tenths.}
#'     \item{gust}{Maximum wind gust reported for the day, in knots to tenths.}
#'     \item{maxtemp}{Maximum temperature reported during the day,
#'       in degrees Celsius to tenths. Time of max temp report varies
#'       by country and region, so this will sometimes not be the max for the calendar day.}
#'     \item{maxtempF}{Blank indicates max temp was taken from the explicit max temp report
#'       and not from the 'hourly' data. An asterisk (*) indicates max temp was derived from
#'       the hourly data (i.e., highest hourly or synoptic-reported temperature).}
#'     \item{mintemp}{Minimum temperature reported during the day,
#'       in degrees Celsius to tenths. Time of min temp report varies
#'       by country and region, so this will sometimes not be the min for the calendar day.}
#'     \item{mintempF}{Blank indicates min temp was taken from the explicit min temp report
#'       and not from the 'hourly' data. An asterisk (*) indicates min temp was derived from
#'       the hourly data (i.e., lowest hourly or synoptic-reported temperature).}
#'     \item{prcp}{Total precipitation (rain and/or melted snow) reported during the day,
#'       in inches and hundredths; will usually not end with the midnight observation,
#'       i.e. may include latter part of previous day. Zero indicates no measurable
#'       precipitation (includes a trace).
#'       Note:  Many stations do not report '0' on days with no precipitation, therefore
#'       \code{NA} will often appear on these days. Also, for example, a station may only
#'       report a 6-hour amount for the period during which rain fell. See 'prcpF' field
#'       for source of data.}
#'     \item{prcpF}{A = 1 report of 6-hour precipitation amount.
#'       B = Summation of 2 reports of 6-hour precipitation amount.
#'       C = Summation of 3 reports of 6-hour precipitation amount.
#'       D = Summation of 4 reports of 6-hour precipitation amount.
#'       E = 1 report of 12-hour precipitation amount.
#'       F = Summation of 2 reports of 12-hour precipitation amount.
#'       G = 1 report of 24-hour precipitation amount.
#'       H = Station reported '0' as the amount for the day (eg, from 6-hour reports),
#'       but also reported at least one occurrence of precipitation in hourly observations;
#'       this could indicate a trace occurred, but should be considered as incomplete data
#'       for the day.
#'       I = Station did not report any precip data for the day and did not report
#'       any occurrences of precipitation in its hourly observations; it's still possible
#'       that precip occurred but was not reported.}
#'     \item{sndp}{Snow depth in inches to tenths--last report for the day
#'       if reported more than once. Note:  Most stations do not report '0' on days with
#'       no snow on the ground--therefore, \code{NA} will often appear on these days.}
#'     \item{frshtt}{Indicators (1 = yes, 0 = no/not reported) for the occurrence during
#'       the day of: Fog ('F' - 1st digit); Rain or Drizzle ('R' - 2nd digit); Snow or Ice
#'       Pellets ('S' - 3rd digit); Hail ('H' - 4th digit); Thunder ('T' - 5th digit);
#'       Tornado or Funnel Cloud ('T' - 6th digit).}
#'   }
#'
#'  Note that, compared to the original NOAA output (for details see
#'  \url{ftp://ftp.ncdc.noaa.gov/pub/data/gsod/readme.txt}),
#'  all temperatures are automatically converted to degrees Celsius (instead of Fahrenheit)
#'  and all missing indicators are replaced with \code{NA}s.
#'
#'
#' @importFrom utils download.file
#' @importFrom utils read.fwf
#'
#' @export
NOAA_getGSOD <- function(stations, years, match.columns="station.name", progress=TRUE) {
  if (!is.data.frame(stations))
    stop("argument `stations` must be a data.frame")
  if (sum(c("usaf","wban") %in% names(stations))!=2)
    stop("argument `stations` must have columns 'usaf' and 'wban'")
  if (!is.numeric(years) || length(years)==0)
    stop("argument `years` must be numeric")
  if (sum(years>as.integer(format(Sys.Date(), "%Y")) | years<1901)>0)
    stop(sprintf("all years should range from 1901 to current year (%s)", format(Sys.Date(), "%Y")))

  nstations <- nrow(stations) * length(years)
  if (nstations<=3) progress <- FALSE
  if (progress) {
    i=0
    pb <- txtProgressBar(title = "FluMoDL",
        label = sprintf("Downloading %s files from NOAA via FTP...", nstations),
        min=0, max=nstations, initial=0, style=3)
  }
  t <- tempfile()
  res <- do.call(rbind, lapply(years, function(y) {
    files <- sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/%s/%s-%s-%s.op.gz", y, stations$usaf, stations$wban, y)
    res <- do.call(rbind, lapply(files, function(f) {
      if (progress) { i <<- i+1; setTxtProgressBar(pb, i) }
      suppressWarnings({
        b <- try({download.file(f, t, quiet=TRUE)}, silent=TRUE)
      })
      if (class(b)!="try-error" && b==0) {
        bb <- try(read.fwf(gzfile(t), header=FALSE, skip=1, as.is=TRUE,
                           widths=diff(c(0,6,12,22,30,33,41,44,52,55,63,66,73,76,83,86,93,100,108,109,116,117,123,124,130,138)),
                           col.names=c("usaf", "wban", "date", "temp", "tempC", "dewp", "dewpC", "slp", "slpC", "stp",
                                       "stpC", "visib", "visibC", "wdsp", "wdspC", "mxspd", "gust", "maxtemp", "maxtempF", "mintemp",
                                       "mintempF", "prcp", "prcpF", "sndp", "frshtt"),
                           colClasses=c("usaf"="character")),
                  silent = TRUE)
        if (class(bb)!="try-error") {
          return(bb)
        }
      }
    }))
  }))
  if (exists("pb")) close(pb)
  if (!is.null(res) && nrow(res)>0) {
    # Convert temperatures to celsius
    toCelsius <- function(x, round=1) round((x-32)*5/9, round)
    res$temp <- toCelsius(res$temp)
    res$dewp <- toCelsius(res$dewp)
    res$mintemp <- toCelsius(res$mintemp)
    res$maxtemp <- toCelsius(res$maxtemp)

    # Convert to Date class
    res$date <- as.Date(as.character(res$date), "%Y%m%d")

    # Mark missings
    res$temp[res$temp==9999.9] <- NA
    res$dewp[res$dewp==9999.9] <- NA
    res$slp[res$slp==9999.9] <- NA
    res$stp[res$stp==9999.9] <- NA
    res$visib[res$visib==999.9] <- NA
    res$wdsp[res$wdsp==999.9] <- NA
    res$mxspd[res$mxspd==999.9] <- NA
    res$gust[res$gust==999.9] <- NA
    res$maxtemp[res$maxtemp==9999.9] <- NA
    res$mintemp[res$mintemp==9999.9] <- NA
    res$prcp[res$prcp==99.9] <- NA
    res$sndp[res$sndp==999.9] <- NA

    # Reorder by date and station, and remove rownames
    res <- res[order(res$date, res$usaf),]
    rownames(res) <- NULL
  }

  if (!is.null(match.columns) && length(match.columns)>0) {
    match.columns <- match.columns[which(match.columns %in% names(stations))]
    if (length(match.columns)>0) {
      m <- match(with(res, paste(usaf,wban)), with(stations, paste(usaf,wban)))
      res <- cbind(res, stations[m, match.columns, drop=FALSE])
      rownames(res) <- NULL
    } else {
      warning("no columns in `match.columns` were matched in `stations`")
    }
  }

  return(res)
}
