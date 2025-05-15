### Non-reactive functions used in CruzPlot R Shiny app


#------------------------------------------------------------------------------
# Get last n element(s) from string x
# From https://stackoverflow.com/questions/7963898
substr_right <- function(x, n) substr(x, nchar(x) - n + 1, nchar(x))


#------------------------------------------------------------------------------
# cruzClosestPt for CruzPlot
# Inputs: location of point on map, type of sighting or effort, and data from DAS data
#   type: 1 = mammal sightings (with sighting number), 2 = non-mammal sightings labeled with time,
#   3 = effort R and E locations, 4 = effort
# Returns label postion and text

# Pieces are passed in separately in case column names, etc, change in the future

cruzClosestPt <- function(curr.pt, type, das.lat, das.lon, das.date,
                          das.sightno = NULL, das.cruise = NULL,
                          das.lat2 = NULL, das.lon2 = NULL) {
  stopifnot(type %in% (1:3))

  # Determine the DAS point closest to the map point
  x1 <- abs(das.lon - curr.pt[1])
  y1 <- abs(das.lat - curr.pt[2])

  if (type %in% c(1, 2)) { #sightings
    min.index <- which.min(sqrt(x1^2 + y1^2))

  } else { #effort
    x2 <- abs(das.lon2 - curr.pt[1])
    y2 <- abs(das.lat2 - curr.pt[2])

    d.df <- data.frame(d1 = sqrt(x1^2 + y1^2), d2 = sqrt(x2^2 + y2^2))
    d.min <- apply(d.df, 1, min)
    min.index <- which.min(d.min)
  }

  # After determining closest DAS point, convert longitudes to [-180, 180] for display
  das.lon <- ifelse(das.lon > 180, das.lon - 360, das.lon)
  if (!is.null(das.lon2)) ifelse(das.lon2 > 180, das.lon2 - 360, das.lon2)

  # Extract DAS information
  das.date.val <- das.date[min.index]
  # "Cr:", data.das$Cruise[min.index], "\n",   # no cruise number for vaquita cruise
  lab.date <- paste(format(das.date.val, format = "%d%b%Y"))
  lab.dt <- paste(format(das.date.val, format = "%d%b%Y %H:%M"))

  # Make label to print on map
  if (type == 1) {
    # Marine mammal sighting
    lab <- paste0(
      "Sight# ", as.numeric(das.sightno[min.index]), "\n",
      lab.dt, "\n",
      round(das.lat[min.index], 2), ", ",
      round(das.lon[min.index], 2)
    )

  } else if (type == 2) {
    # Other sighting
    lab <- paste0(
      lab.dt, "\n",
      round(das.lat[min.index], 2), ", ", round(das.lon[min.index], 2)
    )

  } else if (type == 3) {
    # Simplified effort
    pt.st <- round(c(das.lon[min.index], das.lat[min.index]), 2)
    pt.end <- round(c(das.lon2[min.index], das.lat2[min.index]), 2)

    lab <- paste0(
      lab.dt, "\n",
      "R: ", paste(pt.st, collapse = ", "), "\n",
      "E: ", paste(pt.end, collapse = ", ")
    )

  }

  c(x1[min.index], y1[min.index], lab)
}


#------------------------------------------------------------------------------
# cruzSpeciesRead for CruzPlot
#   Input: SpCodes.dat file
#   Returns: data frame containing the species code, abbreviation,
#     scientific name, and common name

cruzSpeciesRead <- function(file) {
  sp.codes <- scan(file, what = character(), sep = "\n", quiet = TRUE)
  Code <- str_trim(substring(sp.codes, 1, 4), side = "both")
  Abbr <- str_trim(substring(sp.codes, 6, 17), side = "both")
  Name_Scientific <- str_trim(substring(sp.codes, 18, 57), side = "both")
  Name_Common <- str_trim(substring(sp.codes, 58), side = "both")

  data.frame(Code, Abbr, Name_Scientific, Name_Common, stringsAsFactors = FALSE)
}


#------------------------------------------------------------------------------
# funcTickMinor for CruzPlot
#   Inputs: range of figure, location of major tick intervals,
#     width of major tick intervals, number of minor tick marks
#   Returns: vector with locations of the minor tick marks

cruzTickMinor <- function (deg.range, maj.ticks, tick.maj.interval, n=2) {
  sep <- tick.maj.interval / (n+1)
  min.ticks1 <- seq(maj.ticks[1], deg.range[2], by = sep)
  min.ticks2 <- rev(seq(maj.ticks[1], deg.range[1], by = -sep))
  min.ticks <- c(min.ticks2[1:length(min.ticks2)-1], min.ticks1)

  min.ticks
}


#------------------------------------------------------------------------------
# funcTickStart for CruzPlot
#   Input: vector of either longitude or latitude range and major tick interval
#   Returns: start location based on lat/lon input and length of tick interval

cruzTickStart <- function(l.range, b) {
  l.start <- ifelse(l.range[1]%%b > 0, l.range[1] + b - l.range[1]%%b, l.range[1])
  if(!(l.range[1] < l.start && l.start < l.range[2])) l.start <- l.range[1]

  l.start
}


#------------------------------------------------------------------------------
# cruzTickUpdate for CruzPlot
#   Returns default starter value for major tick interval based on the longitude and latitude range

cruzTickUpdate <- function(lon.range, lat.range) {
  lon.diff <- abs(lon.range[2] - lon.range[1])
  lat.diff <- abs(lat.range[2] - lat.range[1])
  tick.breaks <- c(0,2,5,10,40,75,120,361)
  tick.interval <- c(0.5, 1, 2, 5, 10, 15, 30)

  lon.tick.interval <- tick.interval[cut(lon.diff, breaks = tick.breaks, labels = tick.interval)]
  lat.tick.interval <- tick.interval[cut(lat.diff, breaks = tick.breaks, labels = tick.interval)]
  tick.maj.interval <- max(lon.tick.interval, lat.tick.interval)

  tick.maj.interval
}

#------------------------------------------------------------------------------
