#' @title Daily sleep estimate
#'
#' @description Obtain sleep data from accelerometer data
#'
#' @param datain input accelerometry dataset, must be tibble, data frame, etc.
#' @param f cost function indicator
#' @param id subject id
#' @param Y_name column name of the activity count data to be used in analysis, for example max count of x, y, z axes or vector magnitude
#' @param T threshold percentile of activity level
#' @param nonwear_detect flag indicating use of nonwear detection algorithm, default is FALSE
#' @param S number of segments per hour
#' @param NoPA_cut percent of 0 counts to define NoPA segments
#' @param sleep_mins threshold number of minutes to define sleep segment
#' @param wake_mins threshold number of minutes to define wake segment
#' @param wear_mins threshold number of minutes to define wear segment
#' @param nap_mins threshold number of minutes to define nap segment
#' @param UseDiary flag indicating if diary data to be used, default is FALSE
#' @param diary_data diary data, must be a data frame
#' @param CommonBedTime in-bed time if no diary data, default is "22:00:00"
#' @param CommonWakeTime out-bed time if no diary data, default is "8:00:00"
#' @param tz timezone, default is GMT
#'
#' @importFrom utils head
#'
#' @return list containing a data frame of summary sleep data
#' @export
#'
#' @examples
#' data("AccelData")
#'
#' AccelData <-
#' ChangeTimeVar(AccelData, col_idx = 1, format = "%m/%d/%Y %H:%M")
#'
#' SleepEstEachDay(AccelData, Y_name = "VM")
#'
#' @examples
#' data("AccelData")
#'
#' AccelData <-
#' ChangeTimeVar(AccelData, col_idx = 1, format = "%m/%d/%Y %H:%M")
#'
#' SleepEstEachDay(AccelData, Y_name = "VM", nonwear_detect = TRUE)
#'
#' @examples
#' data("AccelData")
#' data("SleepDiary1Day")
#'
#' SleepDiary1Day <-
#' ChangeTimeVar(
#' SleepDiary1Day,
#' col_idx = c(1,2),
#' format = "%m/%d/%Y %H:%M")
#'
#' AccelData <-
#' ChangeTimeVar(AccelData, col_idx = 1, format = "%m/%d/%Y %H:%M")
#'
#' SleepEstEachDay(
#' AccelData,
#' f = 2,
#' Y_name = "VM",
#' T = 0,
#' nonwear_detect = TRUE,
#' S = 2,
#' NoPA_cut = 0.45,
#' sleep_mins = 5,
#' UseDiary = TRUE,
#' diary_data = SleepDiary1Day
#' )
#'
SleepEstEachDay <- function(
    datain,
    f = 1,
    id = NA,
    Y_name = "max_count",
    T = 0.4,
    nonwear_detect = FALSE,
    wear_mins = 120,
    S = 3,
    NoPA_cut = 0.7,
    sleep_mins = 20,
    wake_mins = 180,
    nap_mins = 20,
    UseDiary = FALSE,
    diary_data = c(),
    CommonBedTime = "22:00:00",
    CommonWakeTime = "8:00:00",
    tz = "GMT")
{
  # Convert dataset to data frame if it is not one already
  datain <- data.frame(datain)

  # Format time variable
  # dates <- ChangeTimeVar(datain, col_idx = 1, format = "%m/%d/%Y %H:%M")
  dates <- ymd_hms(datain$date, tz = tz)
  dates <- with_tz(dates, tzone = tz)
  print(head(dates))

  # Throw error if dataset not successfully converted to data frame
  if(!is.data.frame(datain)) stop("Dataset is not a data frame")

  # Throw error if empty data given in datain
  if(nrow(datain) == 0) stop("Empty dataset detected")

  # Throw error if diary usage indicator and presence of diary data are mismatched
  if((UseDiary && is.null(diary_data)) || (!UseDiary && !is.null(diary_data)))
  {stop("Check that diary usage indicator and input (or lack of) diary data match")}

  if(f < 0 ||
     T < 0 ||
     wear_mins < 0 ||
     S < 0 ||
     NoPA_cut < 0 ||
     sleep_mins < 0 ||
     wake_mins < 0 ||
     nap_mins < 0)
  {stop("Check that all numeric parameters are nonnegative numbers")}

  # If diary data not used, use default values
	if(UseDiary == FALSE)
	  {
		SI_i <- GetEstSleepInterval_subj(
		  datain,
		  bed = CommonBedTime,
		  wake = CommonWakeTime,
		  tz = tz)
	  }
  # If using diary data, import them
	if(UseDiary == TRUE) SI_i <- diary_data

	# Throw error if diary data is empty
	if(nrow(SI_i) == 0) stop("No diary data available")

	# Validate wear time
	valid_accel_data = 1
	if(nonwear_detect == TRUE)
	  {
		epoch_secs <- difftime(datain$date[2], datain$date[1], units = "secs")

		if(epoch_secs < 60)
		  {dat_min <- aggregate_dat(datain, cutnum = 1, unit = "min")}

		if(epoch_secs == 60) dat_min = datain
		N_min <- nrow(dat_min)
		N_wear <- sum(weartime(dat_min[,Y_name], window = wear_mins))
		if(N_min != N_wear) valid_accel_data = 0
	}

	# Person-specific activity level threshold
	T_i <- quantile(datain[,Y_name], prob = T)

	# Person-specific outcomes with invalid sleep counts removed
	Y <- datain[,Y_name] * round(datain[,Y_name] > T_i)
	N <- length(Y)

	# Calculate the total number of segments based on S (segments/hour)
	timelength <- difftime(dates[N], dates[1], units = "hours")
	K <- as.numeric(round(timelength * S))

	# Segment data depending on specified cost function
	if(valid_accel_data)
	  {
		if(f == 1 | f == 3)
		  {print("Flag A"); out <- Segmentor(data = Y, model = f, Kmax = K, compress = FALSE); print("Flag B")}

		if(f == 2)
		  {
		  out <- Segmentor(
		    data = (Y - mean(Y)) / sd(Y),
		    model = f,
		    Kmax = K,
		    compress = FALSE)
		  }

		if(f == 4)
		  {
			v <- c(0, diff(Y))
			std_v <- (v - mean(v)) / sd(v)
			out <- Segmentor(data = std_v, model = f, Kmax = K, compress = FALSE)
			}

		# Retrieve sleep segments
		cpts <- getBreaks(out)[K, 1:(K - 1)]
		idx_start <- unique(c(1, cpts))
		idx_end <- c(idx_start[-1], N)

		sleep_seg <- SearchSleepSeg(
		  dates,
		  Y,
		  idx_start,
		  idx_end,
		  NoPA_cut,
		  sleep_mins,
		  SI_i,
		  tz = tz)

		# Identify segments where external info and estimate overlap
		idx_sleep_overlap <- which(
		  sleep_seg$dat_short$overlap == 1 &
		  sleep_seg$dat_short$Sleep == 1)

		# Identify segments where external info and estimate do not overlap
		idx_sleep_nonoverlap <- which(
		  sleep_seg$dat_short$overlap == 0 &
		  sleep_seg$dat_short$Sleep == 1 &
		  sleep_seg$dat_short$Intervals_mins > nap_mins)

		# Count of estimated sleep intervals with overlap with external info
		n_overlap = length(idx_sleep_overlap)

		# Count of estimated sleep intervals with no overlap with external info
		n_nonoverlap <- length(idx_sleep_nonoverlap)

		if(n_overlap > 0)
		  {
		  sleep_tab <- data.frame(
		    id = id,
		    valid_accel = valid_accel_data,
		    df = K,
		    days = wday(dates[1], label = TRUE),
		    sleep_seg$dat_short[idx_sleep_overlap,],
		    SI_bed = SI_i$bed,
		    SI_wake = SI_i$wake,
		    n_sleep = n_overlap)
		  }

		if(n_overlap == 0 & n_nonoverlap > 0)
		  {
			sleep_tab <- data.frame(
			  id = id,
			  valid_accel = valid_accel_data,
			  df = K,
			  days = wday(dates[1], label = TRUE),
			  sleep_seg$dat_short[idx_sleep_nonoverlap,],
			  SI_bed = SI_i$bed,
			  SI_wake = SI_i$wake,
			  n_sleep = n_overlap)
			}

		if(n_overlap == 0 & n_nonoverlap == 0)
		  {
			tmp_sleep <- data.frame(
			  ts_start <- as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
			  ts_end <- as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
			  Intervals_mins = NA,
			  PctZero = NA,
			  NoPA_cut = NoPA_cut,
			  Sleep = NA,
			  NoPA = NA,
			  overlap = NA)

			sleep_tab <- data.frame(
			  id = id,
			  valid_accel = valid_accel_data,
			  df = K,
			  days = wday(dates[1], label = TRUE),
			  tmp_sleep,
			  SI_bed = SI_i$bed,
			  SI_wake = SI_i$wake,
			  n_sleep = n_overlap)
			}
	}

	else
	  {
		tmp_sleep <- data.frame(
		  ts_start <- as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
		  ts_end <- as.POSIXct(NA, tz = "GMT", origin ="1970-01-01"),
		  Intervals_mins = NA,
		  PctZero = NA,
		  NoPA_cut = NA,
		  Sleep = NA,
		  NoPA = NA,
		  overlap = NA)

		sleep_tab <- data.frame(
		  id = id,
		  valid_accel = valid_accel_data,
		  df = NA,
		  days = wday(dates[1], label=TRUE),
		  tmp_sleep,
		  SI_bed = SI_i$bed,
		  SI_wake = SI_i$wake,
		  n_sleep = NA)
		}

	sleep_tab$ts_start <- as.character(sleep_tab$ts_start)
	sleep_tab$ts_end <- as.character(sleep_tab$ts_end)

	idx <- which(nchar(sleep_tab$ts_start) == 10)
	sleep_tab$ts_start[idx] <- paste(sleep_tab$ts_start[idx], "00:00:00")
	idx <- which(nchar(sleep_tab$ts_end) == 10)
	sleep_tab$ts_end[idx] <- paste(sleep_tab$ts_end[idx], "00:00:00")

	print(sleep_tab)

	return(list(summary = sleep_tab))
}


#' @title Estimated sleep interval
#'
#' @description Obtain in-bed and out-bed times from diary data
#'
#' @param datain diary data
#' @param bed default in-bed time
#' @param wake default out-bed time
#' @param tz timezone, default is GMT
#'
#' @return data frame containing in-bed and out-bed times based on sleep diary
#'
GetEstSleepInterval_subj <- function(
    datain,
    bed = "22:00:00",
    wake = "8:00:00",
    tz = "GMT")
  {
  date_formatted <- mdy_hm(datain$date)
  if (is.null(date_formatted)) {
    print("ERROR: No column named date detected")
  }
  # Find child's diary info
  date_names <- names(table(date_formatted))
  n_date <- length(date_names)
  bed_char <- paste(date_names[-n_date], bed)
  wake_char <- paste(date_names[-1], wake)

  bed_time <- strptime(bed_char, "%Y-%m-%d %H:%M:%S", tz = tz)
  wake_time <- strptime(wake_char, "%Y-%m-%d %H:%M:%S", tz = tz)

  sleep_interval <- data.frame(bed = bed_time, wake = wake_time)
  return(sleep_interval)
  }


#' @title Find specific sleep segment
#'
#' @description Obtain specific sleep segment based on day, indices, etc.
#'
#' @param date date of interest
#' @param Y vector containing validated sleep counts
#' @param idx_start start index of sleep segment
#' @param idx_end end index of sleep segment
#' @param NoPA_cut percent of 0 counts to define NoPA segments
#' @param sleep_mins threshold number of minutes to define sleep segment
#' @param SI_i estimated sleep intervals based on sleep diary data
#' @param tz timezone, default is GMT
#'
#' @return list of validated sleep segments
#'
SearchSleepSeg <- function(
    date,
    Y,
    idx_start,
    idx_end,
    NoPA_cut = 0.7,
    sleep_mins = 30,
    SI_i,
    tz = "GMT")
{
  time_intervals <- difftime(date[idx_end], date[idx_start],  units = "mins")

  PctZero <- sapply(
    X = 1:length(idx_start),
    cal_mean_idx,
    I(Y == 0),
    idx_start,
    idx_end)

  NoPA0 <- round(PctZero >= NoPA_cut)

  dat <- data.frame(
    ts_start = date[idx_start],
    ts_end = date[idx_end],
    Intervals_mins = time_intervals,
    PctZero = PctZero,
    NoPA = NoPA0)

  cp_NoPA0 <- rle(NoPA0)
  n_cp_NoPA <- length(cp_NoPA0$lengths)
  idx_end2 <- idx_end[cumsum(cp_NoPA0$lengths)]
  idx_start2 <- c(1, idx_end2[-n_cp_NoPA])

  t_int <- difftime(date[idx_end2], date[idx_start2],  units = "mins")

  PctZero_NoPA <- sapply(
    X = 1:n_cp_NoPA,
    cal_mean_idx,
    I(Y == 0),
    idx_start2,
    idx_end2)

  NoPA <- round(PctZero_NoPA >= NoPA_cut)
  Sleep_NoPA <- round(t_int >= sleep_mins) * round(PctZero_NoPA >= NoPA_cut)

  dat2 <- data.frame(
    ts_start = date[idx_start2],
    ts_end = date[idx_end2],
    Intervals_mins = t_int,
    PctZero = PctZero_NoPA,
    NoPA_cut = NoPA_cut,
    Sleep = Sleep_NoPA,
    NoPA = NoPA)

  # Find time intervals that overlap w/ an external time interval
  int2 <- interval(SI_i$bed, SI_i$wake, tzone = tz)
  dat2$overlap = 0
  for(j in 1:nrow(dat2)){
    int1 <- interval(dat2[j,1], dat2[j,2], tzone = tz)
    dat2$overlap[j] <- intersect(int1, int2)
  }
  dat2$overlap[!is.na(dat2$overlap)] = 1
  dat2$overlap[is.na(dat2$overlap)] = 0

  return(list(dat_long = dat, dat_short = dat2))
}


#' @title Aggregate Accelerometer Data
#'
#' @description Obtain table from raw accelerometer table in 1-min intervals
#'
#' @param datain raw ActiGraph GT3X data
#' @param cutnum quantity of unit to serve as interval, default is 1
#' @param unit time unit to use
#' @param tz timezone, default is GMT
#'
#' @return data frame of accelerometer data aggregated by intervals
#' @export
#'
aggregate_dat <- function(
    datain,
    cutnum = 1,
    unit = c("min", "sec"),
    tz = "GMT")
  {
  dat <- aggregate(
    . ~ cut(dates, paste(cutnum, unit)),
    datain[setdiff(names(datain), "date")],
    sum)
  names(dat)[1] = "date"
  dat$date <- strptime(dat$date, "%Y-%m-%d %H:%M:%S", tz = tz)
  return(dat)
  }


#' @title Calculate Mean Index
#'
#' @description Calculate the mean index of a segment
#'
#' @param i segment index
#' @param datain activity data, must be a data frame
#' @param idx_start array of segment start indices
#' @param idx_end array of segment end indices
#'
#' @return mean value of segment indices
#'
cal_mean_idx <- function(i, datain, idx_start, idx_end)
  {
  # Calculate mean index
  dataout <- mean(datain[idx_start[i]:idx_end[i]])
  return(dataout)
  }
