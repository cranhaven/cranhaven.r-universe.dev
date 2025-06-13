#' @include global_var.R
NULL

#' Line graph for a single continuous variable.
#' 
#' @description Provides a "quick-and-dirty" approach to plotting a line graph 
#'     for a single continuous variable using defaults for axis and title 
#'     labels.  Users can specify if they want to look at an aggregate of a 
#'     variable over the course of a day (avg_to_get_typical_day = TRUE) or look
#'     at that variable at every interval (i.e. every 15 minutes for the entire 
#'     date range).
#' 
#' @param person An instance of the Person class
#' @param measure_var character vector denoting the variables of interest.
#'     Options are one or more of: "steps", "floors", "distance", 
#'     "caloriesBurned","bpm" (heart rate), "weight".
#' @param avg_to_get_typical_day Logical variable "daily" for an aggregate of 
#'     the variable over the course of a day, or "intraday" for the variable at 
#'     every interval over the range.  Default is TRUE.
#' 
#' @return ggplot object
#' @export
#' @importFrom stats complete.cases
#' 
#' @examples
#' data(EX)
#' plot_i(EX, "steps")
#' plot_i(EX, "distance", FALSE)
plot_i <- function(person, measure_var, avg_to_get_typical_day = TRUE) {
  if (avg_to_get_typical_day) {
    
    # Pull and wrangle data
    data <- person$fitbit_intraday[ , c("time", measure_var)]
    data <- dplyr::group_by(data, time)
    data <- dplyr::summarize_(data, avg = 
                                stringr::str_c("mean(", measure_var, ", na.rm = TRUE)"))
    data <- data[stats::complete.cases(data), ]

    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = data$time, y = data$avg)) +
      ggplot2::geom_step(color = CARDINAL)  +
      ggplot2::scale_x_datetime(date_labels = "%H:%M %p", 
                                date_breaks = "3 hours") +
      ggplot2::labs(x = "Time of Typical Day",
                    title = stringr::str_c(
                      "Average",
                      stringr::str_to_title(measure_var),
                      "Per 15 Min Interval vs Time of Day", sep = " "))
  } else if (!avg_to_get_typical_day) {
    data <- person$fitbit_intraday[ , c("datetime", measure_var)]
    data <- data[stats::complete.cases(data), ]
    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = data$datetime, y = data[[measure_var]])) +
      ggplot2::geom_step(color = CARDINAL, alpha = 0.5) +
      ggplot2::labs(x = "Date-Time",
                    title = stringr::str_c(stringr::str_to_title(measure_var),
                      "Per 15 Min Interval vs Date-Time", sep = " "))
  } else {
    stop("must have a logical value for avg_to_get_typical_day")
  }
  p <- p +
      ggplot2::labs(y = stringr::str_to_title(measure_var))
  return(p)
}



#' Switch table to plot intraday variables.
#' 
#' @description Plot one continuous intraday variable across time.  Users can 
#'     specify if they want to look at an aggregate of a variable over the 
#'     course of a day (avg_to_get_typical_day = TRUE) or look at that variable 
#'     at every interval (i.e. every 15 minutes for the entire date range).
#' 
#' @param person An instance of the Person class
#' @param measure_var Character vector of length 1 denoting the variable of 
#'     interest.  Options include: "steps", "floors", "distance", 
#'     "caloriesBurned", "activeMin", "bpm" (heart rate), "weight".  By default, 
#'     all are plotted.
#' @param avg_to_get_typical_day Logical vector of length 1.  If TRUE, plot 
#'     gives an aggregate of the variable over the course of a typical day.  If 
#'     FALSE, plot gives the variable at every interval over the range specified
#'      when the Person object was instantiated.
#' @param ... Extra arguments used to specify unit for the distance and weight
#'     plots.
#' @return NULL, but plots print to screen
#' 
#' @export
#' @examples
#' data(EX)
#' plot_intraday(EX, "steps")
#' plot_intraday(EX, "distance", unit = "km")
#' plot_intraday(EX, "caloriesBurned", FALSE)
#' plot_intraday(EX, "steps", FALSE)
#' plot_intraday(EX, "bpm")
#' 
#' @seealso \code{\link{plot_intraday}}
plot_intraday <- function(person, measure_var = "all", 
                          avg_to_get_typical_day = TRUE, ...) {
  switch(measure_var,
         steps = plot_i_steps(person, avg_to_get_typical_day),
         floors = plot_i_floors(person, avg_to_get_typical_day),
         distance = plot_i_distance(person, avg_to_get_typical_day, ...),
         caloriesBurned = plot_i_cal(person, avg_to_get_typical_day),
         activeMin = plot_i_active_min(person, avg_to_get_typical_day),
         bpm = plot_i_hr(person, avg_to_get_typical_day),
         weight = plot_i_weight(person, avg_to_get_typical_day, ...),
         all = plot_intraday_all(person),
         stop('"measure_var" must be one of "all", "steps", "floors", "distance", "caloriesBurned", "activeMin", "bpm", "weight"')
  )
}

#' Plot all intraday variables.
#' 
#' @description Plots all seven intraday variables using default settings.
#' 
#' @param person An instance of the Person class.
#' @param avg_to_get_typical_day Logical vector of length 1.  If TRUE, plot 
#'     gives an aggregate of the variable over the course of a typical day.  If 
#'     FALSE, plot gives the variable at every interval over the range specified
#'      when the Person object was instantiated.
#' 
#' @return NULL, plots print to screen
#' @seealso \code{\link{plot_i}}
#' 
#' @export
#' @importFrom grDevices dev.flush dev.hold
#' @examples
#' data(EX)
#' plot_intraday_all(EX)
plot_intraday_all <- function(person, avg_to_get_typical_day = TRUE) {
    dev.hold()
    plot_intraday(person, "steps", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    dev.flush()
    
    dev.hold()
    plot_intraday(person, "floors", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    dev.flush()
    
    dev.hold()
    plot_intraday(person, "distance", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    dev.flush()
    
    dev.hold()
    plot_intraday(person, "caloriesBurned", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    dev.flush()
    
    dev.hold()
    plot_intraday(person, "activeMin", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    dev.flush()
    
    dev.hold()
    plot_intraday(person, "bpm", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    dev.flush()
    
    dev.hold()
    plot_intraday(person, "weight", avg_to_get_typical_day)
    readline(prompt = "Press [enter] to continue")
    invisible()
}

#' Plot distance over time.
#' 
#' @description Plot distance over time in units of either miles or kilometers.
#' 
#' @param person An instance of the Person class.
#' @param avg_to_get_typical_day Logical vector of length 1.  If TRUE, plot 
#'     gives an aggregate of the variable over the course of a typical day.  If 
#'     FALSE, plot gives the variable at every interval over the range specified
#'      when the Person object was instantiated.
#' @param unit The unit of distance, 'mi' by default, but can also specify 'km'
#' @return NULL, but plot prints to screen.
#' 
#' @export
#' @importFrom ggplot2 labs
#' 
#' @examples
#' data(EX)
#' plot_i_distance(EX, FALSE)
#' plot_i_distance(EX, unit = "km")
plot_i_distance <- function(person, avg_to_get_typical_day = TRUE, unit = "mi") {
  if (unit == 'mi') {
    p <- plot_i(person, "distance", avg_to_get_typical_day) +
      ggplot2::labs(y = "Distance (mi)")
  } else if (unit == "km") {
    p <- plot_i(person, "distanceKm", avg_to_get_typical_day) +
      ggplot2::labs(y = "Distance (km)")
    if (avg_to_get_typical_day) {
      p <- p + ggplot2::labs(title = 
                               "Average Distance Per 15 Min Interval vs Time of Day")
    } else if (!avg_to_get_typical_day) {
      p <- p + ggplot2::labs(title = 
                               "Distance Per 15 Min Interval vs Date-Time")
    }
  } else {
    stop("unit must be 'lb' or 'kg'")
  }
  print(p)
}

#' @describeIn plot_i Line graph for steps taken per 15 minute interval over 
#' date-time.
plot_i_steps <- function(person, avg_to_get_typical_day = TRUE) {
  p <- plot_i(person, "steps", avg_to_get_typical_day)
  print(p)
}

#' @describeIn plot_i Line graph for floors gone up per 15 minute interval over 
#' date-time.
plot_i_floors <- function(person, avg_to_get_typical_day = TRUE) {
  p <- plot_i(person, "floors", avg_to_get_typical_day)
  print(p)
}

#' @describeIn plot_i Line graph for calories burned per 15 minute interval over 
#'     date-time.
plot_i_cal <- function(person, avg_to_get_typical_day = TRUE) {
  p <- plot_i(person, "caloriesBurned", avg_to_get_typical_day)
  if (avg_to_get_typical_day) {
    p <- p + 
      ggplot2::labs(y = "Calories Burned",
                    title = "Average Calories Burned Per 15 Min Interval vs Time of Day")
  } else if (!(avg_to_get_typical_day)) {
    p <- p + 
      ggplot2::labs(y = "Calories Burned",
                    title = "Calories Burned Per 15 Min Interval vs Date-Time")
  }
  print(p)
}

#' @describeIn plot_i Line graph for active minutes per 15 minute interval over 
#'     date-time.
plot_i_active_min <- function(person, avg_to_get_typical_day = TRUE) {
  p <- plot_i(person, "activeMin", avg_to_get_typical_day)
  if (avg_to_get_typical_day) {
    p <- p + 
      ggplot2::labs(y = "Minutes Active (per 15 minutes)",
                    title = "Average Active Minutes Per 15 Min Interval vs Time of Day")
  } else if (!(avg_to_get_typical_day)) {
    p <- p + 
      ggplot2::labs(y = "Minutes Active (per 15 minutes)",
                    title = "Average Active Minutes Per 15 Min Interval vs Date-Time")
  }
  print(p)
}


#' Calculate Heart Rate Zones.
#' 
#' @description Heart Rate Zones are calculated on the basis of age.  The 
#'     estimated maximum heart rate is calculated as 220 - the age of the user.  
#'     The peak heart rate zone is 85% greater than maximum heart rate, the 
#'     cardio heart rate zone is between 70 and 84% of maximum, and the fat burn
#'      heart rate zone is between 50 and 69% of maximum.
#' 
#' @param person An instance of the Person class
#' @return Returns a list with 3 vectors of length 2: peak, cardio, and fat_burn
#' @seealso \url{https://help.fitbit.com/articles/en_US/Help_article/1565#zones}
#' 
#' @export
#' 
#' @examples
#' data(EX)
#' get_hr_zones(EX)
get_hr_zones <- function(person) {
  age <- person$user_info$age
  max_hr <- HR_MAX_BEFORE_AGE - age
  hr_zones <- list(peak = NULL, cardio = NULL, fat_burn = NULL)
  hr_zones$peak <- as.integer(max_hr * HR_PEAK_PROP)
  hr_zones$cardio <- as.integer(max_hr * HR_CARDIO_PROP)
  hr_zones$fat_burn <- as.integer(max_hr * HR_FAT_BURN_PROP)
  return(hr_zones)
}

#' @describeIn plot_i Line graph for heart rate per 5 minute interval across a
#'     typical day or over date-time.
#' @seealso \code{\link{get_hr_zones}}
plot_i_hr <- function(person, avg_to_get_typical_day = TRUE) {
  if (avg_to_get_typical_day) {
    p <- plot_i(person, "bpm", avg_to_get_typical_day) + 
      ggplot2::labs(y = "Heart Rate (bpm)",
                    title = "Average Heart Rate Per 5 Min Interval vs Time of Day")
  } else if (!avg_to_get_typical_day) {
    p <- plot_i_hr_datetime(person)
  } else {
    stop("'avg_to_get_typical_day' must be a logical value")
  }
  print(p)
}

#' @describeIn plot_i Line graph for heart rate per 5 minute interval across a
#'     typical day.
plot_i_hr_datetime <- function(person) {
  hr_zones <- get_hr_zones(person)

  p <- plot_i(person, "bpm", avg_to_get_typical_day = FALSE)  + 
    ggplot2::annotate("rect",
                      xmin = rep(as.POSIXct(person$start_date), 3),
                      xmax = rep(as.POSIXct(person$end_date) + 
                                   lubridate::days(1), 3),
                      ymin = c(hr_zones$fat_burn[1], hr_zones$fat_burn[2], 
                               hr_zones$cardio[2]),
                      ymax = c(hr_zones$fat_burn[2], hr_zones$cardio[2], 
                               hr_zones$peak[2]),
                      fill = c("orange", "green", "blue"),
                      alpha = 0.25) +
    ggplot2::annotate("text",
                      x = rep(as.POSIXct(person$start_date), 3),
                      y = c(hr_zones$fat_burn[1], hr_zones$fat_burn[2], hr_zones$cardio[2]),
                      label = c("Fat burn", "Cardio", "Peak"),
                      hjust = -0.1,
                      vjust = -0.1) +
    ggplot2::labs(x = "Date-Time", y = "Heart Rate (bpm)",
                 title = "Average Heart Rate Per 5 Min Interval vs Date-Time")
  return(p)
}

#' @describeIn plot_i Line graph for weight over time.
#' @param unit Unit of measurement for plot_i_weight().  Default is "lb", but 
#'     "kb" can also be specified
plot_i_weight <- function(person, avg_to_get_typical_day = TRUE, unit = "lb") {
  if (unit == "lb") {
    p <- plot_i(person, "weight", avg_to_get_typical_day) +
      ggplot2::labs(y = "Weight (lbs)")
  } else if (unit == "kg") {
    p <- plot_i(person, "weightKg", avg_to_get_typical_day) +
      ggplot2::labs(y = "Weight (kg)")
  } else {
    stop("unit must be 'lb' or 'kg'")
  }
  
  if (avg_to_get_typical_day) {
    p <- p +
      ggplot2::labs(title = "Weight vs Time of Day")
  } else if (!(avg_to_get_typical_day)) {
    p <- p +
      ggplot2::labs(title = "Weight vs Date-Time")
  } else {
    stop("'avg_to_get_typical_day' must be a logical value")
  }
  print(p)
}

