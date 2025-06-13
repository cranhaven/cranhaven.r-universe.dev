#' @include viz_daily.R
#' @include experiments.R
NULL

#' Plot a series of six sleep graphs.
#' 
#' @description Prints six plots: two are related to quantity of sleep, and 
#' four are related to quality of sleep
#' 1.  Sleep by day of week (bar graph)
#' 2.  Start and end of sleep period for each day in the range
#' 3.  Duration of sleep and time asleep over time
#' 4.  Proportion of time spent restless out of total sleep duration over time
#' 5.  Time spent restless over time (in minutes)
#' 6.  Sleep quality over time (subjective score, out of 100)
#' 
#' @param person An instance of the Person class
#' @return NULL, but plot prints to screen
#' @export
#' @importFrom grDevices dev.flush dev.hold
#' @examples
#' data(EX)
#' plot_sleep_all(person = EX)
#'
plot_sleep_all <- function(person) {
  dev.hold()
  plot_sleep_weekday(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_start_end(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_over_time(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_restless_prop(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_restless_min(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_quality(person)
  readline(prompt = "Press [enter] to continue")
  invisible()
}

#' Plot sleep.
#' 
#' @description Prints one of six plots: two are related to quantity of sleep, 
#' and four are related to quality of sleep
#' 1.  Sleep by day of week (bar graph)
#' 2.  Start and end of sleep period for each day in the range
#' 3.  Duration of sleep and time asleep over time
#' 4.  Proportion of time spent restless out of total sleep duration over time
#' 5.  Time spent restless over time (in minutes)
#' 6.  Sleep quality over time (subjective score, out of 100)
#' 
#' @param person An instance of the Person class
#' @param plot_type The type of plot.  Options include: "by_weekday", 
#'     "by_start_end_time", "by_datetime", "by_restless_prop", 
#'     "by_restless_min", "by_quality".  Default is to plot all six.
#' @param ... Extra arguments used to specify the `color_var` for the 
#'     `by_start_end_time` plot
#' @return NULL, but plots print to screen
#' @export
#' @examples
#' data(EX)
#' plot_sleep(person = EX)
#'
plot_sleep <- function(person, plot_type = "all", ...) {
  switch(plot_type,
    by_weekday = plot_sleep_weekday(person),
    by_start_end_time = plot_sleep_start_end(person, ...),
    by_datetime = plot_sleep_over_time(person),
    by_restless_prop = plot_sleep_restless_prop(person),
    by_restless_min = plot_sleep_restless_min(person),
    by_quality = plot_sleep_quality(person),
    all = plot_sleep_all(person),
    stop('"plot_type" must be one of "all", "by_weekday", "by_start_end_time", "by_datetime", "by_restless_prop", "by_restless_min", or "by_quality"')
  )
}

# Quantity of Sleep

#' A function to preprocess sleep data for the Person object.
#' 
#' @description Preprocesses data to be used by the plot_sleep_weekday() 
#' function.  Specifically, it calculates the sleep duration and time asleep for 
#' each day of the week (in hours).
#' 
#' @param person An instance of the Person class
#' @return A tidy data frame with the columns weekday, measure, and hours
#' @importFrom dplyr mutate group_by summarize
#' @importFrom tidyr gather
#' @importFrom lubridate wday
#' @export
#' @examples
#' data(EX)
#' agg_sleep_weekday(person = EX)
#'
agg_sleep_weekday <- function(person) {
  data <- create_dataset(person = person,
                         all_variables = 
                           list("fitbit_daily" = c("sleepDurationHrs", 
                                                   "minAsleepHrs"),
                                "util" = c("day_of_week", "day_type")), 
                         time_var = c("date"))
  data <- dplyr::group_by(data, day_of_week)
  data <- dplyr::summarize(data, 
                           sleepDuration = mean(sleepDurationHrs, na.rm = TRUE),
                           minAsleep = mean(minAsleepHrs, na.rm = TRUE))
  data <- tidyr::gather(data, key = "measure", value = "hours", -day_of_week)
  return(data)
}

#' A function to plot sleep by day of week.
#' 
#' @description Returns a bar graph plotting sleep by day of week (Sunday, 
#'     Monday, ...).
#' 
#' @param person An instance of the Person class
#' @return NULL, but plots print to screen
#' @importFrom ggplot2 ggplot aes geom_col labs guides guide_legend scale_fill_discrete
#' @export
#' @examples
#' data(EX)
#' plot_sleep_weekday(person = EX)
#'
# Plot 1: by day of week
plot_sleep_weekday <- function(person) {
  data <- agg_sleep_weekday(person)
  p <- ggplot2::ggplot(data = data,
                  mapping = ggplot2::aes(x = day_of_week, 
                                         y = hours,
                                         fill = measure)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = "Day of the Week", y = "Hours",
                  title = "Hours of Sleep by Day of the Week") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Sleep Type", 
                                                 reverse = TRUE)) +
    ggplot2::scale_fill_discrete(labels = c("Time Asleep", "Sleep Duration"))
  print(p)
}

#' A function to plot sleep each night by start time and end time.
#' 
#' @description Returns a plot with start time of sleep and end time of sleep
#'     each night, colored by weekday vs. weekend.
#' 
#' @param person An instance of the Person class
#' @param color_var "day_type" by default for weekend/weekday, or "day_of_week"
#' for day of week.  Determines color of the lines.
#' @return NULL, but plots print to screen
#' @importFrom ggplot2 ggplot aes geom_col labs guides guide_legend 
#'     scale_fill_discrete
#' @export
#' @examples
#' data(EX)
#' plot_sleep_start_end(person = EX)
#' plot_sleep_start_end(person = EX, "day_of_week")
#'
# Plot 2: start and end
plot_sleep_start_end <- function(person, color_var = "day_type") {
  
  if (!(color_var %in% c("day_type", "day_of_week"))) {
    stop("'color_var' must be 'day_type' for weekend/weekday or 'day_of_week' for day of the week")
  }
  
  # Pull relevant data
  data <- create_dataset(person = person,
                         all_variables = 
                           list("fitbit_daily" = c("startTime", 
                                                   "startDateTime",
                                                   "endTime",
                                                   "endDateTime"),
                                "util" = c("day_of_week", "day_type")), 
                         time_var = c("date"))
  
  # If went to sleep before midnight adjust the start time
  data$startTime <- 
    ifelse(as.Date(as.POSIXct(data$startTime, format = "%H:%M")) !=
             as.Date(as.POSIXct(data$endTime, format = "%H:%M")),
           as.POSIXct(data$startTime, format = "%H:%M") - lubridate::days(1),
           as.POSIXct(data$startTime, format = "%H:%M"))
  data$startTime <- as.POSIXct(data$startTime, origin = "1970-01-01", 
                               tz = Sys.timezone())
  data$endTime <- as.POSIXct(data$endTime, format = "%H:%M")

  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_segment(mapping =
                            ggplot2::aes_string(x = data$date,
                                                xend = data$date,
                                                y = data$startTime,
                                                yend = data$endTime,
                                                color = color_var)) +
    ggplot2::labs(x = "Date", y = "Hours Asleep",
                  title = "Sleep Start and End Times")  +
    ggplot2::coord_cartesian(xlim = c(max(data$date), min(data$date))) +
    ggplot2::scale_y_datetime(date_labels = "%H:%M %p",
                              date_breaks = "3 hours",
    )  +
    ggplot2::guides(color = ggplot2::guide_legend(NULL)) +
    ggplot2::scale_color_discrete(labels = stringr::str_to_title) +
    ggplot2::coord_flip()
  print(p)
}

# Quality of Sleep

# Plot 3
#' A function to plot sleep over time.  
#' 
#' @description Returns a line plot plotting sleep over time.  Includes sleep
#'     duration and time asleep (in hours).
#' 
#' @param person An instance of the Person class
#' @return NULL, but plots print to screen
#' @export
#' @importFrom ggplot2 ggplot geom_line aes labs guides guide_legend 
#'     scale_color_discrete
#' @examples
#' data(EX)
#' plot_sleep_over_time(person = EX)
#'
plot_sleep_over_time <- function(person) {
  p <- plot_d(person, measures = c("sleepDurationHrs", "minAsleepHrs")) +
    ggplot2::labs(y = "Sleep Duration (hours)",
                  title = "Sleep Over Time") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Sleep Type",
                                                  reverse = TRUE)) +
    ggplot2::scale_color_discrete(labels = c("Time Asleep", "Sleep Duration"))
  print(p)
}


# Plot 4: Percent of Restless Sleep
#' A function to plot the proportion of restless sleep over time.  
#' 
#' @description Returns a line plot plotting the proportion of restless sleep 
#'     over time.  The proportion is calculated as the difference between sleep
#'     duration and time spent asleep over sleep duration.
#' 
#' @param person An instance of the Person class
#' @return NULL, but plots print to screen
#' @export
#' @importFrom ggplot2 ggplot geom_line aes labs
#' @examples
#' data(EX)
#' plot_sleep_restless_prop(person = EX)
#'
plot_sleep_restless_prop <- function(person) {
  p <- plot_d(person, "restlessProp")
  p <- p + 
      ggplot2::labs(x = "Date", y = "Percent of Restless Sleep",
                    title = "Quality of Sleep: Restlessness (%)")
  print(p)
}


# Plot 5: Length of Restless Sleep
#' A function to plot the minutes of restless sleep over time.  
#' 
#' @description Returns a line plot plotting the length of restless sleep 
#'     over time (in minutes).
#' 
#' @param person An instance of the Person class
#' @return NULL, but plots print to screen
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' data(EX)
#' plot_sleep_restless_min(person = EX)
#'
plot_sleep_restless_min <- function(person) {
  p <- plot_d(person, "restlessDuration")
  p <- p + ggplot2::labs(y = "Length of Restless Sleep (minutes)",
                    title = "Quality of Sleep: Restlessness (mins)")
  print(p)
}


# Plot 6: Subjective Quality of Sleep
#' A function to plot sleep quality over time.  
#' 
#' @description Returns a line plot plotting sleep quality over time.  Sleep
#' quality is a subjective score given by Fitbit
#' 
#' @param person An instance of the Person class
#' @return NULL, but plots print to screen
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' data(EX)
#' plot_sleep_quality(person = EX)
#'
plot_sleep_quality <- function(person) {
  p <- plot_d(person, "sleepQualityScoreA")
  p <- p + ggplot2::labs(y = "Sleep Quality Score", 
                         title = "Quality of Sleep: Quality Score")
  print(p)
}