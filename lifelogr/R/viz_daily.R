#' @include global_var.R
#' @include experiments.R
NULL

#' Plot a series of six graphs.
#' 
#' @description Prints six plots, each showing daily totals over time: 
#' 1.  Steps
#' 2.  Floors
#' 3.  Distance: in the default unit, miles
#' 4.  Calories
#' 5.  Minutes 'very active'
#' 6.  Resting heart rate
#' 
#' @param person An instance of the Person class
#' @return NULL, but plots printed to screen
#' @importFrom grDevices dev.flush dev.hold
#' @export
#' @examples
#' data(EX)
#' plot_daily_all(EX)
#'
plot_daily_all <- function(person) {
  
  dev.hold()
  plot_steps(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_floors(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_distance(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_cal(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_mins_very(person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_rest_hr(person)
  readline(prompt = "Press [enter] to continue")
  invisible()
}

#' Plot daily health totals.
#' 
#' @description Prints one of six plots, each showing daily totals over time.
#' 
#' @param person An instance of the Person class
#' @param measure_var Default is to print all six plots.  Options include:
#'     "steps", "floors", "distance", "calories", "mins_very", "rest_hr", "all".
#' @param ... Extra arguments used to specify unit for the distance plot.
#' @return NULL, but plots printed to screen
#' @export
#' @examples
#' data(EX)
#' plot_daily(EX, "steps")
#' plot_daily(EX, "distance", "km")
#'
plot_daily <- function(person, measure_var = "all", ...) {
  switch(measure_var,
         steps = plot_steps(person),
         floors = plot_floors(person),
         distance = plot_distance(person, ...),
         calories = plot_cal(person),
         mins_very = plot_mins_very(person),
         rest_hr = plot_rest_hr(person),
         all = plot_daily_all(person),
         stop('"measure_var" must be one of "all", "steps", "floors", "distance", "calories", "mins_very", "rest_hr"')
  )
}

#' 
#' Tidy daily data.
#' 
#' @description Tidy daily data with multiple measures.
#' 
#' @param data Data frame or tibble with a column named 'date' and other columns
#'    of interest.
#' @return Tidy tibble with the columns date, measures, and value.
#' 
#' @export
#' @importFrom tidyr gather
#' @importFrom tibble tibble
#' 
#' @examples
#' a <- tibble::tibble(date = 
#'          lubridate::ymd("1970-01-01", "1970-01-02", "1970-01-03"), 
#'          sleepDurationHrs = c(7.5, 8.0, 7.9), 
#'          minAsleepHrs = c(7.4, 7.0, 7.7))
#' tidy_multi_meas_data(a)
tidy_multi_meas_data <- function(data) {
  if(is.null(data$date)) {
    stop("'date' must be a named column of 'data'")
  }
  return(tidyr::gather(data, key = "measures", value = "value", -date))
}

#' Line graph for continuous variable(s).
#' 
#' @description A "quick-and-dirty" approach to plotting a generic line graph 
#'     with default axis labels.  Can plot one or more variables.
#' 
#' @param person An instance of the Person class
#' @param measures A character vector of length one or more indicating the 
#'     variable(s) of interest.  Options include: "steps", "floors", "distance",
#'      "calories", "mins_very", "rest_hr".
#' @return NULL, but plot printed to screen
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom stringr str_to_title str_c
#' @examples
#' data(EX)
#' plot_d(EX, "steps")
#' plot_d(EX, c("steps", "distance"))
plot_d <- function(person, measures) {
  data <- create_dataset(person = person,
                         all_variables = 
                           list("fitbit_daily" = measures), 
                         time_var = c("date"))
  if (length(measures) == 1) {
    p <- 
      ggplot2::ggplot(data = data,
                      mapping = ggplot2::aes(x = date, y = data[[measures]]))  +
      ggplot2::geom_point(color = CARDINAL) +
      ggplot2::geom_line(color = CARDINAL)  +
      ggplot2::labs(y = stringr::str_to_title(measures),
                    title = stringr::str_c("Total", 
                                           stringr::str_to_title(measures),
                                           "Per Day",
                                           sep = " "))
  } else if (length(measures) > 1) {
    data <- tidy_multi_meas_data(data)
    p <- 
      ggplot2::ggplot(data = data,
                      mapping = ggplot2::aes(x = date, y = data$value, color = 
                                               measures)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(title = "Totals Per Day")
  } else {
    stop("Enter a measure you wish to plot")
  }
  
  p <- p +
    ggplot2::labs(x = "Date")
  
  return(p)
}

#' Plot steps per day over time.  
#' 
#' @description Prints a line plot plotting steps per day over time.  The
#'     reference line refers to the user's target number of steps.
#' 
#' @param person An instance of the Person class
#' @return NULL, but plot printed to screen
#' 
#' @export
#' @importFrom modelr geom_ref_line
#' @importFrom ggplot2 labs
#' @examples
#' data(EX)
#' plot_steps(EX)
#'
plot_steps <- function(person) {
  p <- plot_d(person, "steps") + 
    modelr::geom_ref_line(h = person$target_steps, colour = "orange", 
                          size = 0.7) +
    ggplot2::labs(title = "Number of Steps Per Day")
  print(p)
}

#' Plot number of floors per day over time.  
#' 
#' @description Prints a line plot plotting number of floors per day over time.
#' 
#' @param person An instance of the Person class
#' @return NULL, but plot printed to screen
#' 
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' data(EX)
#' plot_floors(EX)
plot_floors <- function(person) {
  p <- plot_d(person, "floors") +
    ggplot2::labs(title = "Number of Floors Per Day")
  print(p)
}


#' Plot distance per day over time.  
#' 
#' @description Prints a line plot plotting distance in miles or kilometers per 
#'     day over time.
#' 
#' @param person An instance of the Person class
#' @param unit a unit of distance, 'mi' or 'km'.  The default value is 'mi'
#' @return NULL, but plot printed to screen
#' 
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' data(EX)
#' plot_distance(EX)
#' plot_distance(EX, "mi")
#' plot_distance(EX, "km")
plot_distance <- function(person, unit = "mi") {
  
  if (unit == "mi") {
    p <- plot_d(person, "distance")
  } else if (unit == "km") {
    p <- plot_d(person, "distanceKm")
  } else {
    stop("'unit' must be 'mi' or 'km'")
  }
  
  p <- p + ggplot2::labs(y = paste0("Distance (", unit, ")"), title = 
                           "Distance")
  print(p)
}


#' Plot calories over time.  
#' 
#' @description Prints a line plot plotting calories burned over time.  If
#'     calories consumed are in the dataset, it also plots calories consumed.
#' 
#' @param person An instance of the Person class
#' @return NULL, but plot printed to screen
#' 
#' @export
#' @importFrom ggplot2 labs ggplot geom_line aes guides guide_legend 
#'     scale_color_discrete
#' @examples
#' data(EX)
#' plot_cal(EX)
plot_cal <- function(person) {
  data <- create_dataset(person = person,
                         all_variables = 
                           list("fitbit_daily" = c("caloriesBurned", "caloriesIntake")), 
                         time_var = c("date"))
  if (sum(data$caloriesIntake, na.rm = TRUE) == 0) {
    p <- plot_d(person, "caloriesBurned")
    p <- p + ggplot2::labs(y = "Calories", title = "Calories Burned")
  } else if (sum(data$caloriesIntake, na.rm = TRUE) != 0) {
    data <- tidy_multi_meas_data(data)

    p <- ggplot2::ggplot(data = data, mapping = 
                           ggplot2::aes(x = date, y = data$value)) +
      ggplot2::geom_line(mapping = ggplot2::aes(color = data$measures)) +
      ggplot2::labs(x = "Date", y = "Calories",
                    title = "Calories Burned and Consumed") +
      ggplot2::guides(color = ggplot2::guide_legend("Calories")) +
      ggplot2::scale_color_discrete(labels = c("Burned", "Consumed"))
  } 
  print(p)
}


#' Plot minutes 'very active' over time.  
#' 
#' @description Prints a line plot plotting minutes 'very active' per day over 
#'     time.  'Very active' is a subjective term defined by fitbit.
#' 
#' @param person An instance of the Person class
#' @return NULL, but plot printed to screen
#' 
#' @export
#' @importFrom ggplot2 labs
#' 
#' @examples
#' data(EX)
#' plot_mins_very(EX)
plot_mins_very <- function(person) {
  p <- plot_d(person, "minutesVery")
  p <- p + ggplot2::labs(y = "Time 'Very Active' (mins)", 
                    title = "Time Spent 'Very Active' by Day")
  print(p)
}


#' Plot resting heart rate over time.  
#' 
#' @description Prints a line plot plotting heart rate (in beats per minute)
#'     over time.  According to the National Institute of Health, the average
#'     resting heart rate for persons 10 and older (including seniors) is 60 - 
#'     100.  However, well-trained athletes can have resting heart rates between
#'      40 and 60.
#' 
#' @param person An instance of the Person class
#' @return NULL, but plot printed to screen
#' 
#' @export
#' @importFrom ggplot2 labs
#' 
#' @examples
#' data(EX)
#' plot_rest_hr(EX)
#' @seealso \url{http://www.heart.org/HEARTORG/HealthyLiving/PhysicalActivity/FitnessBasics/Target-Heart-Rates_UCM_434341_Article.jsp#.WM3bCxiZMdU}
plot_rest_hr <- function(person) {
  p <- plot_d(person, "restingHeartRate") +
    ggplot2::labs(y = "Resting Heart Rate (bpm)", title = "Resting Heart Rate")
  print(p)
}