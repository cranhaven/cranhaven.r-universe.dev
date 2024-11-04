#' Weather Forecast
#'
#' This functions calls upon the weather forecast API from data.gov.sg
#' and returns a data frame containing different metrics of the forecast.
#' 2-hour, 24-hour and 4-day forecasts are availible. This data provided by the
#' API is updated half-hourly.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#' @param forecast  Defaults to "2-hour". Also availible for "24-hour" and "4-day"
#'
#' @keywords weather
#'
#' @return A dataframe the forecast for a given date and time, and forecast period.
#'
#' @export
#' @examples
#' weather_forecast()
#' weather_forecast(date = "2019-11-08T17:30:00", forecast = "24-hour")
#' weather_forecast(date = "2018-01-04T09:16:17", forecast = "4-day")

weather_forecast = function(date_time = "", forecast = "2-hour") {

  # Creating and pulling URL
  if (!(forecast %in% c("2-hour", "24-hour", "4-day"))) {
    stop("Forecasts only availible for 2-hour, 24-hour and 4 days.")
  }

  URL = parse_api_date(api = paste0("environment/", forecast, "-weather-forecast"),
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  # Extracting Data Frame

  if (forecast == "2-hour") {

    message("Closest timestamp: ", content.output$items[[1]]$timestamp)
    message("Forecast valid to: ", content.output$items[[1]]$valid_period$end)

    weather_forecast = dplyr::bind_rows(content.output$items[[1]]$forecasts)
    weather_forecast = as.data.frame(weather_forecast, stringsAsFactors = FALSE)

    return(weather_forecast)

  } else if (forecast == "24-hour") {

    message("Closest timestamp: ", content.output$items[[1]]$valid_period$start)
    message("Forecast valid to: ", content.output$items[[1]]$valid_period$end)

    weather_forecast = data.frame(general_forecast = content.output$items[[1]]$general$forecast,
                                  relative_humidity_low = content.output$items[[1]]$general$relative_humidity$low,
                                  relative_humidity_high = content.output$items[[1]]$general$relative_humidity$high,
                                  temperature_low = content.output$items[[1]]$general$temperature$low,
                                  temperature_high = content.output$items[[1]]$general$temperature$high,
                                  wind_direction = content.output$items[[1]]$general$wind$direction,
                                  wind_speed_low = content.output$items[[1]]$general$wind$speed$low,
                                  wind_speed_high = content.output$items[[1]]$general$wind$speed$high,
                                  stringsAsFactors = FALSE)

    return(weather_forecast)

  } else {

    message("Closest timestamp: ", content.output$items[[1]]$timestamp)

    weather_forecast = lapply(1:4, function(x) {
      data.frame(date = content.output$items[[1]]$forecasts[[x]]$date,
                 general_forecast = content.output$items[[1]]$forecasts[[x]]$forecast,
                 relative_humidity_low = content.output$items[[1]]$forecasts[[x]]$relative_humidity$low,
                 relative_humidity_high = content.output$items[[1]]$forecasts[[x]]$relative_humidity$high,
                 temperature_low = content.output$items[[1]]$forecasts[[x]]$temperature$low,
                 temperature_high = content.output$items[[1]]$forecasts[[x]]$temperature$high,
                 wind_direction = content.output$items[[1]]$forecasts[[x]]$wind$direction,
                 wind_speed_low = content.output$items[[1]]$forecasts[[x]]$wind$speed$low,
                 wind_speed_high = content.output$items[[1]]$forecasts[[x]]$wind$speed$high,
                 stringsAsFactors = FALSE)
    })

    weather_forecast = dplyr::bind_rows(weather_forecast)

    return(weather_forecast)

  }
}

