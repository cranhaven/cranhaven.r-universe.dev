#' Weather Reading
#'
#' This functions calls upon the weather reading API from data.gov.sg
#' and returns a list containing the air temperature, rainfall, relative humidity,
#' wind direction and speed across Singapore. Data is updated every 5 minutes for the
#' rainfall API, and every half minute for the other 4 API from NEA.
#'
#' This API takes slighlty longer than the other APIs in the package as 5 APIs are
#' wrapped within this function.
#'
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#' @param simplify Defaults to FALSE. Otherwise, simplify = TRUE would return a
#' data frame where all 5 metrics are joined according to weather stations, but
#' return several NAs, as most weather stations collect rainfall data only.
#'
#' @keywords weather
#'
#' @return A dataframe containing various weather readings from weather stations
#'
#' @export
#' @examples
#' weather_reading()
#' weather_reading(date = "2019-11-08T17:30:00")
#' weather_reading(date = "2018-01-04T09:16:17", simplify = TRUE)

weather_reading = function(date_time = "", simplify = FALSE) {

  # 1. Air Temperature
  URL.air.temp = parse_api_date(api = "environment/air-temperature",
                                input_date = date_time,
                                summary = FALSE)
  output.air.temp = httr::GET(URL.air.temp)
  content.output.air.temp = parse_api_output(output.air.temp)

  air_temp = dplyr::bind_rows(content.output.air.temp$items[[1]]$readings)


  # 2. Rainfall
  URL.rainfall = parse_api_date(api = "environment/rainfall",
                                input_date = date_time,
                                summary = FALSE)
  output.rainfall = httr::GET(URL.rainfall)
  content.output.rainfall = parse_api_output(output.rainfall)

  rainfall = dplyr::bind_rows(content.output.rainfall$items[[1]]$readings)


  # 3. Relative Humidity
  URL.rel.hum = parse_api_date(api = "environment/relative-humidity",
                               input_date = date_time,
                               summary = FALSE)
  output.rel.hum = httr::GET(URL.rel.hum)
  content.output.rel.hum = parse_api_output(output.rel.hum)

  relative_humidity = dplyr::bind_rows(content.output.rel.hum$items[[1]]$readings)


  # 4. Wind Direction
  URL.wind.dir = parse_api_date(api = "environment/wind-direction",
                                         input_date = date_time,
                                         summary = FALSE)
  output.wind.dir = httr::GET(URL.wind.dir)
  content.output.wind.dir = parse_api_output(output.wind.dir)

  wind_direction = dplyr::bind_rows(content.output.wind.dir$items[[1]]$readings)


  # 5. Wind Speed
  URL.wind.spd = parse_api_date(api = "environment/wind-speed",
                                input_date = date_time,
                                summary = FALSE)
  output.wind.spd = httr::GET(URL.wind.spd)
  content.output.wind.spd = parse_api_output(output.wind.spd)

  wind_speed = dplyr::bind_rows(content.output.wind.spd$items[[1]]$readings)


  # Bunching output

  weather_reading = list(air_temp = air_temp,
                         rainfall = rainfall,
                         relative_humidity = relative_humidity,
                         wind_direction = wind_direction,
                         wind_speed = wind_speed)

  if (simplify == FALSE) {

    return(weather_reading)

  } else if (simplify == TRUE) {

    message("Note that the NAs in the data set are due to the particular weather station not collecting a particular weather information.")

    return(purrr::reduce(weather_reading, dplyr::full_join, by = "station_id"))

  } else {

    stop("Did you enter the correct parameters for simplify? By default, simplify = FALSE returns a list, while simplify = TRUE returns a data frame.")

  }

}

