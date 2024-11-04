#' Traffic Images
#'
#' This functions calls upon the Traffic Images API from data.gov.sg
#' and returns links to images of live traffic conditions along expressways and
#' Woodlands & Tuas Checkpoints, including details of the camera location.
#' This data provided by the API is updated every minute.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords Traffic
#'
#' @return A dataframe containing links to the traffic images of current
#' traffic conditions
#'
#' @export
#' @examples
#' traffic_images()
#' traffic_images(date = "2022-07-01T15:32:45")
#' traffic_images(date = "2021-02-11T14:11:07")

traffic_images = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "transport/traffic-images",
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  if (length(content.output$items) == 0) {
    stop("No data returned from API.")
  }

  # Extracting Data Frame
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)

  traffic_images = data.frame()

  traffic_images = lapply(1:length(content.output$items[[1]]$cameras),
                          function(x) {
    temp = c(
      date_time = content.output$items[[1]]$cameras[[x]]$timestamp,
      url = content.output$items[[1]]$cameras[[x]]$image,
      camera_lat = content.output$items[[1]]$cameras[[x]]$location$latitude,
      camera_lon = content.output$items[[1]]$cameras[[x]]$location$longitude,
      camera_id = content.output$items[[1]]$cameras[[x]]$camera_id
      ) %>%
      as.data.frame() %>%
      data.table::transpose()

    rbind(traffic_images,
          temp,
          stringsAsFactors = FALSE)
  })

  traffic_images = dplyr::bind_rows(traffic_images)
  colnames(traffic_images) = c("date_time",
                               "url",
                               "camera_lat",
                               "camera_lon",
                               "camera_id")

  return(traffic_images)

}



