#' Handle Dates based on Dataset Ranges
#'
#' @description This function handles dates based on the respective dataset. According to the documentation:
#' - Data from 2024 onwards corresponds to Unverified data transmitted continuously (Up-To-Date/UTD/E2a).
#' - Data from 2013 to the begin of 2023 corresponds to Verified data (E1a) reported by countries by 30 September each year for the previous year.
#' - Data delivered before 2012 corresponds to Historical Airbase data.
#' The range for E1 is extended until 31/12/2023 because the observations are already validated, and no data for 2023 is retrieved when considering E2.
#' @param from StartDate (in "YYYY-MM-DD" format).
#' @param to EndDate (in "YYYY-MM-DD" format).
#'
#' @return A list of datasets with associated date ranges and descriptions.

handle_dates <- function(from,to ) {


  # Ensure both dates are in date format
  dateTimeStart <- lubridate::as_date(from)
  dateTimeEnd <- lubridate::as_date(to)

  # Define date ranges for each dataset
  E1_range <- lubridate::interval(lubridate::ymd("2024-01-01"), lubridate::today())
  E2_range <- lubridate::interval(lubridate::ymd("2013-01-01"), lubridate::ymd("2023-12-31"))
  E3_range <- lubridate::interval(lubridate::ymd("1900-01-01"), lubridate::ymd("2012-12-31"))

  # Initialize lists to store results
  datasets <- list()

  # Check the intervals and split as needed
  if (dateTimeStart %within% E1_range && dateTimeEnd %within% E1_range) {
    datasets[[1]] <- list(dataset = 1, dateStart = dateTimeStart, dateEnd = dateTimeEnd)
  }
  else if (dateTimeStart %within% E2_range && dateTimeEnd %within% E2_range) {
    datasets[[1]] <- list(dataset = 2, dateStart = dateTimeStart, dateEnd = dateTimeEnd)
  }
  else if (dateTimeStart %within% E3_range && dateTimeEnd %within% E3_range) {
    datasets[[1]] <- list(dataset = 3, dateStart = dateTimeStart, dateEnd = dateTimeEnd)
  }
  else if (dateTimeEnd %within% E2_range) {
    datasets <- list(
      list(dataset = 3, dateStart = dateTimeStart, dateEnd = lubridate::ymd("2012-12-31")),
      list(dataset = 2, dateStart = lubridate::ymd("2013-01-01"), dateEnd = dateTimeEnd)
    )
  }
  else if (dateTimeStart %within% E3_range && dateTimeEnd %within% E1_range) {
    datasets <- list(
      list(dataset = 3, dateStart = dateTimeStart, dateEnd = lubridate::ymd("2012-12-31")),
      list(dataset = 2, dateStart = lubridate::ymd("2013-01-01"), dateEnd = lubridate::ymd("2023-12-31")),
      list(dataset = 1, dateStart = lubridate::ymd("2024-01-01"), dateEnd = dateTimeEnd)
    )
  } else if (dateTimeStart %within% E3_range && dateTimeEnd %within% E2_range) {
    datasets <- list(
      list(dataset = 3, dateStart = dateTimeStart, dateEnd = lubridate::ymd("2012-12-31")),
      list(dataset = 2, dateStart = lubridate::ymd("2013-01-01"), dateEnd = dateTimeEnd)
    )
  } else if (dateTimeStart %within% E2_range && dateTimeEnd %within% E1_range) {
    datasets <- list(
      list(dataset = 2, dateStart = dateTimeStart, dateEnd = lubridate::ymd("2023-12-31")),
      list(dataset = 1, dateStart = lubridate::ymd("2024-01-01"), dateEnd = dateTimeEnd)
    )
  } else {
    stop("The specified dates do not fall within valid ranges.")
  }

  # Format each date entry in datasets
  datasets <- lapply(datasets, function(d) {
    d$dateStart <- format(lubridate::ymd_hms(paste0(d$dateStart, " 00:00:00"), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ") #"23:59:59"
    d$dateEnd <- format(lubridate::ymd_hms(paste0(d$dateEnd, "00:00:00"), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    return(d)
  })

  # Return the result as a list
  return(datasets)
}
