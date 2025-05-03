
# ===== INTERNAL FUNCTIONS =====================================================

# NOTE:  Generally useful internal functions that are not associated with either
# NOTE:  'sts' or 'mts' objects.

# ----- .determineTimezone -----------------------------------------------------

.determineTimezone <- function(
    any_ts = NULL,
    startdate = NULL,
    timezone = NULL,
    verbose = FALSE
) {

  MazamaCoreUtils::stopIfNull(any_ts)

  # Timezone determination precedence assumes that if you are passing in
  # POSIXct times then you know what you are doing.
  #   1) get timezone from startdate if it is POSIXct
  #   2) use passed in timezone
  #   3) get timezone from any_ts

  if ( lubridate::is.POSIXt(startdate) ) {

    timezone <- lubridate::tz(startdate)

    # NOTE:  This can happen when using lubridate::now()
    #
    # > lubridate::now() %>% attributes()
    # $class
    # [1] "POSIXct" "POSIXt"
    #
    # $tzone
    # [1] ""

    if ( timezone == "" ) {
      stop(paste0(
        "POSIXct value '", startdate, "' has no timezone.\n",
        "Be sure to use lubridate::now(tzone = 'UTC') or similar to guarantee a timezone is specified."
      ))
    }

  } else if ( !is.null(timezone) ) {

    # Do nothing; timezone is already defined

  } else {

    timezone <- .mostFrequentValue(any_ts, "timezone", verbose)

  }

  if ( !timezone %in% OlsonNames() )
    stop(sprintf("timezone '%s' is not a valid Olson timezone", timezone))

  return(timezone)

}


# ----- .mostFrequentValue -----------------------------------------------------

.mostFrequentValue <- function(
    any_ts = NULL,
    columnName,
    verbose = TRUE
) {

  MazamaCoreUtils::stopIfNull(any_ts)
  MazamaCoreUtils::stopIfNull(columnName)

  # Handle multiple values in 'any_ts'
  valueCount <- length(unique(any_ts$meta[[columnName]]))

  # Use table(...) to find the most common one
  if ( valueCount > 1 ) {
    valueTable <- sort(table(any_ts$meta[[columnName]]), decreasing = TRUE)
    value <- names(valueTable)[1]
    if ( verbose ) {
      warning(sprintf(
        "Found %d %ss. Only '%s' will be used.",
        valueCount,
        columnName,
        value
      ))
    }
  } else {
    value <- any_ts$meta[[columnName]][1]
  }

  return(value)

}


