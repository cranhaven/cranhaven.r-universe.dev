## ----setup, echo = FALSE-------------------------------------------------
NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "TRUE")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## ----load-package--------------------------------------------------------
library(cumulocityr)
library(knitr)

## ------------------------------------------------------------------------
devices <- get_devices()
print(devices[,c("type", "name", "id")])

## ------------------------------------------------------------------------
meas <- get_measurements(device_id = 53700,
                         num_rows = 100,
                         date_from = "2019-09-30T19:59:00Z")
kable(head(meas[c("time", "type", "c8y_Temperature.T.value")]))

time_parsed <- as.POSIXct(meas$time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "Z")

plot(time_parsed, meas$c8y_Temperature.T.value, type = "l",
     xlab = "Time", ylab = "Temperature (deg C)")


## ------------------------------------------------------------------------
events <- get_events(device_id = 53700,
                     num_rows = 6,
                     date_from = "2019-09-30T19:59:00Z")

kable(events[c("type", "time", "c8y_Position.lng", "c8y_Position.alt")])

## ------------------------------------------------------------------------
meas_03 <- get_measurements(device_id = 53700,
                            num_rows = 2,
                            date_from = "2019-09-30T19:59:00Z",
                            parse_json = FALSE)

print(meas_03)

