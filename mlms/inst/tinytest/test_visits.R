library("tinytest")
library("checkmate")
using("checkmate")

# test for data frame class
expect_data_frame(visits, min.rows = 1, min.cols = 1)

# test site numbers
expect_subset(visits$site_nm, choices = wells$site_nm)
is <- visits$site_nm %in% wells$site_nm
d <- visits[!is, c("site_nm", "stime_dt", "site_nm")]
if (nrow(d)) cat("\n", "Invalid site number:", utils::capture.output(d), "", sep = "\n")

# test barometric pessure sensors
expect_subset(visits$baro_id, choices = sensors$sensor_id)
is <- !(visits$baro_id %in% sensors$sensor_id)
d <- visits[is, c("site_nm", "stime_dt", "baro_id")]
if (nrow(d)) cat("\n", "Invalid barometric pressure sensor id:", utils::capture.output(d), "", sep = "\n")

# test fluid pessure sensors
expect_subset(visits$sensor_id, choices = sensors$sensor_id)
is <- !(visits$sensor_id %in% sensors$sensor_id)
d <- visits[is, c("site_nm", "stime_dt", "sensor_id")]
if (nrow(d)) cat("\n", "Invalid fluid sensor id:", utils::capture.output(d), "", sep = "\n")

# test start date-time values
lower <- as.POSIXct("2000-01-01 00:00:00", tz = attr(visits$stime_dt, "tzone"))
expect_posixct(visits$stime_dt, lower = lower, any.missing = FALSE)
is <- is.na(visits$stime_dt) | visits$stime_dt < lower
d <- visits[is, c("site_nm", "operators", "stime_dt")]
if (nrow(d)) cat("\n", "Invalid start date-time value:", utils::capture.output(d), "", sep = "\n")
