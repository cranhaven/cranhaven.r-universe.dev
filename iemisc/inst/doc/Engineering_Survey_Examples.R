## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  install.packages("iemisc", "pander")
#  # install the packages and their dependencies

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# load the required packages
install.load::load_package("iemisc", "pander")

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

Northing_begin <- 283715.8495
Easting_begin <- 1292428.3999

Northing_end <- 303340.6977
Easting_end <- 1295973.7743

pander(project_midpoint(Northing_begin, Easting_begin, Northing_end, Easting_end, units =
"survey_ft", location = "TN", output = "advanced"))

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

Northing2 <- c(232489.480, 234732.431)

Easting2 <- c(942754.124, 903795.239)

dt4A <- project_midpoint(Northing2[1], Easting2[1], Northing2[2], Easting2[2],
"meters", "TN", output = "advanced")
pander(dt4A)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

Northing3 <- c("630817.6396", "502170.6065", "562,312.2349", "574,370.7178")

Easting3 <- c("2559599.9201", "1433851.6509", "1,843,018.4099", "1,854,896.0041")

dt3A <- engr_survey(Northing3[1], Easting3[1], "survey_ft", "TN", output =
"basic", utm = 1)
pander(dt3A) # first set of Northing, Easting points


dt3B <- engr_survey(Northing3[2], Easting3[2], "survey_ft", "TN", output =
"basic", utm = 0)
pander(dt3B) # second set of Northing, Easting points


dt3C <- engr_survey(Northing3[3], Easting3[3], "survey_ft", "TN", output =
"basic", utm = 1)
pander(dt3C) # third set of Northing, Easting points


dt3D <- engr_survey(Northing3[4], Easting3[4], "survey_ft", "TN", output =
"basic", utm = 0)
pander(dt3D) # fourth set of Northing, Easting points

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

Northing4 <- c(232489.480, 234732.431)

Easting4 <- c(942754.124, 903795.239)

dt4A <- engr_survey(Northing4[1], Easting4[1], "meters", "TN", output = "table", utm = 0)
pander(dt4A)


dt4B <- engr_survey(Northing4[2], Easting4[2], "meters", "TN", output = "table", utm = 0)
pander(dt4B)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

Northing2 <- c(232489.480, 234732.431)

Easting2 <- c(942754.124, 903795.239)

dt4 <- engr_survey_batch(Northing2, Easting2, "meters", "TN", output = "table")
pander(dt4)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

station5 <- "516+64.10"
station6 <- "511+29.10"

engr_survey2(station5, station6, units1 = "foot", units2 = "kilometers")


station7 <- "303+91.00"
station8 <- "299+41.00"

engr_survey2(station7, station8, units1 = "meters", units2 = "foot")



station9 <- "43+50.00"
station10 <- "52+00.00"

engr_survey2(station9, station10, units1 = "foot", units2 = "mile")

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

engr_survey3(23, station_distance = 100, units = "survey_mile", output = "numeric")

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

engr_survey4(1394.32, "45+43.12", units = "kilometers")

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

lat <- 35.8466965

long <- -088.9206794

dt1A <- engr_survey_reverse(lat, long, units = "survey_ft", location = "TN", output =
"table", utm = 0)
pander(dt1A)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

lats <- "37'50'21.5988''N"
longs <- "84'16'12.0720'W"

dt2B <- engr_survey_reverse(lats, longs, "foot", "KY", output = "table", utm = 0)
pander(dt2B)

