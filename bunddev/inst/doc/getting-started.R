## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bunddev)

## ----registry-----------------------------------------------------------------
bunddev_list()

## ----registry-filter----------------------------------------------------------
bunddev_list(tag = "jobs")

## ----registry-info------------------------------------------------------------
bunddev_info("abfallnavi")

## ----registry-endpoints-------------------------------------------------------
endpoints <- bunddev_endpoints("abfallnavi")
head(endpoints, 5)

## ----bewerberboerse, eval = FALSE---------------------------------------------
# Sys.setenv(BEWERBERBOERSE_API_KEY = "jobboerse-bewerbersuche-ui")
# bunddev_auth_set("bewerberboerse", type = "api_key", env_var = "BEWERBERBOERSE_API_KEY")
# 
# bewerber <- bewerberboerse_search(
#   params = list(was = "data", size = 10),
#   flatten = TRUE
# )
# 
# details <- bewerberboerse_details(bewerber$refnr[[1]], flatten = TRUE)

## ----autobahn, eval = FALSE---------------------------------------------------
# roads <- autobahn_roads()
# road_id <- roads$road_id[[1]]
# 
# roadworks <- autobahn_roadworks(road_id, flatten = TRUE)
# warnings <- autobahn_warnings(road_id, flatten = TRUE)
# 
# roadwork_details <- autobahn_roadwork_details(roadworks$identifier[[1]], flatten = TRUE)
# warning_details <- autobahn_warning_details(warnings$identifier[[1]], flatten = TRUE)

## ----handelsregister, eval = FALSE--------------------------------------------
# companies <- handelsregister_search("deutsche bahn")

## ----smard, eval = FALSE------------------------------------------------------
# library(ggplot2)
# 
# timestamp <- 1627250400000
# series <- smard_timeseries(410, region = "DE", resolution = "hour", timestamp = timestamp)
# 
# ggplot(series, aes(time, value)) +
#   geom_line() +
#   labs(x = "Time", y = "MW")

## ----dwd, eval = FALSE--------------------------------------------------------
# stations <- dwd_station_overview(c("10865", "G005"), flatten = TRUE)

## ----jobsuche, eval = FALSE---------------------------------------------------
# Sys.setenv(JOBBOERSE_API_KEY = "jobboerse-jobsuche")
# bunddev_auth_set("jobsuche", type = "api_key", env_var = "JOBBOERSE_API_KEY")
# 
# jobs <- jobsuche_search(params = list(was = "data", size = 5), flatten = TRUE)

## ----params, eval = FALSE-----------------------------------------------------
# bunddev_parameters("smard")
# bunddev_parameter_values(smard_timeseries, "resolution")

