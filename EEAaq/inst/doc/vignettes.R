## ----setup--------------------------------------------------------------------
library(EEAaq)
`%>%` <- dplyr::`%>%`

## -----------------------------------------------------------------------------
#' ### Download PM10 data for the province (NUTS-3) of Milano (Italy) from January 1st 2024 to January 31st, 2025
 IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
 IDstations <- IDstations %>%
                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
               dplyr::pull(AirQualityStationEoICode) %>%
                unique()
 data <- EEAaq_get_data(IDstations = IDstations, pollutants = c("PM10", "NO2"),
                        from = "2024-01-01", to = "2025-01-31", verbose = TRUE)

## -----------------------------------------------------------------------------
# Preview the first few rows of the dataset
head(data)

## -----------------------------------------------------------------------------
unique(data$AirQualityStationEoICode)

## -----------------------------------------------------------------------------
# Static map of available stations across the whole country. External borders are given by the
#' ###     union of the available regions (NUTS-2), while municipalities (LAUs) are used as inner borders.
 EEAaq_map_stations(data = data,
                   NUTS_extborder = "NUTS2", NUTS_intborder = "LAU",
                   color = TRUE, dynamic = FALSE)

## -----------------------------------------------------------------------------
 EEAaq_map_stations(data = data,
                  NUTS_extborder = "NUTS2", NUTS_intborder = "NUTS3",
                  color = TRUE, dynamic = TRUE)


## -----------------------------------------------------------------------------
summ <- EEAaq_summary(data = data)

## -----------------------------------------------------------------------------
summ$Summary

## -----------------------------------------------------------------------------
summ$Summary_byStat$Mean_byStat

## -----------------------------------------------------------------------------
summ$Corr_Matrix

## -----------------------------------------------------------------------------
t_aggr <- EEAaq_time_aggregate(
  data = data,
  frequency = "monthly",
  aggr_fun = c("min", "max", "mean", "median" )
)

## -----------------------------------------------------------------------------
t_aggr$TimeAggr

## -----------------------------------------------------------------------------
t_aggr$TimeAggr_byPollutant$PM10

## -----------------------------------------------------------------------------
EEAaq::EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
               distinct = TRUE, gradient = FALSE,
               dynamic = FALSE,
              NUTS_filler = "NUTS3", NUTS_extborder = "NUTS2")

## -----------------------------------------------------------------------------
 t_aggr$TimeAggr_byPollutant$NO2

## -----------------------------------------------------------------------------
EEAaq::EEAaq_idw_map(
  data = t_aggr$TimeAggr_byPollutant$NO2 %>% dplyr::filter(Date %in% c("2024-01-01","2024-02-01")),
  pollutant = "NO2",
  aggr_fun = "max",
  distinct = TRUE,
  gradient = TRUE,
  idp = 2,NUTS_extborder = "NUTS2",NUTS_filler = "LAU"
)

