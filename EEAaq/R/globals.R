utils::globalVariables(c("Code", "CITY_ID", "CITY_NAME", "nuts", "geometry", "LAU_ID", "coor", ".data", "Var1", "Var2",".","FkObservationLog","Samplingpoint", "SamplingPointId","AirQualityStationEoICode", 'AirQualityStationName',

                         ######### EEAaq_get_data
                         "Start", "End", "AggType", "Value","n_righe", "PollutantName", "AveragingTime", "DatetimeBegin", "Concentration", "DatetimeEnd",

                         #EEAaq_map_stations
                         "Lau_geometry",  "ISO", "NUTS3_ID", "LEVL_CODE","NUTS_ID", "CNTR_CODE",

                         #get_station
                         "NAME_LATN", "POP_2021", "POP_DENS_2", "AREA_KM2", "YEAR", "GISCO_ID", "Longitude", "Latitude", "Altitude", "City", "CityCode"

                         ))
`%within%` <- lubridate::`%within%`
