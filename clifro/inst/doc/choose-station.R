## ---- echo=FALSE--------------------------------------------------------------
library(clifro)

## ---- eval = FALSE------------------------------------------------------------
#  # Equivalent to searching for status = "open" on CliFro
#  # Note the search string is not case sensitive
#  cf_find_station("takaka", status = "all")

## ---- echo = FALSE------------------------------------------------------------
takaka.df = structure(list(name = c("Takaka, Kotinga Road", "Riwaka At Takaka Hill", 
"Takaka Pohara", "Takaka At Harwoods", "Takaka At Kotinga", "Takaka @ Canaan", 
"Upper Takaka 2", "Takaka Ews", "Takaka Aero Raws", "Takaka, Kotinga 2", 
"Upper Takaka", "Takaka,Patons Rock", "Takaka,Kotinga 1", "Takaka Aero", 
"Takaka Hill", "Takaka,Bu Bu", "Takaka"), network = c("F02882", 
"O12090", "F02884", "F15292", "F15291", "F0299A", "F12083", "F02885", 
"O00957", "F02883", "F12082", "F02772", "F02971", "F02871", "F12081", 
"F02872", "F02881"), agent = c(3788L, 44046L, 3790L, 44050L, 
44051L, 44072L, 11519L, 23849L, 41196L, 3789L, 7316L, 3779L, 
3794L, 3785L, 3833L, 3786L, 3787L), start = structure(c(18273600, 
316263600, 520516800, 570020400, 704030400, 760014000, 805464000, 
1020081600, 1439294400, 502110000, 688820400, -7992000, -255182400, 
-1046692800, -704894400, -1159875000, -2082886200), class = c("POSIXct", 
"POSIXt"), tzone = "NZ"), end = structure(c(1597665600, 1597665600, 
1597665600, 1597665600, 1597665600, 1597665600, 1597665600, 1597665600, 
1597665600, 1341057600, 720442800, 157719600, 49809600, 7732800, 
-320932800, -760190400, -1333452600), class = c("POSIXct", "POSIXt"
), tzone = "NZ"), open = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE), distance = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA), lat = c(-40.872, -41.03192, -40.845, 
-41.03094, -40.87068, -40.93987, -41.01516, -40.86364, -40.81531, 
-40.882, -41.051, -40.789, -40.9, -40.816, -41.017, -40.85, -40.817
), lon = c(172.809, 172.84439, 172.867, 172.79802, 172.808, 172.90821, 
172.82582, 172.80568, 172.7765, 172.801, 172.833, 172.757, 172.775, 
172.772, 172.867, 172.733, 172.8)), class = "data.frame", row.names = c(NA, 
-17L))

new("cfStation", takaka.df)

## ---- eval = FALSE------------------------------------------------------------
#  cf_find_station("takaka", status = "open")

## ---- echo = FALSE------------------------------------------------------------
takaka.df = structure(list(name = c("Takaka, Kotinga Road", "Riwaka At Takaka Hill", 
"Takaka Pohara", "Takaka At Harwoods", "Takaka At Kotinga", "Takaka @ Canaan", 
"Upper Takaka 2", "Takaka Ews", "Takaka Aero Raws"), network = c("F02882", 
"O12090", "F02884", "F15292", "F15291", "F0299A", "F12083", "F02885", 
"O00957"), agent = c(3788L, 44046L, 3790L, 44050L, 44051L, 44072L, 
11519L, 23849L, 41196L), start = structure(c(18273600, 316263600, 
520516800, 570020400, 704030400, 760014000, 805464000, 1020081600, 
1439294400), class = c("POSIXct", "POSIXt"), tzone = "NZ"), end = structure(c(1597665600, 
1597665600, 1597665600, 1597665600, 1597665600, 1597665600, 1597665600, 
1597665600, 1597665600), class = c("POSIXct", "POSIXt"), tzone = "NZ"), 
    open = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
    TRUE), distance = c(NA, NA, NA, NA, NA, NA, NA, NA, NA), 
    lat = c(-40.872, -41.03192, -40.845, -41.03094, -40.87068, 
    -40.93987, -41.01516, -40.86364, -40.81531), lon = c(172.809, 
    172.84439, 172.867, 172.79802, 172.808, 172.90821, 172.82582, 
    172.80568, 172.7765)), class = "data.frame", row.names = c(NA, 
-9L))

new("cfStation", takaka.df)

## ---- eval = FALSE------------------------------------------------------------
#  cf_find_station("f028", search = "network", status = "all")

## ---- echo = FALSE------------------------------------------------------------
xx.df = structure(list(name = c("Takaka, Kotinga Road", "Takaka Pohara", 
"Takaka Ews", "Aorere At Salisbury Bridge", "Takaka, Kotinga 2", 
"Nelson,Mckay Hut", "Gouland Downs", "Golden Bay,Table Hl I", 
"Golden Bay,Table Hl 2", "Tarakohe", "Takaka Aero", "Totaranui", 
"Takaka,Bu Bu", "Takaka", "Quartz Ranges"), network = c("F02882", 
"F02884", "F02885", "F02854", "F02883", "F02821", "F02831", "F02852", 
"F02853", "F02891", "F02871", "F02892", "F02872", "F02881", "F02851"
), agent = c(3788L, 3790L, 23849L, 44020L, 3789L, 3780L, 3781L, 
3783L, 3784L, 3791L, 3785L, 3792L, 3786L, 3787L, 3782L), start = structure(c(18273600, 
520516800, 1020081600, 1311595200, 502110000, 417960000, 467982000, 
233928000, 233928000, -1188819000, -1046692800, -410270400, -1159875000, 
-2082886200, -2177494200), class = c("POSIXct", "POSIXt"), tzone = "NZ"), 
    end = structure(c(1597665600, 1597665600, 1597665600, 1597665600, 
    1341057600, 745416000, 745416000, 690807600, 690807600, 599569200, 
    7732800, -294667200, -760190400, -1333452600, -2125049400
    ), class = c("POSIXct", "POSIXt"), tzone = "NZ"), open = c(TRUE, 
    TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE), distance = c(NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), lat = c(-40.872, 
    -40.845, -40.86364, -40.80236, -40.882, -40.89, -40.892, 
    -40.807, -40.807, -40.825, -40.816, -40.823, -40.85, -40.817, 
    -40.867), lon = c(172.809, 172.867, 172.80568, 172.53328, 
    172.801, 172.213, 172.351, 172.556, 172.556, 172.898, 172.772, 
    173.002, 172.733, 172.8, 172.517)), class = "data.frame", row.names = c(NA, 
-15L))

new("cfStation", xx.df)

## ---- echo = FALSE------------------------------------------------------------
open.queenstown.stations.df = dget(system.file("extdata", "queenStations", package = "clifro"))
open.queenstown.stations = new("cfStation", open.queenstown.stations.df)

## ---- eval = FALSE------------------------------------------------------------
#  # Partial match for the Queenstown region
#  open.queenstown.stations = cf_find_station("queen", search = "region")

## ---- echo = FALSE------------------------------------------------------------
takaka.town.df = structure(list(name = c("Takaka, Kotinga Road", "Takaka Pohara", 
"Anatoki At Happy Sams", "Takaka At Kotinga", "Takaka Ews", "Motupiko At Reillys Bridge", 
"Takaka Aero Raws"), network = c("F02882", "F02884", "F15293", 
"F15291", "F02885", "F1529M", "O00957"), agent = c(3788L, 3790L, 
44015L, 44051L, 23849L, 44041L, 41196L), start = structure(c(18273600, 
520516800, 657284400, 704030400, 1020081600, 1164711600, 1439294400
), class = c("POSIXct", "POSIXt"), tzone = "NZ"), end = structure(c(1598788800, 
1598788800, 1598788800, 1598788800, 1598788800, 1598788800, 1598788800
), class = c("POSIXct", "POSIXt"), tzone = "NZ"), open = c(TRUE, 
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), distance = c(2.6, 5.7, 5.8, 
2.4, 1.6, 2.7, 4.3), lat = c(-40.872, -40.845, -40.88587, -40.87068, 
-40.86364, -40.85607, -40.81531), lon = c(172.809, 172.867, 172.74982, 
172.808, 172.80568, 172.83162, 172.7765)), class = "data.frame", row.names = c(NA, 
-7L))
takaka.town.st = new("cfStation", takaka.town.df)

## ---- eval = FALSE------------------------------------------------------------
#  takaka.town.st = cf_find_station(lat = -40.85, long = 172.8, rad = 10, search = "latlong")
#  
#  # Print the result, but remove the lat and lon columns to fit the page
#  takaka.town.st[, -c(8, 9)]

## ---- echo = -1---------------------------------------------------------------
takaka.town.st[, -c(8, 9)]

# We may rather order the stations by distance from the township
takaka.town.st[order(takaka.town.st$distance), -c(8, 9)]

## ---- echo = FALSE------------------------------------------------------------
hourly.rain.dt = new("cfDatatype"
    , dt_name = "Precipitation"
    , dt_type = "Rain (fixed periods)"
    , dt_sel_option_names = list("Hourly")
    , dt_sel_combo_name = NA_character_
    , dt_param = structure("ls_ra,1,2,3,4", .Names = "dt1")
    , dt_sel_option_params = list(structure("182", .Names = "prm2"))
    , dt_selected_options = list(2)
    , dt_option_length = 4
)

## ---- eval = FALSE------------------------------------------------------------
#  # Create a clifro datatype for hourly rain
#  hourly.rain.dt = cf_datatype(3, 1, 2)
#  hourly.rain.dt

## ---- echo = FALSE------------------------------------------------------------
hourly.rain.dt

## ---- eval = FALSE------------------------------------------------------------
#  # Conduct the search
#  cf_find_station("takaka", datatype = hourly.rain.dt)

## ---- echo = FALSE------------------------------------------------------------
kaitaia.df = structure(list(name = c("Kaitaia Aero Ews", "Trounson Cws", "Russell Cws", 
"Kaikohe Aws", "Purerua Aws", "Cape Reinga Aws", "Kerikeri Aerodrome Aws", 
"Kaitaia Ews", "Dargaville 2 Ews", "Kerikeri Ews"), network = c("A53026", 
"A53762", "A54212", "A53487", "A54101", "A42462", "A53295", "A53127", 
"A53987", "A53191"), agent = c(18183L, 37131L, 41262L, 1134L, 
1196L, 1002L, 37258L, 17067L, 25119L, 1056L), start = structure(c(960984000, 
1244030400, 1459771200, 500727600, 788871600, 788871600, 1214395200, 
913806000, 1067425200, 1025179200), class = c("POSIXct", "POSIXt"
), tzone = "NZ"), end = structure(c(1598702400, 1598702400, 1598702400, 
1598616000, 1598616000, 1598616000, 1598616000, 1598443200, 1598011200, 
1597924800), class = c("POSIXct", "POSIXt"), tzone = "NZ"), open = c(TRUE, 
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), distance = c(NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA), lat = c(-35.0677, -35.72035, 
-35.26835, -35.4172, -35.129, -34.42963, -35.262, -35.13352, 
-35.93145, -35.183), lon = c(173.2874, 173.65153, 174.136, 173.8229, 
174.015, 172.68186, 173.911, 173.26294, 173.85317, 173.926)), class = "data.frame", row.names = c(NA, 
-10L))
kaitaia.st = new("cfStation", kaitaia.df)
my.composite.search = takaka.town.st + kaitaia.st

## ---- eval = FALSE------------------------------------------------------------
#  my.composite.search = takaka.town.st + cf_find_station("kaitaia",
#                                                         search = "region",
#                                                         datatype = hourly.rain.dt)
#  my.composite.search

## ---- echo = -1---------------------------------------------------------------
my.composite.search

# How long have these stations been open for?
transform(my.composite.search, ndays = round(end - start))[, c(1, 10)]

## ----eval = FALSE-------------------------------------------------------------
#  # First, search for the stations
#  all.auckland.st = cf_find_station("auckland", search = "region", status = "all")

## ----eval=FALSE---------------------------------------------------------------
#  # Then save these as a KML
#  cf_save_kml(all.auckland.st, file_name = "all_auckland_stations")

