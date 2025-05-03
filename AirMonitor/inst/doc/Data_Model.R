## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 5)

## ----data_model_1-------------------------------------------------------------
library(AirMonitor)

# Recipe to select Washington state monitors in August of 2014:
monitor <-
  
  # 1) start with NW Megafires
  NW_Megafires %>%
  
  # 2) filter to only include Washington state
  monitor_filter(stateCode == "WA") %>%
  
  # 3) filter to only include August
  monitor_filterDate(20150801, 20150901) %>%
  
  # 4) remove monitors with all missing values
  monitor_dropEmpty()

# 'mts_monitor' objects can be identified by their class
class(monitor)

# They alwyas have two elements called 'meta' and 'data'
names(monitor)

# Examine the 'meta' dataframe
dim(monitor$meta)
names(monitor$meta)

# Examine the 'data' dataframe
dim(monitor$data)

# This should always be true
identical(names(monitor$data), c('datetime', monitor$meta$deviceDeploymentID))

## ----monitor_leaflet, results = "hold"----------------------------------------
# First, Obtain the monitor ids by clicking on dots in the interactive map:
NW_Megafires %>% monitor_leaflet()

## ----Methow_Valley, results = "hold"------------------------------------------
# Calculate daily means for the Methow Valley from monitors in Twisp and Winthrop

TwispID <- "99a6ee8e126ff8cf_530470009_04"
WinthropID <- "123035bbdc2bc702_530470010_04"

# Recipe to calculate Methow Valley August Means:
Methow_Valley_AugustMeans <- 
  
  # 1) start with NW Megafires
  NW_Megafires %>%
  
  # 2) select monitors from Twisp and Winthrop
  monitor_select(c(TwispID, WinthropID)) %>%
  
  # 3) average them together hour-by-hour
  monitor_collapse(deviceID = 'MethowValley') %>%
  
  # 4) restrict data to August
  monitor_filterDate(20150801, 20150901) %>%
  
  # 5) calculate daily mean
  monitor_dailyStatistic(mean, minHours = 18) %>%
  
  # 6) round data to one decimal place
  monitor_mutate(round, 1)

# Look at the first week
Methow_Valley_AugustMeans$data[1:7,]

## ----custom_use1--------------------------------------------------------------
# Monitors within 100 km of Spokane, WA
Spokane <-
  NW_Megafires %>%
  monitor_filterByDistance(-117.42, 47.70, 100000) %>%
  monitor_filterDate(20150801, 20150901) %>%
  monitor_dropEmpty()

# Show the daily statistic for one week
Spokane %>% 
  monitor_filterDate(20150801, 20150808) %>%
  monitor_dailyStatistic(mean) %>%
  monitor_getData()

# Custom function to convert from metric ug/m3 to imperial grain/gallon 
my_FUN <- function(x) { return( x * 15.43236 / 0.004546 ) }
Spokane %>% 
  monitor_filterDate(20150801, 20150808) %>%
  monitor_mutate(my_FUN) %>%
  monitor_dailyStatistic(mean) %>%
  monitor_getData()

## ----custom_use2--------------------------------------------------------------
# Pull out the time series data to calculate correlations
Spokane_data <- 
  Spokane %>%
  monitor_getData() %>%
  dplyr::select(-1) # omit 'datetime' column

# Provide human readable names
names(Spokane_data) <- Spokane$meta$locationName

# Find correlation among monitors
cor(Spokane_data, use = "complete.obs")

