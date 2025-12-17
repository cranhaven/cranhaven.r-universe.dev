## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(ichimoku)

## ----oandadatanotrun, eval = FALSE--------------------------------------------
# # ichimoku can create clouds directly from OANDA data, for example:
# cloud <- ichimoku(oanda("USD_JPY"))

## ----pipe, eval = FALSE-------------------------------------------------------
# # Using R 4.1's new pipe operator:
# oanda("USD_JPY") |> ichimoku() |> plot()
# # Or equally using the 'magrittr' pipe:
# oanda("USD_JPY") %>% ichimoku() %>% plot()

## ----viewdata-----------------------------------------------------------------
# Sample OHLC price data is assigned to data frame 'TKR':
TKR <- sample_ohlc_data
head(TKR)

## ----ichimoku-----------------------------------------------------------------
cloud <- ichimoku(TKR)

print(cloud, plot = FALSE, width = 180)

## ----ichimokustr--------------------------------------------------------------
str(cloud)

## ----ichimokusummary----------------------------------------------------------
summary(cloud)

## ----ichimokupreserve---------------------------------------------------------
kumo <- ichimoku(TKR, keep.data = TRUE)

kumo[, "volume"]

## ----holidays, eval = FALSE---------------------------------------------------
# # Holidays can be specified directly via a vector of dates:
# ichimoku(TKR, holidays = c("2020-01-13", "2020-02-11", "2020-02-24"))
# 
# # Or via a functions that returns a vector of dates (e.g. from the 'timeDate' package):
# ichimoku(TKR, holidays = timeDate::holidayLONDON())
# ichimoku(TKR, holidays = timeDate::holidayNYSE())
# 
# # For a market that trades 24/7:
# ichimoku(TKR, holidays = NULL)
# 

## ----bind---------------------------------------------------------------------
index <- index(cloud)
core <- coredata(cloud)

cloud2 <- ichimoku(ichimoku(cbind(index, core), ticker = attr(cloud, "ticker")))
identical(cloud, cloud2)

## ----plot2--------------------------------------------------------------------
plot(cloud, window = "2020-05/", ticker = "SYM (JSE)", subtitle = "Sample Data Series")

## ----usertheme, eval=FALSE----------------------------------------------------
# c("#d9d9d9", "#d7d7d7", "#d1d1d1", "#737373", "#1f1f1f", "#b8b8b8", "#1a1a1a", "#1a1a1a", "#1a1a1a", "#ffffff", "#333333", "#1a1a1a")

## ----plotr, eval=FALSE--------------------------------------------------------
# # To plot an R-type oscillator:
# plot(cloud, type = "r")

## ----plots--------------------------------------------------------------------
plot(cloud, window = "2020-04-01/2020-12-01", theme = "solarized", type = "s")

## ----plotbar------------------------------------------------------------------
plot(kumo, window = "2020-04/2020-11", theme = "mono", type = "bar", custom = "volume")

## ----iplot, eval=FALSE--------------------------------------------------------
# # For an interactive plot:
# iplot(cloud)

## ----archivew, eval=FALSE-----------------------------------------------------
# # Write object to file:
# archive(object, "path/filename")
# # Leave second argument empty to choose file save location from a system dialog:
# archive(object, )

## ----archiver, eval=FALSE-----------------------------------------------------
# # Read from file to object:
# object <- archive("path/filename")
# # Choose a file from an interactive system dialog:
# object <- archive()

