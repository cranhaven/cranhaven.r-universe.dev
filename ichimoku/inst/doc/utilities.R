## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(ichimoku)

## ----tradingDays--------------------------------------------------------------
dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
dates
tradingDays(dates)
tradingDays(dates, holidays = c("2020-01-02", "2020-01-03"))
tradingDays(dates, holidays = NULL)

## ----look---------------------------------------------------------------------
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
look(cloud)

strat <- strat(cloud)
look(strat)

grid <- mlgrid(cloud)
look(grid)

## ----xtsdf--------------------------------------------------------------------
cloud <- ichimoku(sample_ohlc_data)
df <- xts_df(cloud)
str(df)

# Preserving custom attributes:
df2 <- xts_df(cloud, keep.attrs = TRUE)
str(df2)

## ----matrixdf-----------------------------------------------------------------
cloud <- ichimoku(sample_ohlc_data)
mcloud <- as.matrix(cloud)
df <- matrix_df(mcloud)
str(df)
str(row.names(df))

## ----dfmerge------------------------------------------------------------------
data1 <- sample_ohlc_data[1:6, ]
data1
data2 <- sample_ohlc_data[4:10, ]
data2
df_merge(data1, data2)

## ----dfappend-----------------------------------------------------------------
data1 <- sample_ohlc_data[1:8, ]
data1
data2 <- sample_ohlc_data[7:10, ]
data2
df_append(data1, data2)

## ----formatposixct------------------------------------------------------------
time <- Sys.time()
format(time)
format_POSIXct(time)

