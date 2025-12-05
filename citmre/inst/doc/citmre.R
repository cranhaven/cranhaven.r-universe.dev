## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(citmre)

## ----plot_1, fig.dim = c(7, 5), fig.align = 'center'--------------------------
library(citmre)
head(rmre_data())
plot(rmre_data())

## ----plot_2, fig.dim = c(7, 5), fig.align = 'center'--------------------------
data <- rmre_data(start_date = "2005-03-18", end_date = "2019-06-26")
head(data)
tail(data)
plot(data)

## ----plot_3, fig.dim = c(7, 5), fig.align = 'center'--------------------------
data_log <- rmre_data(start_date = "2005-03-18", end_date = "2019-06-26", log_return = TRUE)
head(data_log)
tail(data_log)
plot(data_log)

## ----plot_456,fig.dim = c(7, 5), fig.align = 'center'-------------------------
# Monthly RMRE
data_m <- rmre_data(start_date = "1998-03-18", end_date = "2019-06-26", frequency = 12)
head(data_m)
tail(data_m)
plot(data_m)

# Quarterly
data_q <- rmre_data(start_date = "1998-03-18", end_date = "2019-06-26", frequency = 4, log_return = T)
head(data_q)
tail(data_q)
plot(data_q)

# Half-year
data_s <- rmre_data(start_date = "1998-03-18", end_date = "2019-06-26", frequency = 2, type = "mean")
head(data_s)
tail(data_s)
plot(data_s)

## ----plotly, eval=FALSE-------------------------------------------------------
#  # Monthly RMRE
#  rmre_data(start_date = "1998-03-18", end_date = "2019-06-26", frequency = 12, plot_data = T)

