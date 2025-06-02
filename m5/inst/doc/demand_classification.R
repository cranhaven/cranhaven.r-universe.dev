## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE, 
  message = FALSE,
  comment = "#>",
  fig.width = 8, 
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(m5)
library(zeallot)
library(ggplot2)

m5_download('data')

c(sales_train,
  sales_test,
  sell_prices,
  calendar,
  weights) %<-% m5_get_raw_evaluation('data')

## ----prepare------------------------------------------------------------------
m5_data  <-
    m5_prepare(sales_train, sales_test, calendar, sell_prices)
head(m5_data)

## ----classify-----------------------------------------------------------------
m5_demand <- m5_demand_type(m5_data)

foods_demand <- 
  m5_demand[startsWith(as.character(m5_demand$item_id), "FOODS_1")]

plot <-
  ggplot(foods_demand) +
  geom_point(aes(log(cv2), log(adi),
                 item_id = item_id, col = demand_type)) +
  geom_hline(yintercept = log(1.32)) +
  geom_vline(xintercept = log(0.49)) +
  theme_minimal()

plot

