## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = TRUE, message = FALSE--------------------------------------
library(tidyindex)
library(dplyr)
library(lubridate)
library(lmomco)
library(ggplot2)
library(tsibble)

## ----eval = FALSE-------------------------------------------------------------
#  distribution_fit(.fit = dist_gamma(...))

## -----------------------------------------------------------------------------
dist_gamma(var = ".agg")

## -----------------------------------------------------------------------------
texas_post_office <- queensland %>% 
  filter(name == "TEXAS POST OFFICE") %>% 
  mutate(month = lubridate::month(ym)) 

dt <- texas_post_office |>
  init(id = id, time = ym, group = month) |> 
  temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 24)) |> 
  distribution_fit(.fit = dist_gamma(var = ".agg")) |>
  tidyindex::normalise(.index = norm_quantile(.fit))
dt

## -----------------------------------------------------------------------------
dt$data |> 
  ggplot(aes(x = ym, y = .index)) + 
  geom_hline(yintercept = -2, color = "red",  linewidth = 1) + 
  geom_line() + 
  scale_x_yearmonth(name = "Year", date_break = "2 years", date_label = "%Y") +
   theme_bw() +
  facet_wrap(vars(name), ncol = 1) + 
  theme(panel.grid = element_blank(), 
        legend.position = "bottom") + 
  ylab("SPI")

