## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(outbreaks)
library(i2extras)
library(ggplot2)

raw_dat <- ebola_sim_clean$linelist

## -----------------------------------------------------------------------------
dat <- incidence(
    raw_dat, 
    date_index = "date_of_onset",
    interval = "week",
    groups = "gender"
)[1:20, ]
dat
plot(dat, angle = 45, border_colour = "white")

## -----------------------------------------------------------------------------
out <- fit_curve(dat, model = "poisson", alpha = 0.05)
out
plot(out, angle = 45, border_colour = "white")
growth_rate(out)

## -----------------------------------------------------------------------------
unnest(out, estimates)

## -----------------------------------------------------------------------------
grouped_dat <- incidence(
    raw_dat, 
    date_index = "date_of_onset",
    interval = "week",
    groups = "hospital"
)[1:120, ]
grouped_dat

out <- fit_curve(grouped_dat, model = "poisson", alpha = 0.05)
out

# plot with a prediction interval but not a confidence interval
plot(out, ci = FALSE, pi=TRUE, angle = 45, border_colour = "white")
growth_rate(out)

## -----------------------------------------------------------------------------
out <- fit_curve(grouped_dat, model = "negbin", alpha = 0.05)
is_warning(out)
unnest(is_warning(out), fitting_warning)

## ----rolling_average----------------------------------------------------------
ra <- add_rolling_average(grouped_dat, n = 2L) # group observations with the 2 prior
plot(ra, border_colour = "white", angle = 45) +
    geom_line(aes(x = date_index, y = rolling_average))

