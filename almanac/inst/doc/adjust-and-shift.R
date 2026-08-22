## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(almanac)
library(lubridate, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
on_christmas <- yearly() %>%
  recur_on_day_of_month(25) %>%
  recur_on_month_of_year("Dec")

x <- as.Date(c("2019-12-24", "2019-12-25"))

adj_following(x, on_christmas)

adj_preceding(x, on_christmas)

## -----------------------------------------------------------------------------
# Saturday / Sunday
x <- as.Date(c("2019-12-21", "2019-12-22"))

on_weekends <- weekly() %>%
  recur_on_weekends()

# Roll Saturday backwards and Sunday forwards
adj_nearest(x, on_weekends)

## -----------------------------------------------------------------------------
on_adjusted_christmas <- radjusted(
  rschedule = on_christmas, 
  adjust_on = on_weekends,
  adjustment = adj_nearest
)

on_adjusted_christmas

## -----------------------------------------------------------------------------
# Note 2004-12-24, which was rolled back from 2004-12-25, a Saturday.
# Note 2005-12-26, which was rolled forward from 2005-12-25, a Sunday.
alma_search("2002-01-01", "2008-01-01", on_adjusted_christmas)

## -----------------------------------------------------------------------------
# A Thursday / Friday pair
x <- as.Date(c("2019-12-19", "2019-12-20"))

# Shift by 1 working day, stepping over weekends
step <- alma_step(x, n = 1, rschedule = on_weekends)

data.frame(
  x = x,
  x_wday = wday(x, label = TRUE),
  step = step,
  step_wday = wday(step, label = TRUE)
)

## -----------------------------------------------------------------------------
# A Saturday / Sunday pair
x <- as.Date(c("2019-12-21", "2019-12-22"))

step <- alma_step(x, c(1, -1), on_weekends)

data.frame(
  x = x,
  x_wday = wday(x, label = TRUE),
  step = step,
  step_wday = wday(step, label = TRUE)
)

## -----------------------------------------------------------------------------
working_day <- stepper(on_weekends)

x %s+% working_day(c(1, -1))

## -----------------------------------------------------------------------------
# A Wednesday
wednesday <- as.Date("2019-12-18")

# Returns Thursday, Friday, Monday
wednesday %s+% working_day(1:3)

