## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(almanac)

## -----------------------------------------------------------------------------
on_yearly <- yearly()
on_yearly

## -----------------------------------------------------------------------------
alma_search(from = "1990-01-01", to = "1995-12-31", on_yearly)

## -----------------------------------------------------------------------------
on_yearly_jan_5 <- yearly(since = "1990-01-05")

alma_search("1990-01-01", "1995-12-31", on_yearly_jan_5)

## -----------------------------------------------------------------------------
# Same result as above, because the 1988 and 1989 dates are not included. 
alma_search("1988-01-01", "1995-12-31", on_yearly_jan_5)

## -----------------------------------------------------------------------------
alma_events(on_yearly_jan_5)

## -----------------------------------------------------------------------------
# Uses the 10th of the month, pulled from `since`
on_monthly <- monthly(since = "1990-01-10")

x <- as.Date("2000-01-08") + 0:5
x

x_in_set <- alma_in(x, on_monthly)
x_in_set

x[x_in_set]

## -----------------------------------------------------------------------------
since <- "1990-01-01"

on_weekly <- weekly(since = since)

# The first time is "slow"
system.time(alma_search(since, "2000-01-01", on_weekly))

# Repeated access is fast
system.time(alma_search(since, "2000-01-01", on_weekly))

# The entire event set is cached, so even if you change the arguments,
# the operation is still fast.
system.time(alma_search(since, "1990-05-01", on_weekly))

## -----------------------------------------------------------------------------
on_4th_and_16th <- monthly(since = "2000-01-01") %>%
  recur_on_day_of_month(c(4, 16))

alma_search("2000-01-01", "2000-06-01", on_4th_and_16th)

## -----------------------------------------------------------------------------
on_labor_day <- yearly() %>%
  recur_on_month_of_year("Sep") %>%
  recur_on_day_of_week("Monday", nth = 1)

alma_search("2000-01-01", "2005-01-01", on_labor_day)

## -----------------------------------------------------------------------------
on_last_monday_in_sept <- yearly(since = "2000-01-01") %>%
  recur_on_month_of_year("Sep") %>%
  recur_on_day_of_week("Monday", nth = -1)

alma_search("2000-01-01", "2005-01-01", on_last_monday_in_sept)

## -----------------------------------------------------------------------------
on_christmas <- yearly() %>%
  recur_on_month_of_year("Dec") %>%
  recur_on_day_of_month(25)

christmas_or_labor_day <- runion(
  on_christmas,
  on_labor_day
)

alma_search("2000-01-01", "2002-01-01", christmas_or_labor_day)

christmas_or_labor_day_except_2000_labor_day <- rsetdiff(
  christmas_or_labor_day, 
  rcustom("2000-09-04")
)

alma_search("2000-01-01", "2002-01-01", christmas_or_labor_day_except_2000_labor_day)

