## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(almanac)

# A rule for weekends
on_weekends <- weekly() %>%
  recur_on_weekends()

## -----------------------------------------------------------------------------
on_christmas <- hol_christmas()
on_christmas

## -----------------------------------------------------------------------------
alma_events(on_christmas, year = 2020:2025)

## -----------------------------------------------------------------------------
on_christmas <- hol_christmas() %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_nearest)

on_christmas

## -----------------------------------------------------------------------------
alma_events(on_christmas, year = 2020:2025)

## -----------------------------------------------------------------------------
on_boxing_day <- hol_christmas() %>%
  hol_offset(by = 1) %>%
  hol_rename("Boxing Day")

on_boxing_day

## -----------------------------------------------------------------------------
alma_events(on_boxing_day, year = 2020:2025)

## -----------------------------------------------------------------------------
on_christmas <- hol_christmas() %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_nearest)

on_boxing_day <- hol_christmas() %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_nearest) %>%
  hol_offset(by = 1) %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_following) %>%
  hol_rename("Boxing Day")

## -----------------------------------------------------------------------------
df <- data.frame(
  christmas = alma_events(on_christmas, year = 2020:2025),
  boxing_day = alma_events(on_boxing_day, year = 2020:2025)
)
df$christmas_weekday <- lubridate::wday(df$christmas, label = TRUE)
df$boxing_day_weekday <- lubridate::wday(df$boxing_day, label = TRUE)

df

## -----------------------------------------------------------------------------
hol_canada_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "July")
  out <- recur_on_day_of_month(out, 1L)
  
  rholiday(rschedule = out, name = "Canada Day")
}

## -----------------------------------------------------------------------------
hol_canada_day()

alma_next(as.Date("2019-01-01"), hol_canada_day())

## -----------------------------------------------------------------------------
cal <- rcalendar(
  hol_christmas(),
  hol_new_years_day(),
  hol_canada_day()
)

cal

## -----------------------------------------------------------------------------
cal_events(cal, year = 2023)

## -----------------------------------------------------------------------------
cal <- rcalendar(
  hol_christmas() %>%
    hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
  hol_new_years_day() %>%
    hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
  # Canada normally rolls their holidays forward to the following Monday
  hol_canada_day() %>%
    hol_observe(adjust_on = on_weekends, adjustment = adj_following)
)

## -----------------------------------------------------------------------------
cal_events(cal, year = 2023)

## -----------------------------------------------------------------------------
cal_events(cal, year = 2011)

## -----------------------------------------------------------------------------
# New Year's Day is gone
cal_events(cal, year = 2011, observed = TRUE)

# And is now listed twice here
cal_events(cal, year = 2010, observed = TRUE)

## -----------------------------------------------------------------------------
x <- as.Date(c(
  "2019-12-25",
  "2019-12-26",
  "2010-12-31",
  "2011-01-01"
))

data.frame(
  x = x,
  name = cal_match(x, cal)
)

## -----------------------------------------------------------------------------
cal_us_federal()

## -----------------------------------------------------------------------------
cal_events(cal_us_federal(), year = 2023)

