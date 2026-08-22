## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(almanac)

since <- as.Date("1997-09-02")
to <- as.Date("2010-01-01")

## -----------------------------------------------------------------------------
rrule <- daily(since) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- daily(since, until = "1997-12-24")

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- daily(since) %>%
  recur_on_interval(2)

alma_search(since, "1997-10-01", rrule)

## -----------------------------------------------------------------------------
rrule <- daily(since) %>%
  recur_on_interval(10) %>%
  recur_for_count(5)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- daily(since = "1998-01-01", until = "2000-01-31") %>%
  recur_on_month_of_year("January")

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- weekly(since) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- weekly(since, until = "1997-12-24")

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- weekly(since) %>%
  recur_on_interval(2)

alma_search(since, "1997-11-01", rrule)

## -----------------------------------------------------------------------------
rrule <- weekly(since) %>%
  recur_on_day_of_week(c("Tue", "Thur")) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- weekly(since = "1997-09-01", until = "1997-12-24") %>%
  recur_on_day_of_week(c("Mon", "Wed", "Fri")) %>%
  recur_on_interval(2)

alma_search("1997-09-01", to, rrule)

## -----------------------------------------------------------------------------
rrule <- weekly(since) %>%
  recur_on_day_of_week(c("Tue", "Thu")) %>%
  recur_on_interval(2) %>%
  recur_for_count(8)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-05") %>%
  recur_on_day_of_week("Fri", nth = 1) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly(since = "1997-09-05", until = "1997-12-24") %>%
  recur_on_day_of_week("Fri", nth = 1)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-07") %>%
  recur_on_day_of_week("Sun", nth = c(1, -1)) %>%
  recur_on_interval(2) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-22") %>%
  recur_on_day_of_week("Mon", nth = -2) %>%
  recur_for_count(6)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-28") %>%
  recur_on_day_of_month(-3)

alma_search(since, "1997-12-01", rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-02") %>%
  recur_on_day_of_month(c(2, 15)) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-30") %>%
  recur_on_day_of_month(c(1, -1)) %>%
  recur_for_count(10)

alma_search(since, to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-10") %>%
  recur_on_interval(18) %>%
  recur_on_day_of_month(10:15) %>%
  recur_for_count(10)

alma_search("1997-09-10", to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly(since) %>%
  recur_on_interval(2) %>%
  recur_on_day_of_week("Tuesday")

alma_search(since, "1997-12-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-06-10") %>%
  recur_on_month_of_year(c("June", "July")) %>%
  recur_for_count(10)

alma_search("1997-06-10", to, rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-03-10") %>%
  recur_on_month_of_year(c("Jan", "Feb", "Mar")) %>%
  recur_on_interval(2) %>%
  recur_for_count(10)

alma_search("1997-03-10", to, rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-01-01") %>%
  recur_on_day_of_year(c(1, 100, 200)) %>%
  recur_on_interval(3) %>%
  recur_for_count(10)

alma_search("1997-01-01", to, rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-05-19") %>%
  recur_on_day_of_week("Monday", nth = 20)

alma_search("1997-05-19", "2000-01-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-05-12") %>%
  recur_on_week_of_year(20) %>%
  recur_on_day_of_week("Monday")

alma_search("1997-05-12", "2000-01-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-03-13") %>%
  recur_on_day_of_week("Thursday") %>%
  recur_on_month_of_year("March")

alma_search("1997-03-13", "2000-01-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-06-05") %>%
  recur_on_day_of_week("Thursday") %>%
  recur_on_month_of_year(c("Jun", "July", "Aug"))

alma_search("1997-06-05", "1999-01-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly(since) %>%
  recur_on_day_of_week("Friday") %>%
  recur_on_day_of_month(13)

alma_search(since, "2001-01-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1997-09-13") %>%
  recur_on_day_of_week("Saturday") %>%
  recur_on_day_of_month(7:13)

alma_search("1997-09-13", "1998-06-01", rrule)

## -----------------------------------------------------------------------------
rrule <- yearly("1996-11-05") %>%
  recur_on_day_of_week("Tuesday") %>%
  recur_on_day_of_month(2:8) %>%
  recur_on_interval(4) %>%
  recur_on_month_of_year("November")

alma_search("1996-11-05", "2010-12-31", rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-04") %>%
  recur_on_day_of_week(c("Tue", "Wed", "Thu")) %>%
  recur_on_position(3) %>%
  recur_for_count(3)

alma_search("1997-09-04", to, rrule)

## -----------------------------------------------------------------------------
rrule <- monthly("1997-09-29") %>%
  recur_on_day_of_week(1:5) %>%
  recur_on_position(-2)

alma_search("1997-09-29", "1998-12-01", rrule)

## -----------------------------------------------------------------------------
rrule <- weekly("1997-08-05") %>%
  recur_on_interval(2) %>%
  recur_for_count(4) %>%
  recur_on_day_of_week(c("Tue", "Sun")) %>%
  recur_with_week_start("Monday")

# Week 1: 1997-08-04 -> 1997-08-10
# Week 2: 1997-08-11 -> 1997-08-17 (skipped)
# Week 3: 1997-08-18 -> 1997-08-24 
alma_search("1997-08-05", "1998-12-01", rrule)

## -----------------------------------------------------------------------------
rrule <- weekly("1997-08-05") %>%
  recur_on_interval(2) %>%
  recur_for_count(4) %>%
  recur_on_day_of_week(c("Tue", "Sun")) %>%
  recur_with_week_start("Sunday")

# Week 1: 1997-08-03 -> 1997-08-09
# Week 2: 1997-08-10 -> 1997-08-16 (skipped)
# Week 3: 1997-08-17 -> 1997-08-23
# Week 4: 1997-08-24 -> 1997-08-30 (skipped)
# Week 5: 1997-08-31 -> 1997-09-06
alma_search("1997-08-05", "1998-12-01", rrule)

## -----------------------------------------------------------------------------
rrule <- weekly("2007-01-15") %>%
  recur_on_day_of_month(c(15, 30)) %>%
  recur_for_count(5)

alma_search("2007-01-15", "2007-12-31", rrule)

