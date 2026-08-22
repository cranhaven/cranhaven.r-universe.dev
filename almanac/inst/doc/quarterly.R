## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(almanac)
library(lubridate, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
on_first_day_of_quarter <- monthly(since = "2000-01-01") %>%
  recur_on_interval(3) %>%
  recur_on_day_of_month(1)

alma_search("2000-01-01", "2002-01-01", on_first_day_of_quarter)

## -----------------------------------------------------------------------------
on_first_day_of_quarter_march_start <- monthly(since = "2000-03-01") %>%
  recur_on_interval(3) %>%
  recur_on_day_of_month(1)

alma_search("2000-01-01", "2002-01-01", on_first_day_of_quarter_march_start)

## -----------------------------------------------------------------------------
on_60th_day_of_q1 <- yearly() %>%
  recur_on_month_of_year(1:3) %>%
  recur_on_day_of_month(1:31) %>%
  recur_on_position(60)

alma_search("2000-01-01", "2002-01-01", on_60th_day_of_q1)

## -----------------------------------------------------------------------------
make_on_nth_doq <- function(since = "1970-01-01", nth = 1L) {
  all_days <- 1:31
  
  on_nth_day_of_q1 <- yearly(since = since) %>%
    recur_on_month_of_year(1:3) %>%
    recur_on_day_of_month(all_days) %>%
    recur_on_position(nth)
  
  on_nth_day_of_q2 <- yearly(since = since) %>%
    recur_on_month_of_year(4:6) %>%
    recur_on_day_of_month(all_days) %>%
    recur_on_position(nth)
  
  on_nth_day_of_q3 <- yearly(since = since) %>%
    recur_on_month_of_year(7:9) %>%
    recur_on_day_of_month(all_days) %>%
    recur_on_position(nth)
    
  on_nth_day_of_q4 <- yearly(since = since) %>%
    recur_on_month_of_year(10:12) %>%
    recur_on_day_of_month(all_days) %>%
    recur_on_position(nth)
  
  on_nth_doq <- runion(
    on_nth_day_of_q1,
    on_nth_day_of_q2,
    on_nth_day_of_q3,
    on_nth_day_of_q4
  )
  
  on_nth_doq
}

## -----------------------------------------------------------------------------
on_60th_doq <- make_on_nth_doq(since = "2000-01-01", nth = 60)

alma_search("2000-01-01", "2002-01-01", on_60th_doq)

## -----------------------------------------------------------------------------
on_last_doq <- make_on_nth_doq(since = "2000-01-01", nth = -1)

alma_search("2000-01-01", "2002-01-01", on_last_doq)

## -----------------------------------------------------------------------------
since <- "2000-01-01"
day <- "Monday"
nth <- 6

on_6th_monday_of_q1 <- yearly(since = since) %>%
  recur_on_month_of_year(1:3) %>%
  recur_on_day_of_week(day) %>%
  recur_on_position(nth)

alma_search("2000-01-01", "2002-01-01", on_6th_monday_of_q1)

## -----------------------------------------------------------------------------
since <- "2000-01-01"
day <- c("Monday", "Tuesday")
nth <- 19

on_19th_monday_or_tuesday_of_q1 <- yearly(since = since) %>%
  recur_on_month_of_year(1:3) %>%
  recur_on_day_of_week(day) %>%
  recur_on_position(nth)

alma_search("2000-01-01", "2002-01-01", on_19th_monday_or_tuesday_of_q1)

## -----------------------------------------------------------------------------
make_on_nth_day_of_week_of_the_quarter <- function(since = "1970-01-01", 
                                                   day = "Monday", 
                                                   nth = 1L) {
  on_nth_of_q1 <- yearly(since = since) %>%
    recur_on_month_of_year(1:3) %>%
    recur_on_day_of_week(day) %>%
    recur_on_position(nth)
  
  on_nth_of_q2 <- yearly(since = since) %>%
    recur_on_month_of_year(4:6) %>%
    recur_on_day_of_week(day) %>%
    recur_on_position(nth)
  
  on_nth_of_q3 <- yearly(since = since) %>%
    recur_on_month_of_year(7:9) %>%
    recur_on_day_of_week(day) %>%
    recur_on_position(nth)
    
  on_nth_of_q4 <- yearly(since = since) %>%
    recur_on_month_of_year(10:12) %>%
    recur_on_day_of_week(day) %>%
    recur_on_position(nth)
  
  on_nth_of_the_quarter <- runion(
    on_nth_of_q1,
    on_nth_of_q2,
    on_nth_of_q3,
    on_nth_of_q4
  )
  
  on_nth_of_the_quarter
}

## -----------------------------------------------------------------------------
on_last_friday_of_the_quarter <- make_on_nth_day_of_week_of_the_quarter(
  since = "2000-01-01", 
  day = "Friday", 
  nth = -1
)

fridays <- alma_search("2000-01-01", "2002-01-01", on_last_friday_of_the_quarter)
fridays

wday(fridays, label = TRUE)

## -----------------------------------------------------------------------------
on_wednesdays <- weekly() %>%
  recur_on_day_of_week("Wednesday")

on_last_friday_of_quarter_or_wednesdays <- runion(
  on_wednesdays,
  on_last_friday_of_the_quarter
)

last_friday_or_wednesdays <- alma_search(
  "2000-01-01", "2002-01-01", 
  on_last_friday_of_quarter_or_wednesdays
)

last_friday_or_wednesdays[1:15]

wday(last_friday_or_wednesdays[1:15], label = TRUE)

