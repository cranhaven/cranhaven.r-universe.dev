## ----message=F----------------------------------------------------------------
library(rfars)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
myFARS <- get_fars(years = 2021, states = "VA", proceed = T)

## ----results='asis'-----------------------------------------------------------
my_counts <- counts(
  myFARS,
  what = "crashes",
  interval = c("month")
  )

## ----results='asis'-----------------------------------------------------------
knitr::kable(my_counts, format = "html")

## -----------------------------------------------------------------------------
my_counts %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    labs(x=NULL, y=NULL, title = "Fatal Crashes in Virginia")

## -----------------------------------------------------------------------------
counts(
  myFARS,
  what = "fatalities",
  interval = c("month")
  ) %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    labs(x=NULL, y=NULL, title = "Fatalities in Virginia")

## -----------------------------------------------------------------------------
counts(myFARS,
       what = "fatalities",
       interval = c("month"),
       involved = "speeding"
       ) %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    labs(x=NULL, y=NULL, title = "Speeding-Related Fatalities in Virginia")

## -----------------------------------------------------------------------------
counts(myFARS,
       what = "fatalities",
       where = list(urb="rural"),
       interval = c("month"),
       involved = "speeding"
       ) %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    labs(x=NULL, y=NULL, title = "Speeding-Related Fatalities in Rural Virginia")

## -----------------------------------------------------------------------------
compare_counts(
  df = myFARS,
  interval = "month",
  involved = "speeding",
  what = "fatalities",
  where = list(urb="rural"),
  where2 = list(urb="urban")
  ) %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    facet_wrap(.~urb) +
    labs(x=NULL, y=NULL, title = "Speeding-Related Fatalities in Virginia", fill=NULL)

## -----------------------------------------------------------------------------
compare_counts(
  df = myFARS,
  interval = "month",
  involved = "speeding",
  involved2 = "distracted driver",
  what = "crashes",
  ) %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    facet_wrap(.~involved) +
    labs(x=NULL, y=NULL, title = "Speeding- and Distraction-Related Crashes in Virginia", fill=NULL)

