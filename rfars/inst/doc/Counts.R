## ----message=F----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(rfars)

## -----------------------------------------------------------------------------
rfars::annual_counts %>%
  filter(what == "crashes", involved == "any") %>%
  ggplot(aes(x=year, y=n)) +
  geom_col() +
  facet_wrap(.~source, nrow=1, scales = "free_y") +
  labs(title = "Total annual crashes by type (FARS = fatal, CRSS = general)", x=NULL, y=NULL) +
  theme_minimal()

rfars::annual_counts %>%
  filter(source=="FARS", involved != "any") %>%
  ggplot(aes(x=year, y=n)) +
  geom_col() +
  facet_wrap(.~involved, scales = "free_y") +
  labs(title = "Annual fatal crashes by factor involved", subtitle = "Derived from FARS data files", x=NULL, y=NULL) +
  theme_minimal() +
  theme(plot.title.position = "plot")

rfars::annual_counts %>%
  filter(source=="CRSS", involved != "any") %>%
  ggplot(aes(x=year, y=n)) +
  geom_col() +
  facet_wrap(.~involved, scales = "free_y") +
  labs(title = "Annual crashes of all severity levels by factor involved", subtitle = "Derived from CRSS data files", x=NULL, y=NULL) +
  theme_minimal() +
  theme(plot.title.position = "plot")

## ----message=FALSE------------------------------------------------------------
myFARS <- get_fars(years = c(2022, 2023), proceed = T)

## ----results='asis'-----------------------------------------------------------
my_counts <- counts(
  df = myFARS,
  where = list(states = "VA"),
  what = "crashes",
  interval = c("year", "month")
  )

## ----results='asis'-----------------------------------------------------------
knitr::kable(my_counts, format = "html")

## -----------------------------------------------------------------------------
my_counts %>%
  mutate_at("year", factor) %>%
  ggplot(aes(x=month, y=n, group=year, color=year, label=scales::comma(n))) +
  geom_line(linewidth = 1.5) + 
  labs(x=NULL, y=NULL, title = "Fatal Crashes in Virginia") +
  theme_minimal() +
  theme(plot.title.position = "plot")

## -----------------------------------------------------------------------------
my_counts %>%
  mutate(date = lubridate::make_date(year, month)) %>%
  ggplot(aes(x=date, y=n, label=scales::comma(n))) +
  geom_col() + 
  labs(x=NULL, y=NULL, title = "Fatal Crashes in Virginia")  +
  theme(plot.title.position = "plot")

## ----results='asis'-----------------------------------------------------------
counts(
  myFARS,
  where = list(states = "VA"),
  what = "fatalities",
  interval = c("year")
  ) %>%
  knitr::kable(format = "html")

## ----results='asis'-----------------------------------------------------------
counts(
  df = myFARS,
  where = list(states = "VA"),
  what = "fatalities",
  interval = c("year"),
  involved = "speeding"
) %>%
  knitr::kable(format = "html")

## ----results='asis'-----------------------------------------------------------
counts(
  myFARS,
  where = list(states = "VA", urb="rural"),
  what = "fatalities",
  interval = c("year"),
  involved = "speeding"
) %>%
  knitr::kable(format = "html")

## ----results='asis'-----------------------------------------------------------
counts(
  df = myFARS,
  where = list(states = "VA"),
  what = "crashes",
  interval = "year",
  involved = "each"
) %>%
  pivot_wider(names_from = "year", values_from = "n") %>%
  arrange(desc(`2023`)) %>%
  knitr::kable(format = "html")

## -----------------------------------------------------------------------------
compare_counts(
  df = myFARS,
  interval = "year",
  involved = "speeding",
  what = "fatalities",
  where = list(states = "VA", urb="rural"),
  where2 = list(states = "VA", urb="urban")
  ) %>%
  ggplot(aes(x=factor(year), y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    facet_wrap(.~urb) +
    labs(x=NULL, y=NULL, title = "Speeding-Related Fatalities in Virginia", fill=NULL) 

## -----------------------------------------------------------------------------
compare_counts(
  df = myFARS,
  where = list(states = "VA"),
  interval = "year",
  involved = "speeding",
  involved2 = "distracted driver",
  what = "crashes",
  ) %>%
  ggplot(aes(x=factor(year), y=n, label=scales::comma(n))) + 
    geom_col() + 
    geom_label(vjust=1.2) +
    facet_wrap(.~involved) +
    labs(x=NULL, y=NULL, title = "Speeding- and Distraction-Related Crashes in Virginia", fill=NULL)

