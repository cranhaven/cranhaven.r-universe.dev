## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, warning=FALSE, message=FALSE--------------------------
library(rfars)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)

## ----warning=FALSE------------------------------------------------------------
mydata <- rfars::get_gescrss(years=2021, regions = "s")

my_events <- mydata$events

## ----results='asis'-----------------------------------------------------------
my_events %>%
  group_by(soe) %>% summarize(n=n()) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  knitr::kable(format = "html")

## ----results='asis'-----------------------------------------------------------
my_events %>%
  select(-aoi) %>%
  pivot_wider(names_from = "veventnum", values_from = "soe", values_fill = "x",
              names_prefix = "event") %>%
  select(starts_with("event")) %>%
  group_by_all() %>%
  summarize(n=n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  select(event1, event2, n) %>%
  knitr::kable(format = "html")

## ----fig.height=10, fig.width=12----------------------------------------------
my_events %>%
  group_by(year, casenum, veh_no) %>%
  dplyr::rename(event_to = soe) %>%
  mutate(event_from = data.table::shift(event_to, fill = "Pre-Crash")) %>%
  select(event_from, event_to) %>%
  group_by(event_from, event_to) %>% summarize(n=n()) %>%
  group_by(event_from) %>% mutate(n_from = sum(n)) %>%
  mutate(n_pct = n/n_from) %>%
  filter(n_pct>.2, n>5) %>%
  mutate(
    event_from = ifelse(nchar(event_from)>30, paste0(stringr::str_sub(event_from, 1, 30), "..."), event_from),
    #event_to   = paste0(stringr::str_sub(event_to, 1, 30), "..."),
    event_to = stringr::str_wrap(event_to, 40)
    ) %>%
  filter(event_from != event_to) %>%

  ggplot(aes(x=event_from, y=event_to, fill=n_pct, label=scales::percent(n_pct, accuracy = 1))) +
    viridis::scale_fill_viridis() +
    geom_label() +
    scale_x_discrete(position = "top") +
    theme(
      axis.text.x.top = element_text(angle=45, hjust=0),
      axis.ticks = element_blank(),
      #axis.text.x.bottom = element_text(angle=270, hjust = 0, vjust=.5),
      #legend.position = "none"
      )

