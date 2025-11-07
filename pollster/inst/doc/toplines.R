## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pollster)
library(dplyr)
library(knitr)
library(ggplot2)

## -----------------------------------------------------------------------------
topline(df = illinois, variable = voter, weight = weight) %>%
  kable()

## -----------------------------------------------------------------------------
topline(df = illinois, variable = voter, weight = weight, 
        remove = c("(Missing)"), pct = FALSE) %>%
  mutate(Frequency = prettyNum(Frequency, big.mark = ",")) %>%
  kable(digits = 0)

## ---- fig.width=4-------------------------------------------------------------
topline(df = illinois, variable = voter, weight = weight) %>%
  ggplot(aes(Response, Percent, fill = Response)) +
  geom_bar(stat = "identity")

## -----------------------------------------------------------------------------
moe_topline(df = illinois, variable = educ6, weight = weight)

