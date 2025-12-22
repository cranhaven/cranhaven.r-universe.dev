## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(nhlapi)
urls <- c(
  "https://statsapi.web.nhl.com/api/v1/teams/1",
  "https://statsapi.web.nhl.com/api/v1/people/8477474"
)

nhl_get_data(urls)

## -----------------------------------------------------------------------------
nhl_get_data(urls, flatten = FALSE)

## -----------------------------------------------------------------------------
nhl_get_data(c("https://statsapi.web.nhl.com/api/v1/wrongurl", urls))

## -----------------------------------------------------------------------------
lapply(nhl_get_data(urls), attr, which = "url")

