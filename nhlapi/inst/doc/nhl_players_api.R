## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(nhlapi)

## -----------------------------------------------------------------------------
#  # Get information on players by name
#  nhl_players(c("joe sakic", "PETER forsberg"))

## -----------------------------------------------------------------------------
#  playerIds <- c(8451101, 8458554)
#  nhl_players(playerIds = playerIds)

## -----------------------------------------------------------------------------
#  nhl_players(c("made up player", "Joe Sakic"))
#  
#  nhl_players(playerIds = c("made up player", 8451101))

## -----------------------------------------------------------------------------
#  # Requires `dplyr` attached using library("dplyr")
#  nhl_players(playerIds = playerIds) %>%
#    select(fullName, nationality, shootsCatches, primaryPosition.code)

## -----------------------------------------------------------------------------
#  # Get a specific season statistics using player names
#  nhl_players_seasons(
#    c("Joe SAKIC", "Peter Forsberg"),
#    seasons = 1996:1998
#  )

## -----------------------------------------------------------------------------
#  # Get a specific season playoff statistics
#  nhl_players_seasons(
#    c("Joe SAKIC", "Peter Forsberg"),
#    seasons = 1996,
#    playoffs = TRUE
#  )

## -----------------------------------------------------------------------------
#  # Get a specific season playoff statistics
#  nhl_players_seasons(
#    playerIds = c(8451101, 8458554),
#    seasons = 1996,
#    playoffs = TRUE
#  )

## -----------------------------------------------------------------------------
#  # Get all season statistics for players
#  nhl_players_allseasons(playerIds = c(8451101, 8458554))

## -----------------------------------------------------------------------------
#  # Requires `dplyr` attached using library
#  playerNames <- c("Joe Sakic", "Peter Forsberg")
#  
#  result <- nhl_players(playerNames) %>%
#    left_join(
#      nhl_players_allseasons(playerNames),
#      by = c("id" = "playerId")
#    ) %>%
#    filter(league.name == "National Hockey League") %>%
#    select(fullName, seasonStart, stat.points)
#  
#  head(result)

## -----------------------------------------------------------------------------
#  # Requires `ggplot2` attached using library
#  ggplot(result) +
#    geom_line(aes(seasonStart, stat.points, colour = fullName))

## -----------------------------------------------------------------------------
#  # Requires `highcharter` attached using library
#  result %>%
#    hchart("line", hcaes(seasonStart, stat.points, group = fullName)) %>%
#    hc_add_theme(hc_theme_google())

