## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(nhlapi)

## -----------------------------------------------------------------------------
#  # Get meta information on all teams
#  nhl_teams()

## -----------------------------------------------------------------------------
#  # Get current rosters for all teams
#  rosters <- nhl_teams_rosters()
#  
#  # View the current roster for the Devils
#  roster_devils <- rosters %>%
#    filter(name == "New Jersey Devils") %>%
#    pull(roster.roster) %>%
#    # This is still a list, extract the first element
#    first()
#  roster_devils

## -----------------------------------------------------------------------------
#  # Get rosters for a specific season, all teams
#  rosters_1993 <- nhl_teams_rosters(seasons = 1993)
#  
#  # Select the Quebec Nordiques
#  roster_nordiques_1993 <- rosters_1993 %>%
#    filter(name == "Quebec Nordiques") %>%
#    pull(roster.roster)

## -----------------------------------------------------------------------------
#  # Works with multiple seasons
#  rosters_1993_1994 <- nhl_teams_rosters(
#    seasons = c(1993:1995)
#  )
#  
#  # It is a bit nicer though (for traffic) to get less data
#  # and make more specific requests - e.g. only get the Nordiques
#  rosters_1993_1994 <- nhl_teams_rosters(
#    teamIds = 32,
#    seasons = c(1993:1995)
#  )

## -----------------------------------------------------------------------------
#  # Previous game for all teams
#  nhl_teams_shedule_previous()
#  
#  # Previous game for selected teams
#  nhl_teams_shedule_previous(c(1,3,5))

## -----------------------------------------------------------------------------
#  # First get the API URL for the relevant game
#  game_url <- nhl_teams_shedule_previous(1) %>%
#    extract2("previousGameSchedule.dates") %>% first() %>%
#    extract2("games") %>% first() %>%
#    extract2("link")
#  
#  # Now use `nhl_get_data()` to retrieve the data
#  game_data <- file.path("https://statsapi.web.nhl.com", game_url) %>%
#    nhl_get_data()
#  
#  # This is a very complex nested endpoint, for instance,
#  # look at the recorded events in the game
#  game_plays <- game_data %>% first() %>%
#    extract2("liveData") %>%
#    extract2("plays") %>%
#    extract2("allPlays") %>%
#    as_tibble()
#  
#  game_plays

## -----------------------------------------------------------------------------
#  # Next game for all teams
#  nhl_teams_shedule_next()
#  
#  # Next game for selected teams
#  nhl_teams_shedule_next(c(1,3,5))

## -----------------------------------------------------------------------------
#  # All teams, current season
#  nhl_teams_stats()

## -----------------------------------------------------------------------------
#  # Selected teams, selected seasons
#  nhl_teams_stats(1, 2005:2006)

## -----------------------------------------------------------------------------
#  # Get 2 seasons for the Devils:
#  devils_stats <- nhl_teams_stats(1, 2005:2006)
#  
#  # Look at the teamStats object for the first of them
#  # Requires `dplyr` attached using library
#  devils_stats %>%
#    extract2("teamStats") %>% first() %>%
#    extract2("splits") %>% first() %>%
#    slice(1) %>%
#    select(stat.gamesPlayed, stat.wins, stat.losses)

