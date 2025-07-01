## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------
library(tidyr)
library(dplyr)
library(ggplot2)
library(geosphere)
library(diverse)
library(tibble)
library(foreign)

data(matches, package = "volleystat")
data(players, package = "volleystat")
data(matchstats, package = "volleystat")
data(staff, package = "volleystat")
data(sets, package = "volleystat")
data(team_adresses, package = "volleystat")
data(match_adresses, package = "volleystat")

## ----echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----------
    # Select away team matches

    df_1 <- matches %>% 
            rename(team_id_2  = team_id,
                   team_name_2 = team_name,
                   set_won_team_2   = set_won) %>% 
            filter(match == "away") %>%
            select(-match)

    # Select home team matches and join away team information
    df_home <- matches %>% 
               rename(team_id_1   = team_id,
                      team_name_1 = team_name,
                      set_won_team_1   = set_won) %>% 
               filter(match == "home") %>% 
               select(-match) %>% left_join(df_1) %>% mutate(match = "home")

    # Select home team matches
    df_1 <- matches %>% 
            rename(team_id_2  = team_id,
                   team_name_2 = team_name,
                   set_won_team_2   = set_won) %>% 
            filter(match == "home") %>%
            select(-match)

    # Select home team matches and join home team information
    df_away <-  matches %>% 
                rename(team_id_1   = team_id,
                       team_name_1 = team_name,
                       set_won_team_1   = set_won) %>% 
                filter(match == "away") %>% 
                select(-match) %>% left_join(df_1) %>% mutate(match = "away")

    # Bind datasets
    df <- bind_rows(df_home, df_away)

    rm(df_away, df_1, df_home)


## ----echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----------
# Teams get 0 points if they win at most one set, 3 points of they win 3 sets
# and the opponent wins at most one set. In case of tie-breaks, points are splitted.
# The winning team gets 2 points and the other team 1 point.

df <- df %>% 
      mutate(match_points_team_1 = 
             case_when(set_won_team_1 == 0 ~ 0, 
                       set_won_team_1 == 1 ~ 0,
                       set_won_team_1 == 2 ~ 1,
                       set_won_team_1 == 3 & set_won_team_2 == 2 ~ 2,
                       set_won_team_1 == 3 & set_won_team_2 == 1 ~ 3,
                       set_won_team_1 == 3 & set_won_team_2 == 0 ~ 3)) %>% 
      mutate(match_points_team_2 = 
             case_when(set_won_team_2 == 0 ~ 0, 
                       set_won_team_2 == 1 ~ 0,
                       set_won_team_2 == 2 ~ 1,
                       set_won_team_2 == 3 & set_won_team_1 == 2 ~ 2,
                       set_won_team_2 == 3 & set_won_team_1 == 1 ~ 3,
                       set_won_team_2 == 3 & set_won_team_1 == 0 ~ 3)) %>% 
      mutate(match_points_team_1 = 
               replace(match_points_team_1, competition_stage != "Main round", 0),
             match_points_team_2 = 
               replace(match_points_team_2, competition_stage != "Main round", 0))

## ----echo=TRUE-----------------------------------------------------------
# Generate league_points (sum of match_points before match) and 
# league_sets (sum of sets won before match)

df <- df %>% 
      group_by(league_gender, season_id, team_id_1) %>%
      arrange(date_time) %>%
      mutate(league_points_team_1 = cumsum(match_points_team_1),
             league_points_team_1 = league_points_team_1 - match_points_team_1) %>% 
      mutate(league_sets_team_1 = cumsum(set_won_team_1),
             league_sets_team_1 = league_sets_team_1 - set_won_team_1) %>% 
      ungroup() %>% 
      group_by(league_gender, season_id, team_id_2) %>%
      arrange(date_time) %>%
      mutate(league_points_team_2 = cumsum(match_points_team_2),
             league_points_team_2 = league_points_team_2 - match_points_team_2)  %>%
      mutate(league_sets_team_2 = cumsum(set_won_team_2),
             league_sets_team_2 = league_sets_team_2 - set_won_team_2) %>% 
      ungroup() %>% 
      mutate(league_points_diff   = league_points_team_1 - league_points_team_2,
             league_sets_quotient = league_sets_team_1/league_sets_team_2,
             league_sets_diff     = league_sets_team_1 - league_sets_team_2) %>% 
      select(-c(set_won_team_1, set_won_team_2))

## ----message=FALSE, warning=FALSE, paged.print=FALSE---------------------
# Rename coordinates of the match location
df <- df %>% left_join(match_adresses) %>% rename(lat_match = lat,
                                                  lon_match = lon)

# Select relevant information from team_adresses
tmp <- team_adresses %>% select(season_id, team_id, gym_adress, max_spectators, lon, lat)

# Join the dataset twice for team 1 and team 2
df <- df %>% 
      left_join(tmp, by = c("season_id" = "season_id", "team_id_1" = "team_id")) %>% 
      rename(lon_team_1 = lon,
             lat_team_1 = lat,
             gym_adress_team_1 = gym_adress,
             max_spectators_team_1 = max_spectators) %>% 
      left_join(tmp, by = c("season_id" = "season_id", "team_id_2" = "team_id")) %>% 
      rename(lon_team_2 = lon,
             lat_team_2 = lat,
             gym_adress_team_2 = gym_adress,
             max_spectators_team_2 = max_spectators)

# Compute the distance of team 1's home gym to match gym
p1 <- as.matrix(df %>% select(lon_team_1, lat_team_1))
p2 <- as.matrix(df %>% select(lon_match, lat_match))

# Compute the distance in km to the dataset
df$distance_team_1 <- distMeeus(p1, p2)/1000

# Compute the distance of team 1's home gym to match gym
p1 <- as.matrix(df %>% select(lon_team_2, lat_team_2))
p2 <- as.matrix(df %>% select(lon_match, lat_match))

# Compute the distance in km to the dataset
df$distance_team_2 <- distMeeus(p1, p2)/1000

rm(p1, p2, tmp)

## ----echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----------

df <- df %>% 
      mutate(gym_capacity = 
               case_when(match == "home" & (lon_match == lon_team_1 & lat_match == lat_team_1) ~ max_spectators_team_1, 
                         match == "away" & (lon_match == lon_team_2 & lat_match == lat_team_2) ~ max_spectators_team_2
                       )) %>% 
      select(-starts_with("lon_"), -starts_with("lat"), -c(max_spectators_team_1, max_spectators_team_2,
             adress, gym_adress_team_1, gym_adress_team_2))


## ------------------------------------------------------------------------
# Join nationality information to matchstats
lineups <- matchstats %>% 
           select(league_gender:player_id) %>% 
           left_join(players, by = c("league_gender", "season_id", "team_id", "player_id")) %>% 
           select(-c(team_name, lastname, firstname, gender, shirt_number, player_id))

# Prepare dataset to compute diversity measures using diverse package
nat_diversity <- lineups %>% 
                 group_by(season_id, match_id, team_id, nationality) %>% 
                 summarise(n = n()) %>% 
                 unite(season_id, match_id, team_id, col = "id", sep = "_") %>% 
                 mutate(id = factor(id)) %>% 
                 ungroup()

# Compute diversity measures, separate ids and select relevant measures
nat_diversity <- diversity(as.data.frame(nat_diversity)) %>% 
                 rownames_to_column(var = "id") %>% 
                 separate(id, into = c("season_id", "match_id", "team_id"), sep = "_", convert = TRUE) %>% 
                 select(season_id:team_id, HHI, blau.index) %>% 
                 rename(herfindahl_herschman_index = HHI,
                        blau_index = blau.index)

# Roster size
roster <- players %>% 
          group_by(season_id, team_id) %>% 
          summarise(roster_size_team_1 = n())  %>% 
          ungroup()

# Mean height
height <- matchstats %>% 
          select(league_gender:player_id) %>% 
          left_join(players, by = c("league_gender", "season_id", "team_id", "player_id")) %>% 
          select(-c(team_name, lastname, firstname, gender, shirt_number, player_id)) %>%
          mutate(height = replace(height, height == 78, 178)) %>% 
          group_by(season_id, match_id, team_id) %>% 
          summarise(mean_height = mean(height, na.rm = TRUE)) %>% 
          ungroup()

# Join to final dataset
df <- df %>% left_join(nat_diversity, by = c("season_id" = "season_id", 
                                       "match_id" = "match_id", 
                                       "team_id_1" = "team_id")) %>% 
             left_join(roster, by = c("season_id" = "season_id", 
                                       "team_id_1" = "team_id")) %>% 
             left_join(height, by = c("season_id" = "season_id", 
                                       "match_id" = "match_id", 
                                       "team_id_1" = "team_id"))

# Generate winning variable
df <- df %>% mutate(win = case_when(match_points_team_1 == 3 ~ 1,
                                    match_points_team_1 <= 2 ~ 0))

rm(nat_diversity, roster, height, lineups)

## ------------------------------------------------------------------------
df <- df %>%
      mutate(weekday = factor(weekdays(date_time)),
             date    = format(date_time, '%d-%m-%Y'),
             time    = strftime(date_time, format="%H:%M:%S"),
             match   = factor(match),
             league_gender = factor(league_gender),
             competition_stage = factor(competition_stage)) %>%
      select(league_gender:match_id, match_duration, gym,
             spectators, gym_capacity, date, time, weekday, match, win,
             team_name_1, team_name_2, team_id_1, team_id_2,
             match_points_team_1, match_points_team_2, 
             league_points_team_1, league_points_team_2,
             league_sets_team_1, league_sets_team_2,
             distance_team_1, distance_team_2,
             roster_size_team_1, blau_index, 
             herfindahl_herschman_index, mean_height)

## ------------------------------------------------------------------------
df_1 <- df

df_2 <- df %>%
        filter(match == "home")

# write.dta(df, "df_1.dta")
# write.dta(df, "df_2.dta")

