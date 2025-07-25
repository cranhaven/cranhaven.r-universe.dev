# cbbreadr 1.0.2

* Typo fix.

# cbbreadr 1.0.1

* Minor documentation fixes for CRAN.

# cbbreadr 1.0.0

* Initial CRAN submission.
* Added a series of low-level `load_*` functions that allow R users to access pre-compiled data from the CollegeBasketballData.com API. These loaders include:
    * `load_conferences()` - conference data
    * `load_games()` - games
    * `load_lines()` - betting markets
    * `load_media()` - broadcasting information
    * `load_player_box_scores()` - player-game-level box score data
    * `load_player_stats()` - player-season-level box score data
    * `load_plays()` - all tracked plays in specified season(s)
    * `load_rankings()` - AP and Coaches poll results
    * `load_recruiting()` - 247 recruiting data
    * `load_rosters()` - team rosters for specified season(s)
    * `load_team_box_scores()` - team-game-level box score data
    * `load_team_stats()` - team-season-level box score data
    * `load_teams()` - team data
    * `load_venues()` - venues and arenas
* Added the helper function `most_recent_season()` which returns either the most recent or current NCAA season.
* Added full documentation for all functions
* Added data dictionaries for all available resources    
