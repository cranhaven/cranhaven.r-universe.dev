
# cbbreadr <a href='https://github.com/john-b-edwards/cbbreadr'><img src='https://github.com/john-b-edwards/cbbd-data/blob/master/data/outputs/cbbreadr_hex.png?raw=true' align="right" width="15%" min-width="120px" /></a>

<!-- badges: start -->
<!-- badges: end -->

Low level loader for reading in college basketball data in bulk.

## Installation

You can install the stable version of `{cbbreadr}` from CRAN with:

```r
install.packages("cbbreadr")
```

You can install the development version of cbbreadr from github using `{devtools}`:

```r
devtools::install_github("https://github.com/john-b-edwards/cbbreadr")
```

## Examples

Most functions take in a `seasons` argument for seasons to query. Data is typically available between 2003 and the present season, but an error is thrown if data is not available for a specified season. Calling functions as-is will load data from the most recent (or current) college basketball season.

```r
library(cbbreadr)
# with no argument, returns most recent season
load_games()
# specify a single season
load_games(2023)
# specify a range of seasons
load_games(2010:2015)
```

To return all available seasons for a given resource, just pass `TRUE` as an argument to the function.

```r
# return all available seasons
load_games(TRUE)
```

Some functions load resources that do not to be specified by year, and thus do not take an argument.
```r
# does not take any arguments
load_conferences()
```

## Acknowledgements

This package is only possible thanks to the [CollegeBasketballData.com](https://collegebasketballdata.com/) API. The API is maintained by Bill Radjewski, who is kind enough to make this data available for free. 

The API does have premium tiers but maintaining this package requires only the free tier, so no money is needed for upkeep for the package. If you would like to support this package, rather than send any money my way, I encourage you to instead donate or subscribe to [Bill's Patreon](https://www.patreon.com/c/collegefootballdata/posts) so that he can continue developing and maintaining this wonderful project.

I have been a developer with the nflverse organization for a few years now, and have learned much from my talented teammates (who themselves have contributed to `{cbbreadr}`'s development, either directly or indirectly). The structure and code of this project looks quite similar to our NFL pipelines for good reason, and this project would not have been possible without their expertise. My thanks goes out to the entire `nflverse` organization.

## Automation Status

`{cbbreadr}` relies on several automated pipelines from GitHub to keep data resources updated. For largely static data objects (such as `load_conferences()`), these pipelines will run once a year. For more frequently updated data objects (such as `load_plays()`), these pipelines run daily during the college basketball season. To view when a data resource was most recently updated, see below.

| Data | Status | Last Updated |
|:--:|:--:|:--:|
|Conferences|[![update-conferences](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_conferences.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_conferences.yaml)| [![conferences](https://img.shields.io/badge/dynamic/json?color=blue&label=load_conferences&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/conferences/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/conferences)
|Games|[![update-games](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_games.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_games.yaml)| [![games](https://img.shields.io/badge/dynamic/json?color=blue&label=load_games&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/games/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/games)
|Lines|[![update-lines](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_lines.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_lines.yaml)| [![lines](https://img.shields.io/badge/dynamic/json?color=blue&label=load_lines&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/lines/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/lines)
|Media|[![update-media](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_media.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_media.yaml)| [![media](https://img.shields.io/badge/dynamic/json?color=blue&label=load_media&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/media/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/media)
|Player Box Scores|[![update-player-box](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_player_box.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_player_box.yaml)| [![player-box](https://img.shields.io/badge/dynamic/json?color=blue&label=load_player_box_scores&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/player_box_scores/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/player_box_scores)
|Player Stats|[![update-player-stats](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_player_stats.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_player_stats.yaml)| [![player-stats](https://img.shields.io/badge/dynamic/json?color=blue&label=load_player_stats&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/player_stats/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/player_stats)
|Plays|[![update-plays](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_plays.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_plays.yaml)| [![plays](https://img.shields.io/badge/dynamic/json?color=blue&label=load_plays&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/plays/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/plays)
|Rankings|[![update-rankings](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_rankings.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_rankings.yaml)| [![rankings](https://img.shields.io/badge/dynamic/json?color=blue&label=load_rankings&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/rankings/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/rankings)
|Recruiting|[![update-recruiting](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_recruiting.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_recruiting.yaml)| [![recruiting](https://img.shields.io/badge/dynamic/json?color=blue&label=load_recruiting&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/recruiting/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/recruiting)
|Rosters|[![update-rosters](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_rosters.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_rosters.yaml)| [![rosters](https://img.shields.io/badge/dynamic/json?color=blue&label=load_rosters&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/rosters/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/rosters)
|Team Box Scores|[![update-team-box](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_team_box.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_team_box.yaml)| [![team-box](https://img.shields.io/badge/dynamic/json?color=blue&label=load_team_box_scores&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/team_box_scores/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/team_box_scores)
|Team Stats|[![update-team-stats](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_team_stats.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_team_stats.yaml)| [![team-stats](https://img.shields.io/badge/dynamic/json?color=blue&label=load_team_stats&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/team_stats/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/team_stats)
|Teams|[![update-teams](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_teams.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_teams.yaml)| [![teams](https://img.shields.io/badge/dynamic/json?color=blue&label=load_teams&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/teams/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/teams)
|Venues|[![update-venues](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_venues.yaml/badge.svg)](https://github.com/john-b-edwards/cbbd-data/actions/workflows/update_venues.yaml)| [![venues](https://img.shields.io/badge/dynamic/json?color=blue&label=load_venues&query=last_updated&style=flat-square&url=https://github.com/john-b-edwards/cbbd-data/releases/download/venues/timestamp.json)](https://github.com/john-b-edwards/cbbd-data/releases/tag/venues)
