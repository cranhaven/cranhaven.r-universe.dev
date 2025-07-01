#' volleystat: Volleyball match statistics
#'
#' This packages provides data on match statistics of the German volleyball first division league
#' (seasons 2013/2014 to 2018/2019). The data has been collected from the official volleyball first division homepage
#' (www.volleyball-bundesliga.de) and contains information on teams, staff, sets, matches, and player-in-match
#' statistics (extracted automatically using tabulizer from the PDFs of the official match reports).
#'
#' @section players:
#' A dataset containing attributes of all players
#' in the German first division volleyball league for each season
#' starting in 2013/2014 and ending in 2018/2019.
#'
#' @section staff:
#' A dataset containing attributes of all staff members of teams
#' in the German first division volleyball league for each season
#' starting in 2013/2014 and ending in 2018/2019.
#'
#' @section matches:
#' A dataset containing all matches of the German first division volleyball league for each season
#' starting in 2013/2014 and ending in 2018/2019. Note that all matches are included twice in the
#' dataset, i.e., from the perspective of the home team and from the perspective of the away team.
#'
#' @section sets:
#' A dataset containing all matches of the German first division volleyball league for each season on set level.
#' starting in 2013/2014 and ending in 2018/2019. Note that all sets are included twice in the
#' dataset, i.e., from the perspective of the home team and from the perspective of the away team.
#'
#' @section matchstats:
#' A dataset containing match-player level statistics of the German first division volleyball league for each match and
#' each season starting 2013/2014 and ending 2018/2019.
#'
#' @section match_adresses:
#' A dataset containing adresses and coordinates of all matches of the German first division volleyball league for each match and
#' each season starting 2013/2014 and ending 2018/2019.
#'
#' @section team_adresses:
#' A dataset containing adresses and coordinates of all teams of the German first division volleyball league for each match and
#' each season starting 2013/2014 and ending 2018/2019.
#'
#' @docType package
#' @name volleystat
NULL
