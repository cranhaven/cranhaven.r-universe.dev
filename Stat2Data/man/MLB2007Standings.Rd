\name{MLB2007Standings}
\alias{MLB2007Standings}
\docType{data}
\title{Standings and Team Statistics from the 2007 Baseball Season}
\description{
Data for Major League Baseball teams from the 2007 regular season
}
%\useage{data(MLB2007Standings)}
\format{
  A dataset with 30 observations on the following 21 variables.
  \tabular{rl}{
    \code{Team} \tab {Name of the team}\cr
    \code{League} \tab {League: \code{AL} or \code{NL}}\cr
    \code{Wins} \tab {Number of wins for the season (out of 162 games)}\cr
    \code{Losses} \tab {Number of losses for the season}\cr
    \code{WinPct} \tab {Proportion of games won (Wins/162)}\cr
    \code{BattingAvg} \tab {Team batting average}\cr
    \code{Runs} \tab {Number of runs runs scored}\cr
    \code{Hits} \tab {Number of hits}\cr
    \code{HR} \tab {Number of home runs hit}\cr
    \code{Doubles} \tab {Number of doubles hit}\cr
    \code{Triples} \tab {Number of triple hit}\cr
    \code{RBI} \tab {Number of runs batted in}\cr
    \code{SB} \tab {Number of stolen bases}\cr
    \code{OBP} \tab {On base percentage}\cr
    \code{SLG} \tab {Slugging percentage}\cr
    \code{ERA} \tab {Earned run average (earned runs allowed per 9 innings)}\cr
    \code{HitsAllowed} \tab {Number of hits against the team}\cr
    \code{Walks} \tab {Number of walks allowed}\cr
    \code{StrikeOuts} \tab {Number of strikeouts (by the team's pitchers)}\cr
    \code{Saves} \tab {Number of games saved (by the team's pitchers)}\cr
    \code{WHIP} \tab {Number of walks and hits per inning pitched}\cr
  }
}
\details{
Data for all 30 Major League Baseball (MLB) teams for the 2007 regular season.
This includes team batting statistics (BattingAvg through SLG) and team pitching statistics
(ERA through WHIP)

Updated to MLBStandings2016 in second edition.
}
\source{
Data downloaded from baseball-reference.com:\cr
http://www.baseball-reference.com/leagues/MLB/2007-standings.shtml\cr
http://www.baseball-reference.com/leagues/MLB/2007.shtml
}

\keyword{datasets}
