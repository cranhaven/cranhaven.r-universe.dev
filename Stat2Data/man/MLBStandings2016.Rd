\name{MLBStandings2016}
\alias{MLBStandings2016}
\docType{data}
\title{MLB Standings in 2016}
\description{
Major League Baseball (MLB) standings and team statistics for the 2016 season
}
\format{
  A data frame with 30 observations on the following 21 variables.
  \describe{
    \item{\code{Team}}{Team name}
    \item{\code{League}}{\code{AL}=American or \code{NL}=National}
    \item{\code{Wins}}{Number of wins for the season (out of 162 games)}
    \item{\code{Losses}}{Number of losses for the season}
    \item{\code{WinPct}}{Proportion of games won}
    \item{\code{BattingAverage}}{Team batting average}
    \item{\code{Runs}}{Number of runs scored}
    \item{\code{Hits}}{Number of hits}
    \item{\code{HR}}{Number of home runs hit}
    \item{\code{Doubles}}{Number of doubles hit}
    \item{\code{Triples}}{Number of triples hit}
    \item{\code{RBI}}{Number of runs batted in}
    \item{\code{SB}}{Number of stolen bases}
    \item{\code{OBP}}{On base percentage}
    \item{\code{SLG}}{Slugging percentage}
    \item{\code{ERA}}{Earned run average (earned runs allowed per 9 innings)}
    \item{\code{HitsAllowed}}{Number of hits against the team}
    \item{\code{Walks}}{Number of walks allowed}
    \item{\code{StrikeOuts}}{Number of strikeouts (by the team's pitchers)}
    \item{\code{Saves}}{Number of games saved (by the team's pitchers)}
    \item{\code{WHIP}}{Number of walks and hits per inning pitched}
  }
}
\details{
Data for all 30 Major League Baseball (MLB) teams for the 2016 regular season. This includes team batting statistics (BattingAvg through SLG) and team pitching statistics (ERA through WHIP)
}
\source{
Data downloaded from baseball-reference.com: \cr
http://www.baseball-reference.com/leagues/MLB/2016-standings.shtml \cr
http://www.baseball-reference.com/leagues/MLB/2016.shtml
}
\keyword{datasets}
