\name{football}
\alias{football}
\docType{data}
\title{  Football Point Spreads and Game Outcomes  }
\description{
  Data on football point spreads and game outcomes (north american football) for ten seasons, 
  1981, 1983-1986, 1988-1992, each season are 224 games and they are strung together. Only three
  first seasons are used in chapter one of book. 
}
\usage{data(football)}
\format{
  A data frame with 2240 observations on the following 7 variables.
  \describe{
    \item{\code{home}}{home indicator}
    \item{\code{favorite}}{favorite score}
    \item{\code{underdog}}{underdog score}
    \item{\code{spread}}{point spread}
    \item{\code{favorite.name}}{a factor with levels \code{ATL} \code{BUF} \code{CHI} 
                                     \code{CIN} 
                                     \code{CLE} \code{DAL} \code{DEN} \code{DET} \code{GB} 
                                     \code{HOU} \code{IND} \code{KC} \code{LAA} \code{LAN} 
                                     \code{MIA} \code{MIN} \code{NE} \code{NO} \code{NYG} 
                                     \code{NYJ} \code{PHA} \code{PHX} \code{PIT} \code{SD} 
                                     \code{SEA} 
                                     \code{SF} \code{TB} \code{WAS}}
    \item{\code{underdog.name}}{a factor with levels \code{ATL} \code{BUF} \code{CHI} 
                                     \code{CIN} \code{CLE} 
                       \code{DAL} \code{DEN} \code{DET} \code{GB} \code{HOU} \code{IND} 
                                       \code{KC} \code{LAA} 
                       \code{LAN} \code{MIA} \code{MIN} \code{NE} \code{NO} \code{NYG} 
                                         \code{NYJ} \code{PHA} 
                       \code{PHX} \code{PIT} \code{SD} \code{SEA} \code{SF} 
                                       \code{TB} \code{WAS}}
    \item{\code{week}}{a numeric vector}
  }
}
\details{
  Football experts provide the point spread as a measure of the
  difference in 
  ability between the two teams.  
  For example, team A might be a 3.5 favourite to team B.
  The implication of this is that the proposition
  that team A, the favourite, defeats team B, the underdog,
  by 4 or more points, are considered a fair bet. 
  In other words, the probability that A wins by more than 3.5 points is 0.5.  
  If the point spread are an integer, then the implication is that team
A 
is as likely to win by more 
points than the point spread as it is to win by fewer points
than the point spread (or to loose). 
  If the 
  win is by exactly the point spread then neither side is paid off.  
}
\examples{
data(football)
summary(football)
names(football)
# In chapter 1 only three first seasons are used:
cap1 <- football[1:672, ]

}
\keyword{datasets}
