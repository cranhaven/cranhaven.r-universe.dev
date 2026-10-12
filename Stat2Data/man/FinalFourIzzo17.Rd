\name{FinalFourIzzo17}
\alias{FinalFourIzzo17}
\docType{data}
\title{NCAA Final Four by Seed and Tom Izzo (through 2017)}
\description{
NCAA Final Four by seed with indicator for Tom Izzo's teams for 1985 - 2017
}
\format{
  A data frame with 2112 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{Year 1985 - 2017}
    \item{\code{Seed}}{Seed in NCAA men's basketball tournament: 1 to 16}
    \item{\code{Final4}}{\code{1}=made Final Four or \code{0}=did not make Final Four}
    \item{\code{Izzo}}{\code{1}=team coached by Tom Izzo or \code{0}=not an Izzo team}
  }
}
\details{
Each year 64 college teams are selected for the NCAA Division I Men's
Basketball tournament, with 16 teams placed in each of four regions. Within each region the teams are seeded from 1 to 16, with the (presumed) best team as the 1 seed and the (presumed) weakest team as the 16 seed; this practice of seeding teams began in 1979 for the NCAA tournament. Only one team from each region (so four teams each year) advances to the Final Four. This dataset is an extention of FinalFourIzzo (that ended in 2017) and the same as FinalFourLong2017, except the data starts in 1985 and we have an extra column that is an indicator for Michigan State teams coached by Tom Izzo.

}
\source{
Final Four teams and their seed can be found at http://www.championshiphistory.com/ncaahoops.php
}
\keyword{datasets}
