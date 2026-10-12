\name{FinalFourIzzo}
\alias{FinalFourIzzo}
\docType{data}
\title{NCAA Final Four by Seed and Tom Izzo (through 2010)}
\description{
NCAA Final Four by seed with indicator for Tom Izzo's teams from 1985 - 2010.
}
\format{
  A dataset with 1664 observations on the following 4 variables.
  \tabular{rl}{
    \code{Year} \tab {Year (1985 - 2010)}\cr
    \code{Seed} \tab {Seed in NCAA men's basketball tournament: 1 to 16}\cr
    \code{Final4} \tab {\code{1}=made Final Four or \code{0}=did not make Final Four}\cr
    \code{Izzo} \tab {\code{1}=team coached by Tom Izzo or \code{0}=not an Izzo team}\cr
  }
}
\details{
Each year 64 college teams are selected for the NCAA Division I Men's
Basketball tournament, with 16 teams placed in each of four regions. Within each region the teams
are seeded from 1 to 16, with the (presumed) best team as the 1 seed and the (presumed) weakest
team as the 16 seed; this practice of seeding teams began in 1979 for the NCAA tournament. Only
one team from each region (so four teams each year) advances to the Final Four. This dataset is the same as FinalFourLong,
except the data starts in 1985 and
we have a extra column that is an indicator for Michigan State teams coached by Tom Izzo.

Updated to FinalFourIzzo17 in second edition.
}
\source{
Final Four teams and their seed can be found at\cr
http://www.championshiphistory.com/ncaahoops.php.
}

\keyword{datasets}
