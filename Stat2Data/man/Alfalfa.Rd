\name{Alfalfa}
\alias{Alfalfa}
\docType{data}
\title{
Alfalfa Growth
}
\description{
Growth of alfalfa sprouts in acidic conditions
}
\format{
  A dataset with 15 observations on the following 3 variables.
  \tabular{rl}{
    \code{Ht4} \tab {Height of alfalfa sprouts after four days}\cr
    \code{Acid} \tab {Amount of acid: \code{1.5HCl}, \code{3.0HCl}, or \code{water}}\cr
    \code{Row} \tab {\code{a} through \code{e} with \code{a}= closest to window and  \code{e}=farthest from window}\cr
  }
}
\details{
Some students were interested in how an acidic environment might affect
the growth of plants. They planted alfalfa seeds in 15 cups and randomly chose five to get plain
water, five to get a moderate amount of acid (1.5M HCl), and five to get a stronger acid solution
(3.0M HCl). The plants were grown in an indoor room so the students assumed that the distance
from the main source of daylight (windows) might have an affect on growth rates. For this reason,
they arranged the cups in five rows of three with one cup from each Acid level in each row. These
are labeled in the data set as Row: a=farthest from the window through e=nearest to the window.
}
\source{
Neumann, A., Richards, A. L., and Randa, J. (2001).
Effects of acid rain on alfalfa plants. Unpublished manuscript, Oberlin College.
}
\examples{
data(Alfalfa)
}

\keyword{datasets}
