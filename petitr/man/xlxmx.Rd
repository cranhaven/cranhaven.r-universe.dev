\name{xlxmx}
\Rdversion{1.1}
\alias{xlxmx}
\title{
builds an average life table from a set of individuals
}
\description{
Calculates a life table with three columns, x, lx mx from the age specific birth data of a set of individuals. Called by petitr, but may be used as standalone function.
}
\usage{
xlxmx(X, s)
}
\arguments{
  \item{X}{
a data.frame. See petitr
} 
  \item{s}{
Sex ratio expressed as the proportion of females in the population
}
}
\value{
a data.frame with three columns, x (age), lx (survival at age x), mx (birth rate at age x)
}
\references{
Lotka 1924, Birch 1948
}
\author{
Jean-Sebastien Pierre jean-sebastien.pierre@univ-rennes1.fr
}

\seealso{
 \code{\link{r}}, and \code{\link{petitr}}, called by  \code{\link{ranova}}
}
\examples{
data(life)
tablif=xlxmx(life,s=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
