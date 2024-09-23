\name{leukDat}
\docType{data}
\alias{leukDat}
\alias{GDRmap}
\title{Childhood leukemia in the former GDR}
\description{
In this example we present data from the former East Germany within the time
period from 1980 to 1989. The data are taken from the cancer atlas of the former
East Germany (Moehner et al. 1994). The data provided  the regional distribution of childhood leukemia in the former East Germany.
}
\usage{data("leukDat")}

\examples{
## disease mapping example
data(leukDat)
data(GDRmap) # this is the map of the former GDR
mix.leuk <- mixalg(obs = "oleuk", pop.at.risk = "eleuk",
                   data = leukDat, family = "poisson")
mix.leuk
plot(GDRmap, col = mix.leuk@classification)          
}

\references{
Schlattmann, P.(2009) \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
} 
\keyword{datasets}
