\name{Velib}
\alias{Velib}
\docType{data}
\title{
The Velib data set.
}
\description{
The Velib data set contains data from the bike sharing system of Paris, called Velib. The data are loading profiles of the bike stations over seven days. The data were collected every hour during the period Sunday 1st Sept. - Sunday 7th Sept., 2014.
}
\usage{data("Velib")}
\format{
  The format is:
- data: the loading profiles (nb of available bikes / nb of bike docks) of the 1189 stations for 7 days every hour.
- position: the longitude and latitude of the 1189 bike stations.
}

\source{
The real time data are available at https://developer.jcdecaux.com/ (with an api key).
}
\references{
The data were first used in C. Bouveyron, E. Come and J. Jacques, The discriminative functional mixture model for a comparative analysis of bike sharing systems, The Annals of Applied Statistics, vol. 9 (4), pp. 1726-1760, 2015 (http://dx.doi.org/10.1214/15-AOAS861).
}
\examples{
data(Velib)

\donttest{
set.seed(12345)
# Co-clustering with funLBM
out = funLBM(Velib$data,K=4,L=2,basis.name="fourier",nbasis=5)

# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')
}
}
\keyword{datasets}
