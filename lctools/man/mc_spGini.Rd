\name{mc.spGini}
\alias{mc.spGini}
\title{Monte Carlo simulation for the significance of the Spatial Gini coefficient}
\description{This function provides one approach for inference on the spatial Gini inequality measure. This is a small Monte Carlo simulation according to which: a) the data are spatially reallocated in a random way; b) the share of overall inequality that is associated with non-neighbour pairs of locations - SG (Eq. 5 in Rey & Smith, 2013) - is calculated for the original and simulated spatial data sets; c) a pseudo p-value is calculated as p=(1+C)/(1+M) where C is the number of the permutation data sets that generated SG values that were as extreme as the observed SG value for the original data (Eq. 6 in Rey & Smith, 2013). If p<=0.05 it can be argued that the component of the Gini for non-neighbour inequality is statistically significant. For this approach, a minimum of 19 simulations is required.}

\usage{
mc.spGini(Nsim=99,Bandwidth,x,Coord.X,Coord.Y,WType='Binary')
}

\arguments{
  \item{Nsim}{a positive integer that defines the number of the simulation's iterations}
  \item{Bandwidth}{a positive integer that defines the number of nearest neighbours for the calculation of the weights}
  \item{x}{a numeric vector of a variable}
  \item{Coord.X}{a numeric vector giving the X coordinates of the observations (data points or geometric centroids)}
  \item{Coord.Y}{a numeric vector giving the Y coordinates of the observations (data points or geometric centroids)}

  \item{WType}{string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary", "Bi-square", "RSBi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise;

  RSBi-square: weight = Bi-square weights / sum (Bi-square weights) for each row in the weights matrix}
}

\details{For 0.05 level of significance in social sciences, a minimum number of 19 simulations (Nsim>=19) is required. We recommend at least 99 and at best 999 iterations}

\value{Returns a list of the simulated values, the observed Gini and its spatial decomposition, the pseudo p-value of significance
  \item{SIM}{a dataframe with simulated values: SIM.ID is the simulation ID, SIM.gwGini is the simulated Gini of neighbours, SIM.nsGini is the simulated Gini of non-neighbours, SIM.SG is the simulated share of the overall Gini that is associated with non-neighbour pairs of locations, SIM.Extr = 1 if the simulated SG is greater than or equal to the observed SG}
  \item{spGini.Observed}{Observed Gini (Gini) and its spatial components (gwGini, nsGini)}
  \item{pseudo.p}{pseudo p-value: if this is lower than or equal to 0.05 it can be argued that the component of the Gini for non-neighbour inequality is statistically significant.}
}

\references{
Rey, S.J., Smith, R. J. (2013) A spatial decomposition of the Gini coefficient, Letters in Spatial and Resource Sciences, 6 (2), pp. 55-70.

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{Acknowledgement: I would like to thank LI Zai-jun, PhD student at Nanjing Normal University, China for encouraging me to develop this function and for testing this package.}

\examples{
data(GR.Municipalities)
Nsim=19
Bd1<-4
x1<-GR.Municipalities@data$Income01[1:45]
WType1<-'Binary'

SIM20<-mc.spGini(Nsim,Bd1,x1,GR.Municipalities@data$X[1:45], GR.Municipalities@data$Y[1:45],WType1)
SIM20

hist(SIM20$SIM$SIM.nsGini,col = "lightblue", main = "Observed and simulated nsGini",
xlab = "Simulated nsGini", ylab = "Frequency",xlim = c(min(SIM20$SIM$SIM.nsGini),
SIM20$spGini.Observed[[3]]))
abline(v=SIM20$spGini.Observed[[3]], col = 'red')
}
\keyword{Monte Carlo Simulation}
\keyword{Inference}
\keyword{Spatial Gini}
\keyword{Spatial Inequality}
\keyword{Spatial autocorrelation}