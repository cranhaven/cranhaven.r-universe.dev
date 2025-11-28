\name{moransI}
\alias{moransI}
\title{Moran's I classic statistic for assessing spatial autocorrelation}
\description{Moran's I is one of the oldest statistics used to examine spatial autocorrelation. This global statistic was first proposed by Moran (1948, 1950). Later, Cliff and Ord (1973, 1981) present a comprehensive work on spatial autocorrelation and suggested a formula to calculate the I which is now used in most textbooks and software: \deqn{I = (n/W)*(\Sigma \Sigma w_{ij}*z_i*z_j/ \Sigma z_i^2)} where \eqn{n} is number of observations, \eqn{W} is the sum of the weights w_ij for all pairs in the system, \eqn{z_i=x_i - mean(x)} where \eqn{x} is the value of the variable at location \eqn{i} and \eqn{mean(x)} the mean value of the variable in question (Eq. 5.2 Kalogirou, 2003).The implementation here allows only nearest neighbour weighting schemes. Resampling and randomization null hypotheses have been tested following the discussion of Goodchild (1986, pp. 24-26).}

\usage{moransI(Coords, Bandwidth, x, WType = 'Binary')}

\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidth}{a positive integer that defines the number of nearest neighbours for the calculation of the weights}
  \item{x}{a numeric vector of a variable}
  \item{WType}{a string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary" and "Bi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: \eqn{weight = (1-(ndist/H)^2)^2} for distances less than or equal to H, 0 otherwise.}
}

\details{The Moran's I statistic ranges from -1 to 1. Values in the interval (-1, 0) indicate negative spatial autocorrelation (low values tend to have neighbours with high values and vice versa), values near 0 indicate no spatial autocorrelation (no spatial pattern - random spatial distribution) and values in the interval (0,1) indicate positive spatial autocorrelation (spatial clusters of similarly low or high values between neighbour municipalities should be expected.)
}

\value{Returns the weights matrix, the calculated Moran's I and a list of statistics for the latter's inference: the expected I (E[I]), z scores and p values for both resampling and randomization null hypotheses.

  \item{W}{Weights Matrix}
  \item{Morans.I}{Classic global Moran's I statistic}
  \item{Expected.I}{The Expected Moran's I \eqn{(E[I]=-1/(n-1))}}
  \item{z.resampling}{The z score calculated for the resampling null hypotheses test}
  \item{z.randomization}{The z score calculated for the randomization null hypotheses test}
  \item{p.value.resampling}{The p-value (two-tailed) calculated for the resampling null hypotheses test}
  \item{p.value.randomization}{The p-value (two-tailed) calculated for the randomization null hypotheses test}

}

\references{
Anselin, L., I. Syabri and Y Kho., 2006, GeoDa: An Introduction to Spatial Data Analysis. Geographical Analysis 38(1), 5-22.

Bivand et al., 2014, spdep: Spatial dependence: weighting schemes, statistics and models, http://cran.r-project.org/web/packages/spdep/index.html

Cliff, A.D., and Ord, J.K., 1973, Spatial autocorrelation (London: Pion).

Cliff, A.D., and Ord, J.K., 1981, Spatial processes: models and applications (London: Pion).

Goodchild, M. F., 1986, Spatial Autocorrelation. Catmog 47, Geo Books.

Moran, P.A.P., 1948, The interpretation of statistical maps, Journal of the Royal Statistics Society, Series B (Methodological), 10, 2, pp. 243 - 251.

Moran, P.A.P., 1950, Notes on continuous stochastic phenomena, Biometrika, 37, pp. 17 - 23.

Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}

Paradis et al., 2014, ape: Analyses of Phylogenetics and Evolution, \url{https://CRAN.R-project.org/package=ape}
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{This function has been compared to the function Moran.I within the file MoranI.R of package ape version 3.1-4 (Paradis et al., 2014). This function results in the same Moran's I statistic as the one in package ape. The statistical inference in the latter refers to the randomization null hypotheses test discussed above. It is necessary to acknowledge that the code of this function has been assisted by the one in ape package: this is the calculation of statistics S1 and S2 (lines 67 and 69 of the source code) in this function. Another R package with functions for calculating and testing the Moran's I statistic and its significance is the spdep package (Bivand et al. 2014). The Moran's I statistic calculated using this function is not the same as the one in OpenGeoDa (Anselin et al., 2006). The latter is another very popular software for calculating spatial autocorrelation statistics.}

\examples{
data(GR.Municipalities)
attr<-GR.Municipalities@data
m.I<-moransI(cbind(attr$X, attr$Y),6,attr$UnemrT01)

t(as.matrix(m.I[2:7]))

}
\keyword{Moran's I}
\keyword{Moran's I significance test}
\keyword{Spatial autocorrelation}
