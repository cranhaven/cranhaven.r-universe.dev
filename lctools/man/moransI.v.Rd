\name{moransI.v}
\alias{moransI.v}
\title{Computes a vector of Moran's I statistics.}
\description{Moran's I is one of the oldest statistics used to examine spatial autocorrelation. This global statistic was first proposed by Moran (1948, 1950). Later, Cliff and Ord (1973, 1981) present a comprehensive work on spatial autocorrelation and suggested a formula to calculate the I which is now used in most textbooks and software: \deqn{I = (n/W)*(\Sigma \Sigma w_{ij}*z_i*z_j/ \Sigma z_i^2)} where n is number of observations, W is the sum of the weights w_ij for all pairs in the system, \eqn{z_i=x_i - mean(x)} where x is the value of the variable at location i and mean(x) the mean value of the variable in question (Eq. 5.2 Kalogirou, 2003).

This function allows the computation of an number of Moran's I statistics of the same family (fixed or adaptive) with different kernel size. To achieve this it first computes the weights matrix using the w.matrix function and then computes the Moran's I using the moransI.w function for each kernel. The function returns a table with the results and a simple scatter plot with the Moran's I and the kernel size. The latter can be disabled by the user.}

\usage{
moransI.v(Coords, Bandwidths, x, WType='Binary', family='adaptive', plot = TRUE)
}

\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidths}{a vector of positive integers that defines the number of nearest neighbours for the calculation of the weights or a vector of Bandwidths relevant to the coordinate systems the spatial analysis refers to.}
  \item{x}{a numeric vector of a variable}
  \item{WType}{a string giving the weighting function used to compute the weights matrix. 
  Options are: "Binary", "Bi-square", and "RSBi-square". The default value is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise;

  RSBi-square: weight = Bi-square weights / sum (Bi-square weights) for each row in the weights matrix
  }
  \item{family}{a string giving the weighting scheme used to compute the weights matrix. 
  Options are: "adaptive" and "fixed". The default value is "adaptive".
  
  adaptive: the number of nearest neighbours (integer). 
  
  fixed: a fixed distance around each observation's location (in meters).
  }
  \item{plot}{a logical value (TRUE/FALSE) denoting whether a scatter plot with the Moran's I and the kernel size will be created (if TRUE) or not.}
}
\details{The Moran's I statistic ranges from -1 to 1. Values in the interval (-1, 0) indicate negative spatial autocorrelation (low values tend to have neighbours with high values and vice versa), values near 0 indicate no spatial autocorrelation (no spatial pattern - random spatial distribution) and values in the interval (0,1) indicate positive spatial autocorrelation (spatial clusters of similarly low or high values between neighbour municipalities should be expected.)
}
\value{Returns a matrix with 8 columns and plots a scatter plot. These columns present the following statistics for each kernel size:
  \item{ID}{an integer in the sequence 1:m, where m is the number of kernel sizes in the vector Bandwidths}
  \item{k}{the kernel size (number of neighbours or distance)}
  \item{Moran's I}{Classic global Moran's I statistic}
  \item{Expected I}{The Expected Moran's I (E[I]=-1/(n-1))}
  \item{Z resampling}{The z score calculated for the resampling null hypotheses test}
  \item{P-value resampling}{The p-value (two-tailed) calculated for the resampling null hypotheses test}
  \item{Z randomization}{The z score calculated for the randomization null hypotheses test}
  \item{P-value randomization}{The p-value (two-tailed) calculated for the randomization null hypotheses test}
}

\references{
Cliff, A.D., and Ord, J.K., 1973, Spatial autocorrelation (London: Pion).

Cliff, A.D., and Ord, J.K., 1981, Spatial processes: models and applications (London: Pion).

Goodchild, M. F., 1986, Spatial Autocorrelation. Catmog 47, Geo Books.

Moran, P.A.P., 1948, The interpretation of statistical maps, Journal of the Royal Statistics Society, Series B (Methodological), 10, 2, pp. 243 - 251.

Moran, P.A.P., 1950, Notes on continuous stochastic phenomena, Biometrika, 37, pp. 17 - 23.

Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\seealso{
\code{\link{moransI.w}}, \code{\link{w.matrix}}
}

\examples{
data(GR.Municipalities)
Coords<-cbind(GR.Municipalities@data$X, GR.Municipalities@data$Y)

#using an adaptive kernel
bws <- c(3, 4, 6, 9, 12, 18, 24)
moransI.v(Coords, bws, GR.Municipalities@data$Income01)
}

\keyword{Moran's I}
\keyword{Moran's I significance test}
\keyword{Spatial autocorrelation}