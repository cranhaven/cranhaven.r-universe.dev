\name{adequadExp}
\alias{adequadExp}
\title{ test the adequation of a random variable to the exponential distribution  }
\description{
	In order to test the adequation of a random variable, this function plot the
	probabilit plot with reference distribution exponential of the variable and calculates
	the Kolmogorov Smirnov test. 
}
\usage{
adequadExp(T, toplot = FALSE)
}
\arguments{
  \item{T}{ the random variable on which we want to test the adequation }
  \item{toplot}{ a logical to plot the probability graph  }
}
\value{
  a list of the following components
  \item{LinearRegression }{ the result of the linear regression of the ordinate on the abcisse of the probability plot}
  \item{KolmogorvSmirnovTest }{the result of the Kolmogorvo Smirnoc test}
}
\author{ Christophe Dutang and Julie Barthes }
\examples{
\dontrun{
T <- c(12.9622796,  1.4146460,  1.3146761, 14.9147353,  7.5131105,  
8.5130874, 6.5943351, 10.6954653, 14.1000977, 12.4673316,  2.7185478,  
9.6297777, 10.0930441,  0.6270543, 26.7937074,  7.6082447)
adequadExp(T,TRUE)
}
}
\keyword{ htest }
