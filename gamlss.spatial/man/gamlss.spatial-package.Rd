\name{gamlss.spatial-package}
\alias{gamlss.spatial-package}
\alias{gamlss.spatial}
\docType{package}
\title{
\packageTitle{gamlss.spatial}
}
\description{
\packageDescription{gamlss.spatial}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{gamlss.spatial}
\packageIndices{gamlss.spatial}
}
\author{
\packageAuthor{gamlss.spatial}

Maintainer: \packageMaintainer{gamlss.spatial}
}
\references{
De Bastiani, F. Rigby, R. A., Stasinopoulos, D. M.,  Cysneiros, A. H. M. A. and Uribe-Opazo, M. A. (2016) Gaussian Markov random spatial models in GAMLSS. \emph{Journal of Applied Statistics}, pp 1-19.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.


Rue and Held (2005) \emph{Gaussian markov random fields: theory and applications}, Chapman  & Hall, USA. 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

(see also \url{https://www.gamlss.com/}).
}
\keyword{ package }
\examples{
library(mgcv)
data(columb)
data(columb.polys)
m1 <- MRFA(columb$crime, columb$district, polys=columb.polys)
draw.polys(columb.polys, m1) 
}
