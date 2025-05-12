\name{gamlss.gmrf}
\alias{gamlss.gmrf}
\alias{gmrf}

\title{Gaussian Markov Random Field fitting within GAMLSS 
}
\description{
The function \code{gmrf()} can be used to fit Markov Random Field additive terms within GAMLSS. 
}
\usage{
gamlss.gmrf(x, y, w, xeval = NULL, ...)
gmrf(x, precision = NULL, neighbour = NULL, polys = NULL,
                 area = NULL, adj.weight = 1000, df = NULL, lambda =
                 NULL, start = 10, method = c("Q", "A"), control =
                 gmrf.control(...), ...)
}

  
\arguments{
  \item{x}{a factor containing the areas}
  \item{precision}{the precision matrix if set}
 \item{neighbour}{an object containing the neighbour information for the area if set}
  \item{polys}{the polygon information if set}
  \item{area}{this argument is here to allow more areas than the levels of the factor \code{x}, see example below} 
  \item{adj.weight}{a value to adjust the iterative weight if necessary}
  \item{df}{degrees of freedom for fitting if required, only for \code{method="A"}}
   \item{lambda}{The smoothing parameter \code{lambda} if known, only for \code{method="A"}}
  \item{start}{starting value for the smoothing parameter \code{lambda}}
  \item{method}{"Q" for Q-function, or "A" for alternating method}
  \item{y}{working response variable}
  \item{w}{iterative weights}
  \item{xeval}{whether to predict or not}
   \item{control}{to be use for some of the argument of \code{MRF()}.}
  \item{\dots}{for extra arguments}
}
\details{
The function \code{gmrf()} is to support  the function \code{MRF()} and \code{MRFA()} within GAMLSS. 
It is intended to be called within a GAMLSS formula. The function \code{gmrf()} is not intended to be used directly. It is calling the  function \code{MRFA()} and \code{MRF()} within the GAMLSS fitting algorithm.
The results using the option \code{method="Q"} or \code{method="A"} should produce identical results.
}
\value{
a fitted gamlss object
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
\author{Fernanda De Bastiani, Mikis Stasinopoulos, Robert Rigby and Vlasios Voudouris. 

Maintainer: Fernanda <fernandadebastiani@gmail.com>
}


\seealso{
 \code{\link{MRF}}, \code{\link{MRFA}}
}
\examples{
library(gamlss)
library(mgcv)
data(columb)
data(columb.polys)
vizinhos=polys2nb(columb.polys)
precisionC <- nb2prec(vizinhos,x=columb$district)
# MRFA
 m1<- gamlss(crime~ gmrf(district, polys=columb.polys, method="Q"), data=columb)
 m2<- gamlss(crime~ gmrf(district, polys=columb.polys, method="A"), data=columb)
AIC(m1,m2, k=0)
draw.polys(columb.polys, getSmo(m2), scheme="topo")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}
\keyword{spatial}% __ONLY ONE__ keyword per line
