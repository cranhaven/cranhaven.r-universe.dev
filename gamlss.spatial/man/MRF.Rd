\name{MRF}
\alias{MRF}
\alias{MRFA}

 
\title{
Markov Random Fields Fitting Functions
}
\description{
The functions \code{MRF()} and  \code{MRFA()} fit a Gaussian Markov Random Fields (MRF) model. They are used by the functions \code{mrf()} and  \code{mrfa()} respectively to fit a MRF additive term within GAMLSS 
}
\usage{
MRF(y, x, precision = NULL, neighbour = NULL, polys = NULL, 
            area = NULL, weights = rep(1, length(y)), sig2e = 1, 
            sig2b =             1, sig2e.fix = FALSE, 
            sig2b.fix = FALSE, penalty = FALSE, 
            delta = c(0.01, 0.01), shift = c(0, 0))

MRFA(y, x, precision = NULL, neighbour = NULL, polys = NULL, 
           area = NULL, weights = rep(1, length(y)), 
           lambda = NULL, df = NULL, start = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
response variable
}
  \item{x}{
a factor containing the areas
}
  \item{precision}{the precision matrix if set}
 \item{neighbour}{an object containing the neighbour information for the area if set}
  \item{polys}{the polygon information if set}
  \item{area}{this argument is here to allow more areas than the levels of the factor \code{x}, see example below.}
  \item{weights}{prior weights}
  \item{sig2e}{starting values for the error variance}
  \item{sig2b}{starting values for the random field variance}
  \item{sig2e.fix}{whether sig2e is fixed in the fitting, default equals FALSE}
  \item{sig2b.fix}{whether sig2B is fixed in the fitting, default equals FALSE}
  \item{penalty}{whether quadratic penalty is required to help convergence in for flat likelihoods, this is equivalent of putting a normal prior distribution for the log-sigmas e.g. \code{logsig2e-N(shift, 1/delta)}}
  \item{delta}{the precision of the prior}
  \item{shift}{the mean of the prior}
  \item{lambda}{smoothing parameter for \code{MRFA} function}
  \item{start}{starting value for the smoothing parameter \code{lambda} for \code{MRFA} function}

 \item{df}{for fixing the degrees of freedom (only in \code{MRFA()})}
}
\details{
There are two functions for fitting Markov random fields: i) \code{MRF())}  which uses the Q-function (marginal likelihood) for estimating the \code{sig2e} and \code{sig2b} parameters and ii) \code{MRFA()} which estimates the smoothing parameter \code{lambda=sig2e/sig2b} using the "alternating" method.
}
\value{
a fitted MRF object
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

\author{
Fernanda De Bastiani, Mikis Stasinopoulos, Robert Rigby and Vlasios Voudouris.

Maintainer: Fernanda <fernandadebastiani@gmail.com>
}

\seealso{
\code{\link{mrf}}
}

\examples{
library(mgcv)
data(columb)
data(columb.polys)
vizinhos=polys2nb(columb.polys)
precisionC <- nb2prec(vizinhos,x=columb$district)
# MRFA
 m1<-MRFA(columb$crime, columb$district, polys=columb.polys)
m11<-MRFA(columb$crime, columb$district, precision=precisionC)
m12<-MRFA(columb$crime, columb$district,  neighbour=vizinhos)
draw.polys(columb.polys, m12, scheme="heat",swapcolors=TRUE)
\dontrun{
# MRF 
  m2<-MRF(columb$crime, columb$district, polys=columb.polys)
 m21<-MRF(columb$crime, columb$district, precision=precisionC)
 m22<-MRF(columb$crime, columb$district, neighbour=vizinhos)
AIC(m1, m11,m12,m2, m21, m22, k=0)
draw.polys(columb.polys, m12, scheme="heat",swapcolors=TRUE)
# removing one area
columb2 <- columb[-5,]
# creating new precision matrix
precisionC2 <- nb2prec(vizinhos,x=columb$district,area=columb$district)
# MRFA 
# new data but declaring  area
m11<-MRFA(columb2$crime, columb2$district, polys=columb.polys, area=columb$district)
# new data old polys
m112<-MRFA(columb2$crime, columb2$district, polys=columb.polys)
# new data old precision old area
m111<-MRFA(columb2$crime, columb2$district, precision=precisionC,area=columb$district)
# new data old neighbour old area
m121<-MRFA(columb2$crime, columb2$district,  neighbour=vizinhos,area=columb$district)
# new data new precision old area
m113<-MRFA(columb2$crime, columb2$district, precision=precisionC2,area=columb$district)
AIC(m11,m112,m111,m121,m113, k=0)
m11<-MRFA(columb2$crime, columb2$district, polys=columb.polys, area=columb$district)
# new data old polys
m112<-MRFA(columb2$crime, columb2$district, polys=columb.polys)
# new data old precision old area
m111<-MRFA(columb2$crime, columb2$district, precision=precisionC,area=columb$district)
# new data old neighbour old area
m121<-MRFA(columb2$crime, columb2$district,  neighbour=vizinhos,area=columb$district)
# new data new precision old area
m113<-MRFA(columb2$crime, columb2$district, precision=precisionC2,area=columb$district)
AIC(m11,m112,m111,m121,m113, k=0)
draw.polys(columb.polys, fitted(m11))
}
}

\keyword{regression}
