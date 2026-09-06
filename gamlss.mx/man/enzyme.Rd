\name{enzyme}
\alias{enzyme}
\alias{brains}
\docType{data}

\title{Data used in gamlss.mx}
\description{
  enzyme : The data comprise independent measurement of enzyme activity in the
blood of 245 individuals. The data were analysed by Bechker \emph{at al.} (1993).

brains : the brain size, \code{brain}, and body weight, \code{body},
for 28 differnt animals.
  
}
\usage{
data(enzyme)
data(brains)
}
\format{
  enzyme :  data frame with 245 observations on the following variable \code{act}.
  
  brains : data frame with 28  observations on the following variables. \code{body}, \code{brain}
  
  \describe{
    \item{\code{act}}{a numeric vector showing enzyme activity in the
blood of 245 individuals.}

 \item{\code{body}}{a numeric vector showing the body weight of 28 differnt animals}
  \item{\code{brain}}{a numeric vector showing the brain size  of 28 differnt animals}

  }
}

\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos M.D., Kneib T, Klein N, Mayr A, Heller GZ. (2024) Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications. Cambridge University Press.

(see also \url{https://www.gamlss.com/}).

}

\examples{
data(enzyme)
hist(enzyme$act)
data(brains)
brains$lbrain<-log(brains$brain)
brains$lbody<-log(brains$body)
with(brains, plot(lbrain~lbody))
}
\keyword{datasets}
