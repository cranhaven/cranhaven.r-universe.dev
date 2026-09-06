\name{gamlss.mx-package}
\alias{gamlss.mx-package}
\alias{gamlss.mx}
\docType{package}
\title{The GAMLSS add on package for mixture distributions}
\description{
The main purpose of this package is to allow the user of the GAMLSS models to fit mixture distributions.
}
\details{
\tabular{ll}{
Package: \tab gamlss.mx\cr
Type: \tab Package\cr
Version: \tab 0.0\cr
Date: \tab 2005-08-3\cr
License: \tab GPL (version 2 or later) \cr
}
This package has two main function the \code{gamlssMX()} which is loosely based on the
package \code{flexmix} of R and the function \code{gamlssNP()}  which is based on the \code{npmlreg} 
package of  Jochen Einbeck, Ross Darnell and John Hinde (2006) which in turns
is based on several GLIM4 macros originally written  by  Murray Aitkin and Brian 
Francis. It also contains the function 
\code{gqz()} which is written by  Nick Sofroniou and the function \code{gauss.quad()} 
written by Gordon Smyth.
}


\author{
Mikis Stasinopoulos <\email{d.stasinopoulos@londonmet.ac.uk}> and Bob Rigby <\email{r.rigby@londonmet.ac.uk}> 

Maintainer: Mikis Stasinopoulos <mikis.stasinopoulos@gamlss.org>

}
\references{

Jochen Einbeck, Ross Darnell and John Hinde (2006) npmlreg: Nonparametric maximum likelihood estimation for random effect
models, R package version 0.34

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
\keyword{ package }
\keyword{ distribution }
\keyword{ regression }

\seealso{\code{\link[gamlss]{gamlss}},\code{\link[gamlss.dist]{gamlss.family}}  
}
\examples{
data(enzyme)
mmNO <- gamlssMX(act~1, family=NO, K=2, data=enzyme)
mmNO
\donttest{
# also to make sure that it reaches the maximum
mmNOs <- gamlssMXfits(n=10, act~1, family=NO, K=2, data=enzyme)
fyNO<-dMX(y=seq(0,3,.01), mu=list(1.253, 0.1876), sigma=list(exp(-0.6665 ), exp(-2.573 )),
                  pi=list(0.4079609, 0.5920391 ), family=list("NO","NO") )
hist(enzyme$act,freq=FALSE,ylim=c(0,3.5),xlim=c(0,3),br=21)
lines(seq(0,3,.01),fyNO, col="red")
# equivalent model using gamlssNP
mmNP <- gamlssNP(act~1, data=enzyme, random=~1,sigma.fo=~MASS,family=NO, K=2)
}
}
