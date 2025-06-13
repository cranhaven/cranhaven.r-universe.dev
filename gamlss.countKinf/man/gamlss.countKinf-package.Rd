\name{gamlss.countKinf-package}
\alias{gamlss.countKinf-package}
\alias{gamlss.countKinf}
\docType{package}
\title{Generating and Fitting K-Inflated 'discrete gamlss.family' Distributions}

\description{
The main purpose of this package is to allow the user of the GAMLSS models to fit K-inflated discrete distributions.
}

\details{
\tabular{ll}{
Package: \tab gamlss.countKinf\cr
Type: \tab Package\cr
Version: \tab 3.5.1\cr
Date: \tab 2018-11-2\cr
}
The user can generates K-inflated distrinutions from discrete \code{gamlss.family} for fitting gamlss model.
}

\author{
Saeed Mohammadpour <\email{s.mohammadpour1111@gamlil.com}>, Mikis Stasinopoulos <\email{d.stasinopoulos@londonmet.ac.uk}>

Maintainer: Saeed Mohammadpour <\email{s.mohammadpour1111@gamlil.com}>

}

\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion),
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).
}


\examples{
# generating one inflated distribution from SICHEL model
gen.Kinf(family=SICHEL, kinf=1)

# generating two inflated distribution from Delaporte model
gen.Kinf(family=DEL, kinf=1)
}

\keyword{ package }
\keyword{ distribution }
\keyword{ regression }
