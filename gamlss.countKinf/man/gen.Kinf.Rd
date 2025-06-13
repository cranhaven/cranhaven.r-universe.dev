\name{gen.Kinf}
\alias{gen.Kinf}

\title{generates a K-inflated distribution from discrete gamlss family}

\description{
The \code{gen.Kinf()} function allows the user to generate \code{d}, \code{p},  \code{q}, and \code{r} K-inflated distribution functions plus an extra K-inflated from \code{gamlss.family} function for fitting a K-inflated  distribution with \code{gamlss}.
}

\usage{
gen.Kinf(family = "NO", kinf=1)
}

\arguments{
\item{family}{a \code{gamlss.family} object, which is used to define the distribution for generating K-inflated model. The distribution families supported by \code{gamlss()} can be found in \code{gamlss.family}.
 }
\item{kinf}{define inflated point in generating K-inflated distribution from discrete \code{gamlss.family}}
}


\value{
The functions \code{gen.Kinf} return \code{d}, \code{p},  \code{q}, and \code{r}
K-inflated distribution functions and K-inflated distribution from discrete \code{gamlss.family}
}

\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion),

\emph{Appl. Statist.},\bold{54}, part 3, pp 507-554.




Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying
 documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).



Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.

\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.



Rigby, R. A. and Stasinopoulos D. M. (2010) The gamlss.family distributions, (distributed with this package or see
\url{http://www.gamlss.org/})



Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},
Chapman and Hall/CRC.

}

\author{
Saeed Mohammadpour <\email{s.mohammadpour1111@gamlil.com}>, Mikis Stasinopoulos <\email{d.stasinopoulos@londonmet.ac.uk}>
}


\examples{
# generate one inflated Negative Binomial distribution
gen.Kinf(family =NBI, kinf=1)

# generate one inflated Delaporte distribution
gen.Kinf(family =DEL, kinf=1)

# generate one inflated Sichel distribution
gen.Kinf(family =SICHEL, kinf=1)

}

\keyword{distribution}




