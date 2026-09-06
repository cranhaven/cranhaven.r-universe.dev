\name{NP.control}
\alias{NP.control}
\title{Control function for gamlssNP}
\description{
  This is a control function for \code{gamlssNP} function.
}
\usage{
NP.control(EMcc = 0.001, EMn.cyc = 200, damp = TRUE, 
           trace = TRUE, plot.opt = 3, ...)
}

\arguments{
  \item{EMcc}{convergence criterion for the EM}
  \item{EMn.cyc}{number of cycles for the EM}
  \item{damp}{Not in used}
  \item{trace}{whether to print the EM iterations}
  \item{plot.opt}{plotting the }
  \item{\dots}{for extra arguments}
}
\value{
  Returns a list.
}
\references{ Einbeck, J. Darnell R. and  Hinde J. (2006) npmlreg: Nonparametric maximum likelihood estimation for random effect
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
\author{Mikis Stasinopoulos}
\seealso{\code{\link[gamlss]{gamlss}}, \code{\link{gamlssNP}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}

