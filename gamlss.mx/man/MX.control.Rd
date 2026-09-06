\name{MX.control}
\alias{MX.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The control function for gamlssMX}
\description{
  The function sets controls for the \code{gamlssMX} function.
}
\usage{
MX.control(cc = 1e-04, n.cyc = 200, trace = FALSE, 
        seed = NULL, plot = TRUE, sample = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cc}{convergent criterion for the EM}
  \item{n.cyc}{number of cycles for EM}
  \item{trace}{whether to  print the EM iterations}
  \item{seed}{a number for setting the seeds for starting values}
  \item{plot}{whether to plot the sequence of global deviance up to convergence}
  \item{sample}{how large the sample to be in the starting values}
  \item{\dots}{for extra arguments}
}

\value{
 Returns a list 
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
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

\author{ Mikis Stasinopoulos and Bob Rigby }
\seealso{\code{\link[gamlss]{gamlss}}, \code{\link{gamlssMX}}, \code{\link{gamlssMXfits}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}
