\name{resid_symmetry}
\alias{resid_symmetry}
\alias{y_symmetry}

\title{Symmetry plots}
\description{
The functions \code{resid_symmetry()} and \code{y_symmetry()} plot symmtry plots for residuals and single variable, respectively.
}
\usage{
resid_symmetry(model, title)

y_symmetry(y, title)
}
\arguments{
  \item{model}{A model which allows the function \code{resid()}}
   \item{y}{a single variable}
  \item{title}{A title for the plot if needed}
}
\details{
The function orders the data (or residuals) and takes the median minus the lower half and plot it against  the upper half minus the median. 
}
\value{
The symmetry plot is produced.
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}

\seealso{
\code{\link{resid_index}}
}
\examples{
y <- rBCT(1000, mu=3, sigma=.1, nu=-1, tau=5)
y_hist(y)
gg <- y_symmetry(y)
}

\keyword{regression}
