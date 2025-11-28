\name{ACE}
\alias{ACE}
\alias{mcor}

\title{
Alternating Conditional Expectations
}
\description{
The function \code{ACE()} uses the alternating conditional expectations algorithm to find a transformations of \code{y} and \code{x} that maximise the proportion of variation in y explained by x. It is a less general function  than the \code{ace()} function  of the package `acepack` in that it takes only one explanatory variable. The function \code{ACE()} is used  by the function \code{mcor()}  to calculate the maximal correlation between \code{x} and \code{y}.
}
\usage{
ACE(x, y, weights, data = NULL, con_crit = 0.001, 
    fit.method = c("loess", "P-splines"), nseg = 10, 
    max.df = 6, ...)
    
mcor(x, y, data = NULL,  fit.method = c("loess", "P-splines"),  
        nseg = 10, max.df = 6,  ...)

}
\arguments{
  \item{x}{the unique x-variables}
  \item{y}{the y-variable}
  \item{weights}{prior weights}
  \item{data}{a data frame for y, x and weights}
  \item{con_crit}{the convergence criterio of the algorithm}
  \item{fit.method}{the method use to fit the smooth functions $t_1()$ and $t_2()$}
  \item{nseg}{the number of knots}
  \item{max.df}{the maximum od df allowed}
  \item{\dots}{arguments to pass to the fitted functions \code{fir_PB} or \code{loess()}}
}
\details{
The function \code{ACE} is a simplified version of the function \code{ace()} of the package \pkg{agepack}.
}
\value{
A fitted \code{ACE} model with methods \code{print.ACE()} and \code{plot.ACE() }
}
\references{
Eilers, P. H. C. and Marx, B. D. (1996). Flexible smoothing with B-splines and penalties (with comments and rejoinder). \emph{Statist. Sci}, \bold{11}, 89-121.

Rigby, R. A. and  Stasinopoulos D. M.(2005). Generalized additive models for location, scale and shape, (with discussion),\emph{Appl. Statist.}, 
\bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos
}

\seealso{
fit_PB
}
\examples{
data(rent)
ACE(Fl, R, data=rent)
pp <- ACE(Fl, R, data=rent)
pp
plot(pp)
mcor(Fl, R, data=rent)
}

\keyword{regression}
