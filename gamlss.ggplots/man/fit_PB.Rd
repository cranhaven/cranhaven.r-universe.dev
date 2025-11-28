\name{fit_PB}
\alias{fit_PB}

\title{
P-spline smoother
}
\description{
The function \code{fit_PB()} fits a P-spline   univariate smoother [Eilers and Marx (1996)] to \code{y} against the \code{x} with prior weights \code{weights}. 
}
\usage{
fit_PB(x, y, weights, data, xmin, xmax, nseg = 20, 
      lambda = 10, order = 2, degree = 3, max.df = 20, 
      ylim, plot = TRUE, col.ribbon = "pink")
}
\arguments{
  \item{x}{the explanatory variable}
  \item{y}{the response}
  \item{weights}{possible prior weights (set to one by default) }
  \item{data}{the data frame  where \code{x}, \code{y} and \code{weights} are coming from
}
  \item{xmin}{the \code{x} minimum if different from \code{min(x)}}
  \item{xmax}{the \code{x} maximum if different from \code{max(x)}

}
  \item{nseg}{the number of knots}
  \item{lambda}{the smotthing parameter}
  \item{order}{the ordr of the difference}
  \item{degree}{the degree of the piewise polynonmial}
  \item{max.df}{the maximum allowed degress of freedom}
  \item{ylim}{the \code{ylim} in the plot}
  \item{plot}{whether to plot the results}
  \item{col.ribbon}{the color in the se of the fitted values}
  
}

\value{
A object \code{Psplines} is produced with methods \code{print()}, \code{coef()}   \code{deviance()},  \code{fitted()},  \code{predict()} and  \code{resid().}
}
\references{
Eilers, P. H. C. and Marx, B. D. (1996). Flexible smoothing with B-splines and penalties (with comments and rejoinder). \emph{Statist. Sci}, \bold{11}, 89-121.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019) 
\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}.
	
Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also  \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos
}
\note{
The function\code{fit_PB()} is an engine for getting the maximal correlation between two continuous variables. It can be also
used on its own as a smoother.
}


\seealso{ACE}
\examples{
data(abdom)
m1 <- fit_PB(x,y, data=abdom)
}

\keyword{regression}
