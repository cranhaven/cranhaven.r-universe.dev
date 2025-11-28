\name{gamlss.ggplots-package}
\alias{gamlss.ggplots-package}
\alias{gamlss.ggplots}
\docType{package}
\title{
\packageTitle{gamlss.ggplots}
}
\description{
\packageDescription{gamlss.ggplots}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{gamlss.ggplots}
\packageIndices{gamlss.ggplots}
The following convention has been used to name the functions: 

\code{fitted_NAME}: plots concerning  fitted values from a single fitted model

\code{resid_NAME}: plots concerning  residuals  from a single fitted model

\code{predict_NAME}: plots concerning prediction values from a single fitted  model usually having \code{newdata} option.

\code{model_NAME}: plots concerning  different  fitted models

where \code{NAME} refer to different characteristics.
}
\author{
\packageAuthor{gamlss.ggplots}

Maintainer: \packageMaintainer{gamlss.ggplots}
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}.
	An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.


Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}


Stasinopoulos, M. D., Rigby, R. A., and De Bastiani F., (2018) GAMLSS: a distributional regression approach, \emph{Statistical Modelling}, Vol. \bold{18}, 
pp, 248-273, SAGE Publications Sage India: New Delhi, India. 

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.



(see also  \url{https://www.gamlss.com/}).
}

\keyword{ package }
\seealso{\code{\link[gamlss]{gamlss}}, \code{\link[gamlss.dist]{gamlss.family}}}  
\examples{
library(gamlss)
m1 <- gamlss(y~pb(x), data=abdom)
resid_index(m1)
}
