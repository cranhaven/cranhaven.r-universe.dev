\name{model_pca}
\alias{model_pca}
\title{Plotting residuals using PCA}
\description{
The function \code{model_pca()} plots several GAMLSS residuals using Principal Component Analysis. 
}
\usage{
model_pca(obj, ..., scale = TRUE, arrow_size = 1.5)
}
\arguments{
  \item{obj}{A gamlss object}
  \item{\dots}{for extra GAMLSS models}
  \item{scale}{whether to scale the residuals}
  \item{arrow_size}{the arrow sizw in the biplot}
}
\details{
The function \code{model_pca()}  plot a  \code{biplot()} plot of the residuals from different models.   It uses Principal Component Analysis in the residuals of different models and plots the resuls. 
}
\value{
A biplot of the first two components is plotted.
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


\seealso{  \code{\link{gamlss}}, \code{\link{resid_index} }}
\examples{
m1 <- gamlss(y~x, data=abdom)
m2 <- gamlss(y~pb(x), data=abdom)
m3 <- gamlss(y~pb(x), sigma.fo=~pb(x), data=abdom)
model_pca(m1,m2,m3)
}

\keyword{regression}
