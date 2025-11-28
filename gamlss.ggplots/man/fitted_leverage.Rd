\name{fitted_leverage}
\alias{fitted_leverage}

\title{
Plot of the linear leverage of a GAMLSS model
}
\description{
This is plot of the "linear" leverage of a GAMLSS fitted model.
By linear we mean the leverage (hat-values) we would have obtain in all the explanatory variables for all distribution parameters where put together and used to fit a linear model to the response. The "linear" leverage is them the  hat-values obtained by fitting this simple linear model. Hopefully the "linear" leverage can indicate observations with extreme values in the x's. Note that observations with hight linear leverage  may not be influential in the GAMLSS fitting especially if the x-variables are fitted using smoothers.    
}
\usage{
fitted_leverage(obj, plot = TRUE, title, quan.val = 0.99, 
          annotate = TRUE, line.col = "steelblue4", 
          point.col = "steelblue4", annot.col = "darkred")
}
\arguments{
  \item{obj}{A GAMLSS fitted model}
  \item{plot}{whether to plot ot not}
  \item{title}{for different title than the default}
  \item{quan.val}{which quantile value of the leverage should be taked to indicate the observation values}
  \item{annotate}{whether to annotate the extreme levarages}
  \item{line.col}{the colour of the lines} 
  \item{point.col}{the colout of the points}
  \item{annot.col}{the colour used for annotation}
}

\value{
Returns a plot of the linear leverage against index.  
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

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

\seealso{ \code{\link[gamlss]{gamlss}} }

\examples{
m1 <- gamlss(R~pb(Fl)+pb(A)+loc+H, data=rent, family=GA)
fitted_leverage(m1)
}

\keyword{regression}
