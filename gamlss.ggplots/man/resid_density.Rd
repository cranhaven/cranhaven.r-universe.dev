\name{resid_density}
\alias{resid_density}
\alias{model_density}

\title{Density of the residuals in a GAMLLSS model}
\description{
The function  \code{resid_density()} plots an histogram and a density estimator of the normalised quantile residuals from a fitted GAMLSS model. The function  \code{model_density()} plots  density estimators of the normalised quantile residuals from more than one  fitted GAMLSS models.
}
\usage{
resid_density(obj, resid, hist.col = "black", hist.fill = "white", 
              dens.fill = "#FF6666", title)
model_density(obj, ..., title)              
}
\arguments{
  \item{obj}{
The function needs a  GAMLSS fitted model or 
}
  \item{resid}{any standarised residual }
  \item{hist.col}{The colour of the border of the histogram}
  \item{hist.fill}{The colout of the hisogram}
  \item{dens.fill}{the colour of the desnsity}
  \item{title}{A title if needed}
   \item{\dots}{extra GAMLSS models}
}
\details{
This function \code{resid_density()} is a  denity plot (similar to of the four  plots produded when the  plotting function \code{plot.gamlss()} is used within the  \pkg{gamlss} package.
I uses plotting function from   the \pkg{ggplot2} package.
}
\value{
A density plot of the residuals is produced.
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
\code{\link[gamlss]{plot.gamlss}}
}
\examples{
data(abdom)
a<-gamlss(y~pb(x),family=LO,data=abdom)
b<-gamlss(y~pb(x),family=NO,data=abdom)
resid_density(a)
model_density(a,b)
}

\keyword{regression}
