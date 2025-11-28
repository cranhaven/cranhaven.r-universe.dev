\name{resid_dtop}
\alias{resid_dtop}
\alias{resid_ecdf}
\alias{y_ecdf}

\title{Detrended Transformed Owen's Plot and ECDF for the residuals}
\description{

The function \code{resid_dtop()} provides single de-trended transformed  Owen's plot, Owen (1995), for a GAMLSS fitted objects or any other residual vector (suitable standardised). This is a diagnostic tool for checking whether the normalised quantile residuals are coming from a normal distribution or not. 
This could be true if the horizontal line is within the confidence intervals.

The function \code{resid_ecdf()} provides the empirical cumulative distribution function of the residuals.

The function \code{y_ecdf()} provides the empirical cumulative distribution function of any numerical vector \code{y}.

}
\usage{
resid_dtop(obj, resid, type = c("Owen", "JW"), conf.level = c("95", "99"),
           value = 2, points.col = "steelblue4",
           check_overlap = TRUE,  title, ylim, ...)
           
resid_ecdf(obj, resid, type = c("Owen", "JW"), conf.level = c("95", "99"), 
           value = 2, points.col = "steelblue4", 
           check_overlap = TRUE,  show.outliers = TRUE, title, ...)
           
y_ecdf(y, type = c("Owen", "JW"), conf.level = c("95", "99"), value = 2, 
           points.col = "steelblue4", check_overlap = TRUE, 
           show.outliers = FALSE, from, to, title, ...)           
}
\arguments{
  \item{obj}{A GAMLSS fitted model}
  \item{resid}{if the object is not specified the residual vector can be given here}
   \item{y}{a numeric vector}
  \item{type}{whether to use Owen (1995) or Jager and Wellner (2004) approximate formula}
  \item{conf.level}{ 95\% (default) or 99\% percent confidence interval for the plots}
  \item{value}{cut of point for large residuals}
  \item{points.col}{the colour of the points in the plot}
  \item{check_overlap}{to check for overlap when plotting the observation numbers}
  \item{title}{required title}
   \item{show.outliers}{whether to shoe the number of an outlier obsrvation}
  \item{ylim}{if the y limit should be different from the default max(y)+.1}
   \item{from}{where to start the ecdf}
  \item{to}{where to finish the ecdf}
  \item{\dots}{ further arguments}
}
\value{A ggplot is generated}
\references{
Jager, L. and Wellner, J. A (2004) A new goodness of fit test: the reversed Berk-Jones statistic, University of Washington, Department of Statistics, Technical report 443. 

Owen A. B. (1995) Nonparametric Confidence Bands for a Distribution Function. Journal of the American Statistical Association Vol. 90, No 430, pp. 516-521.

Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), Appl. Statist., 54, part 3, 1-38.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda de Bastiani}

\seealso{\code{\link{resid_wp}}}
\examples{
library(ggplot2)
data(abdom)
a<-gamlss(y~pb(x),sigma.fo=~pb(x,1),family=LO,data=abdom)
resid_dtop(a)
resid_ecdf(a)+ stat_function(fun = pNO, args=list(mu=0, sigma=1)) 
# create a gamma distributed random variable
y <- rGA(1000, mu=3, sigma=1)
gp<- y_ecdf(y)
gp + stat_function(fun = pGA, args=list(mu=3, sigma=1)) 
}
\keyword{regression}
