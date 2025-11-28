\name{resp_mu}
\alias{resp_mu}
\alias{resp_param}
\alias{resp_quantile}
\alias{quantile_gamlss}

\title{Plotting the response against  quantities of the fitted model}
\description{
All plots are of  the response variable against  fitted values of interest.

The function \code{resp_mu()} is the `original' one plotting the response against the parameter \code{mu}. The function reports the Pearson's correlation coefficient and plot a lines (45\% degrees) throught the graph.   

The function \code{resp_param()} plots the response against any fitted parameter \code{mu}, \code{sigma}, \code{nu} or  \code{tau}.  The function also plots a smooth curve going throught the data and gives the  Pearson's correlation coefficient.

The function \code{resp_quantile()} plots the response against any fitted quantile, with default the median (0.50). The function also plots a smooth curve going throught the data and gives the  Pearson's correlation coefficient.

The function \code{quantile_gamlss()} is used by the function  \code{resp_quantile()} to calculate the quantiles of the fitted distribution . 

}
\usage{
resp_mu(obj, title, line.col = "darkred", 
           point.col = "steelblue4", 
           point.shape = 20)
           
resp_param(obj, param = c("mu", "sigma", "nu", "tau"), 
           title, line.col = "darkred", point.col = "steelblue4", 
           point.shape = 20)

resp_quantile(obj, quantile = 0.5, title, newdata, 
           line.col = "darkred", point.col = "steelblue4", 
           point.shape = 20)

quantile_gamlss(obj, quantile = 0.5, newdata)
}
\arguments{
  \item{obj}{a GAMLSS fitted object}
   \item{param}{which parameters? \code{mu}, \code{sigma}, \code{nu} or  \code{tau}, [only for \code{resp_param()}].}
    \item{quantile}{which quantile? default the median (0.50), [only for \code{resp_quantile()} and \code{quantile_gamlss()}].}
  \item{title}{a tittle if needed it, by default for the function  \code{fitted_resp} it print the correlation coefficients  between the two variable.}
  \item{line.col}{the colour of the line}
  \item{point.col}{the colour of the points}
  \item{point.shape}{the shape of the points}
   \item{newdata}{whether the evaluation should be in newdata or the old data points [only for functions \code{resp_quantile()} and \code{quantile_gamlss()} ] }
}
\details{
This is standard plot in regression where the fitted values are plotted against the response. In GAMLSS model is done by plotting the response against the fitted values of the \code{mu} model which is most case is a location parameter. 
}
\value{
A plot is returned
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
\author{
Mikis Stasinopulos, Rober Rigby and Fernanda de Bastiani 
}
\note{
Do not use this plot if \code{mu} is not a location parameter. 
}

\seealso{
\code{\link{resid_plots}}
}
\examples{
m1 <- gamlss(R~pb(Fl)+pb(A)+H+loc, data=rent, family=GA )
resp_mu(m1)
resp_param(m1)
resp_quantile(m1)
}

\keyword{regression}
\concept{ GAMLSS}
