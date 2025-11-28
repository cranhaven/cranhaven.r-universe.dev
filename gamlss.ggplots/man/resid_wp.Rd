\name{resid_wp}
\alias{model_wp}
\alias{resid_wp}
\alias{resid_wp_wrap}
\alias{model_wp_wrap}

\title{Worm plot using ggplot2}
\description{The function produces worm plot of the residuals of a fitted  model.  A worm plot is a de-trended normal QQ-plot so departure from normality is highlighted.

The function \code{plot_wp()} it is similar to the \pkg{gamlss} package function \code{wp()} when the argument \code{xvar} is not used.} 
\usage{
resid_wp(obj, resid, value = 3, points_col = "steelblue4", 
         poly_col = "darkred", 
         check_overlap = TRUE, title, ylim)

model_wp(obj, ..., title)  

resid_wp_wrap(obj, resid, value = 3, xvar = NULL, n_inter = 4, 
         points_col = "steelblue4", poly_col = "darkred", 
         alpha_bound = 0.1, check_overlap = TRUE, title, ylim)
         
model_wp_wrap(obj, ..., xvar = NULL, value = 3, n_inter = 4, 
         points_col = "steelblue4", alpha_bound = 0.1, 
         check_overlap = TRUE, ylim, title)         
}
\arguments{
  \item{obj}{
a GAMLSS fitted object or any other fitted model where the \code{resid()} method works (preferably the residuals should be standardised or better normalised quantile residuals. Note for \code{model_wp} only gamlss object are accepted.)
}
  \item{resid}{
if object is missing this argument can be used to specify the residual vector (again it should a normalised quantile residual vector)
}
  \item{value}{A cut off point to indicate large residuals, default is \code{value=3}}
  \item{xvar}{the x term  for which the worm plot will be plotted against}
  \item{n_inter}{the number of intervals for continuous x-term}
  \item{points_col}{the color of the points in the plot}
  \item{poly_col}{the colour of the fitted polynomial in the plot}
  \item{check_overlap}{to check for overlap  when plotting the observation numbers 
}
 \item{alpha_bound}{the transparency parameter for the coinfidence bound}  
  \item{title}{required title}
  \item{ylim}{if the y limit should be different from the default max(y)+.1}
   \item{\dots}{extra GAMLSS models}
}

\value{
A worm plot is produced
}
\references{
van Buuren and Fredriks M. (2001) Worm plot: simple diagnostic device for modelling growth reference curves. 
            \emph{Statistics in Medicine}, \bold{20}, 1259--1277

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
\code{\link[gamlss]{wp}}
}
\examples{
data(abdom)
# with data
a<-gamlss(y~pb(x),sigma.fo=~pb(x,1),family=LO,data=abdom)
resid_wp(a)
resid_wp(resid=resid(a))
}

\keyword{regression}
