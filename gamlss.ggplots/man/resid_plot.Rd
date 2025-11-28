\name{resid_index}
\alias{resid_index}
\alias{resid_mu}
\alias{resid_median}
\alias{resid_param}
\alias{resid_plots}
\alias{resid_xvar}
\alias{resid_quantile}

\title{A residual plots}
\description{

The function \code{resid_index()} is plotting the residuals of a GAMLSS fitted model (or any other suitable standardised residual) against the observation number index. 

The function \code{resid_mu()} plots the residuals against fitted values for \code{mu}.  

The function \code{resid_median()} plots the residuals against fitted median values.

The function \code{resid_param()} plots the residuals against any of the GAMLSS fitted parameters, \code{mu}, \code{sigma}, \code{nu}, or \code{tau}.

The function \code{resid_quantile()} plots the residuals against any fitted quantile. 

The function \code{resid_xvar()} plots the residuals against an explanatory term.


The function \code{resid_plots()} produces a plot similar to the one that the function  \code{plot()} produce for a GAMLSS model in package \pkg{gamlss}. This is, four plots:   a) \code{resid_index()}(b) \code{resid_mu()}, (c) \code{resid_density()} and (d) \code{resid_qqplot()}. 

Residuals above (or below) certain specified value are identified. 



}
\usage{
resid_index(obj, resid, plot = TRUE, value = 2, title, annotate = TRUE, 
           no.lines = FALSE)
           
resid_mu(obj, resid, plot = TRUE, value = 2, title, annotate = TRUE)

resid_median(obj, resid, plot = TRUE, value = 3, title, 
            annotate = TRUE)

resid_param(obj, param = c("mu", "sigma", "nu", "tau"), title, 
            line.col = "darkred", point.col = "steelblue4", 
            point.shape = 20)            

resid_quantile(obj, quantile = 0.5, title, newdata, 
            line.col = "darkred", point.col = "steelblue4", 
            point.shape = 20)
            
resid_plots(obj, theme = c("original", "ts", "new", "ecdf"), value = 3)

resid_xvar(obj, xvar, plot = TRUE, value = 2, title, annotate = TRUE)
}
\arguments{
  \item{obj}{a GAMLSS object}
   \item{resid}{or any other suitable standardised residual vector.}
   \item{xvar}{a continuous explanatory variable}
   \item{plot}{whether to plot the result}
   \item{param}{which GAMLSS parameter \code{mu}, \code{sigma}, \code{nu}, or \code{tau}}
   \item{value}{the cut off value for the identification of very large or very small residuals}
  \item{annotate}{whether the threshold annotation should appear or not}
  \item{line.col}{the colour of the line}
  \item{point.col}{the colour of the points}
  \item{point.shape}{the shape of the points}
  \item{title}{a title of the plot if needed}
  \item{theme}{what type of plots should \code{resid_plots()} used : "original" is like using \code{plot.gamlss()},
  "ts" is like using \code{plot.gamlss(,ts="TRUE")} (not implemented yet),
  "new" it uses (a) \code{resid_index()},  (b) \code{resid_density()}, (c) \code{resid_wp()} and (d) 
  \code{resid_dtop().}
  }
  \item{no.lines}{this option allows to hide the horizontal lines so the resulting gg-plot can be used later with say \code{facet_wrap()} see example}  
  \item{newdata}{whether the evaluation should be in newdata or the old data points}
  \item{quantile}{which quantile? default the median (0.50).}
}

\value{
A plot of the residuals is returned.
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


\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}


\seealso{  \code{\link{gamlss}}, \code{\link{plot.gamlss} }}

\examples{
library(ggplot2)
data(rent)
r1<-gamlss(R~pb(Fl)+pb(A)+H+loc,family=GA,data=rent)
resid_index(r1)
resid_mu(r1)
resid_median(r1)
resid_param(r1)
resid_quantile(r1)
resid_xvar(r1, A)
resid_plots(r1)
resid_index(r1, no.lines=TRUE)+facet_wrap(~ cut_number(rent$A, 6))
}

\keyword{regression}
