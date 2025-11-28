\name{fitted_cdf}
\alias{fitted_cdf}
\alias{family_cdf}
\alias{predict_cdf}
\alias{fitted_cdf_data}

\title{
Plotting Cumulative Distribution Functions (cdf's) for GAMLSS,
}
\description{
The function \code{family_cdf()} takes a GAMLSS family distribution and plots different pdf's according to the specified parameters.

The function \code{fitted_cdf()} takes a gamlss fitted object and plots the fitted distributions for specified observations.

The function \code{fitted_cdf_data()} is similat to  \code{fitted_cdf()} but also adds the data points as gray vertical lines. 

The function \code{predict_pdf()} (NOT IMPLEMENTED YET) takes a fitted object and test  data and plots the predictive cdf's.
}
\usage{
fitted_cdf(model, obs, title, from = 0, to = 10, no.points = 201, 
          alpha = 1, size.line = 1.2, 
          col.fill = hcl.colors(lobs, palette = "viridis"), 
          size.seqment = 1.5, size.point = 1, 
          plot.line = TRUE, size.line.disc = 0.2, lower.tail = TRUE, ...)

fitted_cdf_data(model, obs, from, to, ...)

predict_cdf(model, newdata, title, from = 0, to = 10, no.points = 201, 
          alpha = 0.4, size.line = 1.2, 
          col.fill = hcl.colors(lobs, palette = "viridis"), 
          size.seqment = 1.5, plot.point = TRUE, size.point = 1, 
          plot.line = TRUE, size.line.disc = 0.2, lower.tail = TRUE, ...)

family_cdf(family = NO(), mu = NULL, sigma = NULL, nu = NULL, 
         tau = NULL, title, from = 0, to = 10, no.points = 201, 
         alpha = 0.4, size.line = 1.2, col.fill = hcl.colors(lobs, 
         palette = "viridis"), size.seqment = 1.5, plot.point = TRUE,  
         size.point = 1, plot.line = TRUE, lower.tail = TRUE, ...)

}
\arguments{
 \item{family}{A GAMLSS family}
 \item{model}{A GAMLSS fitted model}
  \item{obs}{observations to plot fitted distributions}
  \item{newdata}{for test data}
  \item{mu}{the \code{mu} parameter value(s)}
  \item{sigma}{the \code{sigma} parameter value(s)}
  \item{nu}{the \code{nu} parameter value(s)}
  \item{tau}{the \code{tau} parameter value(s)}
  \item{title}{a diferent title for the default}
  \item{from}{minimum value for the response}
  \item{to}{maximum value for the response}
  \item{no.points}{number of points (relevant for continuous responses)}
  \item{alpha}{trasparency factor}
  \item{col.fill}{the colour pallet default is \code{hcl.colors(lobs, palette="viridis")}}
  \item{size.seqment}{for discrete responses the size of the bars}
  \item{plot.point}{for discrete responses whether to put poits on the top of the bars}
  \item{size.point}{for discrete responses }
  \item{plot.line}{for discrete responses whether to joint the bars with lines}
  \item{size.line.disc}{for discrete responses the size of the joining lines}
   \item{size.line}{The size of the lines}
  \item{lower.tail}{if \code{TRUE} cdf is plotted if \code{FALSE} the survival function}
  \item{\dots}{for extra argumnets}
}

\details{
The  functions \code{family_cdf()},  \code{fitted_cdf()}, and \code{predict_cdf()}
are function to plot cdf's for a \code{gamlss.family}, fitted \code{gamlss} model or predictive \code{gamlss} model, respectively. 

For discrete fitted distributions maybe increase the value of \code{alpha} for clearer  plot.

For binomial type of data (discrete response with upper limit) the function \code{family_cdf()} takes the argument \code{to} as the binomial denominator,
For fitted model with  binomial type responses the function \code{fitted_cdf()}  takes the binomial denominator form the fitted model and set the argument \code{to} to the maximum of those binomial denominators.
}
\value{
Creates a plot
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

\seealso{  \code{\link{gamlss}}}
\examples{
# function fitted_cdf
# continuous variabe
a1 <- gamlss(y~pb(x),sigma.fo=~pb(x), data=abdom, family=LO)
fitted_cdf(a1, obs=c(10,15,20), from=30, to=100)
fitted_cdf(a1, obs=c(10,15,20), from=30, to=100, lower.tail=FALSE)
# count data
p1 <- gamlss(y~pb(x)+qrt, data=aids, family=NBI)
fitted_cdf(p1, obs=c(10:15), from=10, to=130, alpha=.9)
fitted_cdf(p1, obs=c(10:15), from=10, to=130, alpha=.9, lower.tail=FALSE)
# binomial type
h<-gamlss(y~ward+loglos+year, sigma.formula=~year+ward, family=BB, data=aep) 
fitted_cdf(h, obs=c(10:15),  alpha=.9)
fitted_cdf(h, obs=c(10:15),  alpha=.9, lower.tail=FALSE)
###################################
# function predict_pdf
predict_cdf(a1, newdata=abdom[c(10,15,20), ], from=30, to=100)
predict_cdf(a1, newdata=abdom[10:20, ], from=30, to=100, lower.tail=FALSE)
# count data
predict_cdf(p1, newdata=aids[10:15, ], from=10, to=150)
predict_cdf(p1, newdata=aids[10:15, ], from=10, to=150, lower.tail=FALSE)
# binomial
predict_cdf(h, newdata=aep[10:15, ], from=0, to=20)
predict_cdf(h, newdata=aep[10:15, ], from=0, to=20, lower.tail=FALSE)
###################################
# function family_cdf
# continuous
family_cdf(from=-5,to=5, mu=0, sigma=c(.5,1,2))
family_cdf(from=-5,to=5, mu=0, sigma=c(.5,1,2), lower.tail=FALSE)
# count data 
family_cdf(NBI, to=15, mu=1, sigma=c(.5,1,2), alpha=.9, size.seqment = 3)
family_cdf(NBI, to=15, mu=1, sigma=c(.5,1,2), alpha=.9, size.seqment = 3, lower.tail=FALSE)
# binomial type
family_cdf(BB, to=15, mu=.5, sigma=c(.5,1,2),  alpha=.9, , size.seqment = 3)
family_cdf(BB, to=15, mu=.5, sigma=c(.5,1,2),  alpha=.9, , size.seqment = 3, lower.tail=FALSE)
}

\keyword{regression}
