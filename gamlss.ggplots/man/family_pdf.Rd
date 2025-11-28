\name{family_pdf}
\alias{family_pdf}
\alias{fitted_pdf}
\alias{fitted_pdf_data}
\alias{predict_pdf}

\title{
Plotting Probabilities Density Functions (pdf's) for GAMLSS
}
\description{
The function \code{family_pdf()} takes a GAMLSS family distribution and plots different pdf's according to the specified parameters.

The function \code{fitted_pdf()} takes a gamlss fitted object and plots the fitted distributions for specified observations.

The function \code{fitted_pdf_data()} it does the same as \code{fitted_pdf()}  but it adds  also the observation values  as grey vertical lines.

The function \code{predict_pdf()} takes a fitted object and test  data and plots the predictive pdf's.

}
\usage{
family_pdf(family = NO(), mu = NULL, sigma = NULL, nu = NULL, tau = NULL, 
         title, from = 0, to = 10, no.points = 201,
         alpha = 0.4, col.fill = hcl.colors(lobs, palette = "viridis"), 
         size.seqment = 1.5, plot.point = TRUE, size.point = 1, 
         plot.line = TRUE, size.line = 0.2, ...)
         
fitted_pdf(model, obs, title, from = 0, to = 10, no.points = 201, alpha = 0.4, 
         col.fill = hcl.colors(lobs, palette = "viridis"), 
         size.seqment = 1.5, plot.point = TRUE, size.point = 1, plot.line = TRUE,
         size.line = 0.2, ...)
         
fitted_pdf_data(model, obs, from, to, ...)         
         
predict_pdf(model, newdata, title, from = 0, to = 10, no.points = 201, 
         alpha = 0.4, col.fill = hcl.colors(lobs, palette = "viridis"), 
         size.seqment = 1.5, plot.point = TRUE, size.point = 1, 
         plot.line = TRUE, size.line = 0.2, ...)
         
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
  \item{size.line}{for discrete responses the size of the joining lines}
  \item{\dots}{for extra argumnets}
}
\details{
The  functions \code{family_pdf()} and \code{fitted_pdf()} are ggplot version of the   function \code{pdf.plot()}  used to plot fitted distributions of  GAMLSS family at specified observation values. Note that the range of the response  has to be specified using the argument \code{from} \code{to}. 

For discrete fitted distributions maybe increase the value of \code{alpha} for clearer  plot.

For binomial type of data (discrete response with upper limit) the function \code{family_pdf()} takes the argument \code{to} as the binomial denominator,
For fitted model with  binomial type responses the function \code{fitted_pdf()}  takes the binomial denominator form the fitted model and set the argument \code{to} to the maximum of those binomial denominators.

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

\seealso{  \code{\link{gamlss}}, \code{\link{resid_density} }}
\examples{
###################################
# function fitted_pdf
# continuous variabe
a1 <- gamlss(y~pb(x),sigma.fo=~pb(x), data=abdom, family=LO)
fitted_pdf(a1, obs=c(10,15,20), from=30, to=100)
# count data
p1 <- gamlss(y~pb(x)+qrt, data=aids, family=NBI)
fitted_pdf(p1, obs=c(10:15), from=25, to=130, alpha=.9)
# binomial type
h<-gamlss(y~ward+loglos+year, sigma.formula=~year+ward, family=BB, data=aep) 
fitted_pdf(h, obs=c(10:15),  alpha=.9)
###################################
# function predict_pdf
predict_pdf(a1, newdata=abdom[10:20, ], from=30, to=100)
# count data
predict_pdf(p1, newdata=aids[10:15, ], from=30, to=150)
# binomial
predict_pdf(h, newdata=aep[10:15, ], from=0, to=20)
###################################
# function family_pdf
# continuous
family_pdf(from=-5,to=5, mu=0, sigma=c(.5,1,2))
# count data 
family_pdf(NBI, to=15, mu=1, sigma=c(.5,1,2), alpha=.9, size.seqment = 3)
# binomial type
family_pdf(BB, to=15, mu=.5, sigma=c(.5,1,2),  alpha=.9, , size.seqment = 3)
}

\keyword{regression}
