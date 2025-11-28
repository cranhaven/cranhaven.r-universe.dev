\name{pcr_coef_path}
\alias{pcr_coef_path}
\alias{pcr_path}
\title{
Plotting the fitted path of a PCR  model.
}
\description{
This function is similar to the function \code{plot.PCR()} which is used to plot the path of a fitted principal componet regression model, fitted using the function \code{fitPCR()} of the package \pkg{gamlss.foreach}.
}
\usage{
pcr_coef_path(x, legend=FALSE, plot=TRUE)

pcr_path(x, parameter = c("mu", "sigma", "nu", "tau"),
          legend = FALSE, plot = TRUE)

}
\arguments{
  \item{x}{a fitted PCR object (or a fitted GAMLSS object for function \code{pcr_path(}}
  \item{legend}{whether legent is needed}
  \item{plot}{whether to plot the path}
  \item{parameter}{which GAMLSS parameter, between "mu", "sigma", "nu", "tau"}
}
\value{A gg-plot.}
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

\seealso{
\code{\link[gamlss.foreach]{plot.PCR}}
}
\examples{
\donttest{
library(gamlss.foreach)
library(glmnet)
library(ggplot2)
data(QuickStartExample)
attach(QuickStartExample)
hist(y, main="(a)")
if (is.null(rownames(x))) 
  colnames(x) <- paste0("X", seq(1:dim(x)[2]))
# fitting
MM<- fitPCR(x,y, k=log(100))
pp<-pcr_coef_path(MM)
pp+ ggplot2::geom_vline(xintercept = MM$pc, colour = "gray") 
# using gamlss
m1 <- gamlss(y~pcr(x=x))
pcr_path(m1)
}
}

\keyword{regression}
