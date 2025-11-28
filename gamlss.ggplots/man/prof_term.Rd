\name{prof_term}
\alias{prof_term}

\title{Plotting the profile deviance of one fitted term}
\description{
Plotting the profile deviance of a fitted term in GAMLSS
}
\usage{
prof_term(model = NULL, criterion = c("GD", "GAIC"), 
      penalty = 2.5,  from = NULL, to = NULL, 
      step = NULL, length = 7, xlabel = NULL, plot = TRUE, 
      perc = 95, start.prev = TRUE, line.col = "darkgreen", 
      dash.line.type = 3, dash.line.size = 0.8, text.size = 5, title)
}
\arguments{
  \item{model}{a GAMLSS fitted model}
  \item{criterion}{whether Global deviance or GAIC}
  \item{penalty}{the penalty k for GAIC}
  \item{from}{start from}
  \item{to}{finish at}
  \item{step}{using step}
  \item{length}{if the \code{step} is left NULL then \code{length} is considered for evaluating the grid for the parameter. It has a default value of 11.}
  \item{xlabel}{if a x label is required}
  \item{plot}{whether tto plot the function}
  \item{perc}{what percentage confidence interval is required}
  \item{start.prev}{whether to start from the previous fitted model parameters
  values or not (default is TRUE)}
  \item{line.col}{the colour of the plotting line}
  \item{dash.line.type}{the type of verical dash line for CI's}
  \item{dash.line.size}{The size of the dash lines}
  \item{text.size}{the size of text}
  \item{title}{the title}
}
\details{
This function is the \code{ggplot2} version of the original \code{prof.term()} function.
}
\value{
creates a  plot
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
Mikis Stasinopoulos
}

\seealso{
\code{\link[gamlss]{prof.term}}
}
\examples{
data(aids)
# fitting a linear model
gamlss(y~x+qrt,family=NBI,data=aids)
# testing the linear beta parameter
mod<-quote(gamlss(y ~ offset(this * x) + qrt, data = aids, family = NBI))
prof_term(mod, from = .06, to=0.13)
}

\keyword{regression}
