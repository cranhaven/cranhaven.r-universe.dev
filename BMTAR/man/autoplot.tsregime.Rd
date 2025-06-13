\name{autoplot.tsregime}
\alias{autoplot.tsregime}

\title{tsregime object ggplot for the outputs on the function tsregime}

\description{
Produces a ggplot object for the results of the tsregime function.
}
\usage{
\method{autoplot}{tsregime}(object, type = 1,...)
}
\arguments{
\item{object}{Object of class \dQuote{\code{tsregime}}.}
\item{type}{character string giving the type of plot to be computed. Allowed
values are 1 for "\code{Output process}" (the default), 2 for \dQuote{\code{Threshold process}}, 3 for \dQuote{\code{Covariate process}}
}
\item{...}{other arguments passed to specific methods}
}
\details{
Graph for the stochastic processes of the object \ dQuote {\ code {tsregime}} for: Output process and if they exist for: Threshold process and Covariates process. In the case that there are missing data, a red line is drawn at the indicated time.
}
\value{
  Return a ggplot object.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{tsregime}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim")
yt = datasim$Sim
Yt = yt$Yt
Zt = yt$Zt
data = tsregime(Yt,Zt)
autoplot.tsregime(data)
}
