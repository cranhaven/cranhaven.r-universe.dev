\name{as.graphNEL}
\alias{as.graphNEL}
\title{Conversion into the \code{graphNEL} class}
\description{An object of class \code{dlsem} is converted into an object of class \code{graphNEL}.}
\usage{as.graphNEL(x, conf = 0.95, use.ns = FALSE)}
\arguments{
  \item{x}{An object of class \code{dlsem}.}
  \item{conf}{The confidence level for each edge: only edges with statistically significant causal effect at such confidence are considered. Default is 0.95.}
  \item{use.ns}{A logical value indicating whether edges without statistically significant causal effect (at level \code{conf}) should be considered or not. If \code{FALSE} (the default), they will be ignored.}
}
\value{An object of class \code{graphNEL}.}
%\note{}
%\references{}
\seealso{\link{dlsem}.}
\examples{
data(industry)
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",exogenous=c("Population","GDP"),data=industry,
  log=TRUE)
as.graphNEL(indus.mod)
}
