\name{residualPlot}
\alias{residualPlot}
\title{Plot for diagnostics of the residuals in a distributed-lag structural equation model.}
\description{Several different types of plot for diagnostics of the residuals in a distributed-lag structural equation model are displayed.}
\usage{residualPlot(x, type = "fr")}
\arguments{
  \item{x}{An object of class \code{dlsem}.}
  \item{type}{A character string indicating the type of plot, which should be one among
  'fr' (fitted vs. residuals plot, the default),
  'qq' (quantile-quantile plot of the residuals),
  'ts' (time series plot of the residuals),
  'ac' (auto-correlation plot of the residuals).}
}
%\value{}
\note{If \code{type} is equal to 'ts' or 'ac' and a group factor was specified for \code{x}, the results are displayed as minimum, 1st quartile, median, 3rd quartile and maximum by group.
}
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
residualPlot(indus.mod,type="fr")
residualPlot(indus.mod,type="qq")
residualPlot(indus.mod,type="ts")
residualPlot(indus.mod,type="ac")
}
