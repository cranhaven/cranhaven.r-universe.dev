\name{auto.lagPlot}
\alias{auto.lagPlot}
\title{Automated plot of lag shapes}
\description{All the single-edge pathwise causal lag shapes are saved as pdf files.}
\usage{auto.lagPlot(x, cumul = FALSE, conf = 0.95, plotDir = NULL)}
\arguments{
  \item{x}{An object of class \code{dlsem}.}
  \item{cumul}{Logical. If \code{TRUE}, cumulative causal effects are displayed. Default is \code{FALSE}.}
  \item{conf}{The confidence level for each plot. Default is 0.95.}
  \item{plotDir}{The directory where to save the plots. If \code{NULL} (the default), plots will be saved in the current working directory.}
}
%\value{}
%\references{}
\seealso{\link{dlsem}; \link{lagPlot}.}
\examples{
data(industry)
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",exogenous=c("Population","GDP"),data=industry,
  log=TRUE)
## NOT RUN:
# auto.lagPlot(indus.mod,plotDir=getwd())
}
