\name{sampleData}
\alias{sampleData}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~'
Simulated Data Set for MOGA-VS
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A simulated data set with 100 observations, 1 dependent variable and 60 independent variables. 
}
\usage{data("sampleData")}

\details{
%%  ~~ If necessary, more details than the __description__ above ~~
The data frame variable \code{y} includes the dependent variables, while the \code{x1} to \code{x60} refer to independent variables.
}
\examples{
data(sampleData)
ans <- mogavs(as.matrix(sampleData)[,-1],as.matrix(sampleData)[,1],maxGenerations=10)
}
\keyword{datasets}
