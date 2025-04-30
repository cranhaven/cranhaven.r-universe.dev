\name{mogavs-package}
\alias{mogavs-package}
\alias{mogavs-package}
\docType{package}
\title{
Package for regression variable selection with genetic algorithm MOGA-VS
}
\description{
Runs the genetic algorithm MOGA-VS for variable selection on a given data set.
}
\details{
\tabular{ll}{
Package: \tab mogavs\cr
Type: \tab Package\cr
Version: \tab 1.1.0\cr
Date: \tab 2017-04-11\cr
License: \tab GPL-2\cr
}

}
\author{
Tommi Pajala, Ankur Sinha, Pekka Malo, Timo Kuosmanen
Maintainer: Tommi Pajala <tommi.pajala@aalto.fi>

}
\references{
Sinha, A., Malo, P. & Kuosmanen, T. (2015) A Multi-objective Exploratory Procedure for Regression Model Selection. \emph{Journal of Computational and Graphical Statistics, 24(1). pp. 154-182.}
}
\keyword{ package }
\examples{
data(sampleData)
mod <- mogavs(y~.,data=sampleData,maxGenerations=20)
summary(mod)
createAdditionalPlots(mod,epsilonBand=0,kBest=30,"kbest")
}
