\name{estWtVariables}
\alias{estWtVariables}
\title{Weighted Estimation}
\description{estWtVariables}
\usage{
    estWtVariables(data, variable.weights,
                        estimation="estVARXls", estimation.args=NULL)
}
\arguments{
    \item{data}{A TSdata object.}
    \item{variable.weights}{weights to use for each output series.}
    \item{estimation}{An estimation method.}
    \item{estimation.args}{An arguments for the estimation method.}
}
\value{A TSestModel.}
\details{
    Weight series so that some series residuals are more 
    important than others. Each output variable is scaled according to 
    variable.weights, estimate is done, and then the estimated model
    unscaled. Estmation is done the method specified by estimate and any
    arguments specified by estimation.args.
    estimation.args should be NULL if no args are needed.
}
\seealso{
    \code{\link{estVARXls}}
    \code{\link{estBlackBox}}
    \code{\link{bft}}
    \code{\link{estMaxLik}}
}
\concept{DSE}
\keyword{ts}


