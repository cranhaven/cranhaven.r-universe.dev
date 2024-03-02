\name{reachability}
\alias{reachability}
\alias{reachability.ARMA}
\alias{reachability.SS}
\alias{reachability.TSestModel}

\title{Calculate Model Reachability Matrix}
\description{
    Calculate the singular values of the reachability matrix of a model.
}
\usage{
    reachability(model)
    \method{reachability}{ARMA}(model)
    \method{reachability}{SS}(model)
    \method{reachability}{TSestModel}(model)
}
\arguments{
    \item{model}{An object containing TSmodel.}}
\value{The singular values of the reachability matrix.}
\details{

If all singular values are significantly different from zero the
model is controllable.
}
\seealso{
\code{\link{observability}},
\code{\link{stability}}
\code{\link{roots}}
\code{\link{McMillanDegree}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- toSS(estVARXls(eg1.DSE.data.diff))
reachability(model)
}
\concept{DSE}
\keyword{ts}

