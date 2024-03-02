\name{estBlackBox}
\alias{estBlackBox}
\title{Estimate a TSmodel}
\description{Estimate a TSmodel.}
\usage{
    estBlackBox(data,...)
}
\arguments{
    \item{data}{Data in an object of class TSdata.}
    \item{...}{
    Optional arguments depent on the function which is eventually called.}
}
\value{A state space model in an object of class TSestModel.}
\details{
The function makes a call to \code{estBlackBox4}, also called \code{bft}, 
which seems the most reliable of functions
\code{estBlackBox1}, \code{estBlackBox2}, \code{estBlackBox4}.
My research in this area is no longer active, but the actual routine called
could change if better methods appear.

To reduce load on the CRAN check servers, the example below uses 
\code{max.lag=3} rather than the default \code{12} used in Gilbert (1995). 
}
\examples{
data("egJofF.1dec93.data", package="dse")
goodmodel <- estBlackBox(egJofF.1dec93.data, max.lag=3)
}
\references{
    Gilbert, P. D. (1995) Combining VAR Estimation and State Space 
    Model Reduction for Simple Good Predictions. \emph{J. of Forecasting: 
    Special Issue on VAR Modelling}, \bold{14}, 229-250.
}
\concept{DSE}
\keyword{ts}

