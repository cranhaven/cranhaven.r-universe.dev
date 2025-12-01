\name{plot.ISEdata}
\alias{plot.ISEdata}
\title{Basic plot of ion selective electrode calibration data}
\usage{
\method{plot}{ISEdata}(x, xlab = expression(paste(log[10], " { ", italic(x),
  " }")), ylab = "emf", pch = 20, ...)
}
\arguments{
\item{x}{ISE calibration data}

\item{xlab}{Label for the x-axis}

\item{ylab}{Label for the y-axis}

\item{pch}{Plotting symbol for data}

\item{...}{Other arguments to be passed through to plotting functions.}
}
\description{
Plots raw ISE calibration data; data should follow a hockey stick pattern coinciding with the equation y = a + b log(x + c) + error, where error follows a normal distribution with mean 0 and standard deviation sigma.
}
\value{No return value, creates plot.}
\examples{
data(LeadStdAdd)
plot(LeadStdAdd)
}
\seealso{
\code{\link{loadISEdata}}
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}
