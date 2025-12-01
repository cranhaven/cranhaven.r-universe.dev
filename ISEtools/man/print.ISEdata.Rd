\name{print.ISEdata}
\alias{print.ISEdata}
\title{Prints ISE data}
\usage{
\method{print}{ISEdata}(x, ...)
}
\arguments{
\item{x}{ISE data (e.g. object of class ISEdata)}

\item{...}{Other objects passed through.}
}
\value{No return value, prints ISE data.}
\description{
Prints tables of calibration data and experimental data (if present).
}
\examples{
data(LeadStdAdd)
print(LeadStdAdd)
}
\seealso{
\code{\link{loadISEdata}}
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}
