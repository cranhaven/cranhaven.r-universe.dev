\name{summary.ISEdata}
\alias{summary.ISEdata}
\title{Summarises ISE data}
\usage{
\method{summary}{ISEdata}(object, ...)
}
\arguments{
\item{object}{Data set of class ISEdata}

\item{...}{Other objects passed through.}
}
\value{
metadata: Metadata for the ISEs, a list with N, R, calibration.only, M, and stdadd

\item{N}{Total number of calibration observations}

\item{R}{Number of ISEs}

\item{calibration.only}{Indicates calibration only data (T), or calibration and experimental data (F)}

\item{M}{Number of experimental samples (NA if no experimental data were loaded)}

\item{stdadd}{Indicates whether standard addition used for experimental samples (T) or the basic model was used (F), or no experimental data (NA)}
}
\description{
summary.ISE takes an object of class ISEdata (e.g. see loadISEdata) and produces metadata for it.
}
\examples{
data(LeadStdAdd)
summary(LeadStdAdd)	
}
\seealso{
\code{\link{loadISEdata}}
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}
