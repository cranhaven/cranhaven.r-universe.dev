\name{summary.analyseISE}
\alias{summary.analyseISE}
\title{Summary of estimates for ISE parameter values and experimental sample concentrations.}
\usage{
\method{summary}{analyseISE}(object, ...)
}
\arguments{
\item{object}{Data set of class ISEdata}

\item{...}{Other objects passed through.}
}
\value{
tables: Two tables (table1 and table2) are returned as a list.

\item{table1}{A table of ISE parameter values (see summary.describeISE for details)}

\item{table2}{A table of estimated analyte concentrations for experimental samples}
}
\description{
summary.analyseISE takes an object of class analyseISE and produces summary tables.
}
\seealso{
\code{\link{analyseISE}} \code{\link{summary.ISEdescription}}
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}
