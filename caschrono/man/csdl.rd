\name{csdl}
\alias{csdl}
\docType{data}
\title{French stock and returns}
\description{
French stocks (Cac40, SG, Danone, L Oreal) for the period 2006 - 2009, on Euronext Paris.}
\usage{data(csdl)}
\format{This \code{its} object contains the following firms close prices:
\itemize{
\item \code{Cac40}, ^FCHI (name of the quote symbol), common used French stock market index
\item \code{Socgen}, GLE.PA (name of the quote symbol), SG
\item \code{Danone}, BN.PA (name of the quote symbol), Danone
\item \code{L_Oreal}, OR.PA (name of the quote symbol), L Oreal
}
from 2006-01-02 to 2009-06-30.}

\source{http://fr.finance.yahoo.com/}
\examples{
if(require("timeSeries")){
data(csdl)
# we create then the returns
aa <- returns(csdl, percentage = TRUE)
aab <- aa[complete.cases(aa) == TRUE,]
# in previous version we use package its which will not be maintained anymore
# r.csdl = its(aab, as.POSIXct(row.names(aab)))
r.csdl <- zoo(aab, as.POSIXct(row.names(aab)))
}
}
\keyword{datasets}
