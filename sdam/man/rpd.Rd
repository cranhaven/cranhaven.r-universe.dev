\name{rpd}
\docType{data}
\alias{rpd}
\title{
Roman provinces dates from EDH dataset
}
\description{
Dataset with a list with Roman province dates from the Epigraphic Database Heidelberg as in \code{EDH} dataset.
}
\usage{
data("rpd")
}
\format{
A list object of 66 Roman provinces with dates for inscriptions. 
Each list component has a vector for the province containing following dating data: 
average date, min TAQ, max TPQ, and the average length timespan. 
Components in the list have also an attribute class with the \code{HD_nr} entries of the province in \code{EDH} 
dataset where timespans, TAQ and TPQ are from entries \code{not_before} and \code{not_after}, respectively.
}
\source{
https://edh-www.adw.uni-heidelberg.de/data/api/
}
%\references{
%}
\seealso{
\code{\link{rmids}}, \code{\link{edhwpd}}, \code{EDH}, \code{\link{get.edh}}, \code{\link{rp}}
}
\keyword{datasets}
