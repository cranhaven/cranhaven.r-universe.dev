\name{retn}
\docType{data}
\alias{retn}
\title{
Roman Empire transport network and Mediterranean region
}
\description{
This is a list of lists with specifications to plot different cartographical maps of the Roman Empire and 
the Mediterranean with transport network including settlements, roads, and shipping routes. 
}
\usage{
data("retn")
}
\format{
A list of lists object with the shape data in different slots for 4 cartographical maps of the Roman Empire 
with names \code{rcoast} for a plain map, \code{rpcoast} for a map with provinces, \code{rpsi} for a map 
with senatorial and imperial provinces, and \code{rptetra} for a tetrarchy map. 
These options for cartographical maps in the Mediterranean are for both the classical and the late antiquity periods. 

Three components in \code{retn} dataset have coordinates for settlements \code{nds}, roads \code{rds}, and shipping 
routes \code{srs} for these maps. 
In addition, the dataset has a cartographical map of the Mediterranean in \code{med} where settlements 
and transport network is yet to complete.
}
\source{
DARMC, Center for Geographic Analysis, Harvard University

Rodrigue, Comtois, Slack. \emph{The geography of transport systems}. Routledge (2013)

https://commons.wikimedia.org/wiki/File:RomanEmpire_117.svg

https://commons.wikimedia.org/wiki/File:Roman_provinces_trajan.svg

https://commons.wikimedia.org/wiki/File:Regioni_dell'Italia_Augustea.svg
}
%\references{
%}
\seealso{
\code{\link{plot.map}}, \code{\link{rp}}
}
\keyword{datasets}
