\name{croatiadb}
\alias{croatiadb}
\docType{data}
\title{
principal coordinates associated with data climatic stations in Croatia 2008. 
}
\description{
data frame \eqn{1752 \times 14}{1752 x 14} of spatio-temporal coordinates, earth's average temperature monthly and 10 principal coordinates associated with data climatic stations in Croatia 2008. 
}
\usage{data(croatiadb)}
\format{
  The format is:
Formal class 'data.frame' [package "base"] 
}
\references{
Hengl, T. (2009). \emph{A Practical Guide to Geostatistical Mapping}, 2nd edn, University of Amsterdam, Amsterdam.

Hengl, T., Heuvelink Gerard, B. M., Percec Tadic, M. & Pebesma, E. J. (2012).
\emph{Spatio-temporal prediction of daily temperatures using time-series of
MODIS LST images}, Theoretical and Applied Climatology 107, 1-2, 265-277.

Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]} 
}
\seealso{
\code{\link{croatia.grid7cp}}, \code{\link{croatia.temp}}
}
\examples{
data(croatiadb)
str(croatiadb)
names(croatiadb)
}
\keyword{datasets}
