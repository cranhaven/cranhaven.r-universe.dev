\name{croatia.grid7cp}
\alias{croatia.grid7cp}
\docType{data}
\title{
Principal coordinates of a pixelated size 4994 in Croatia.
}
\description{
data frame \eqn{4994 \times 13}{4994 x 13} of spatio-temporal coordinates and principal coordinates associated with a pixelated size 4994 in Croatia. Spatial reference system: UTM 33N.
}
\usage{data(croatia.grid7cp)}

\references{  
Hengl, T. (2009). \emph{A Practical Guide to Geostatistical Mapping}, 2nd edn, University of Amsterdam, Amsterdam.

Hengl, T., Heuvelink Gerard, B. M., Percec Tadic, M. & Pebesma, E. J. (2012).
\emph{Spatio-temporal prediction of daily temperatures using time-series of
MODIS LST images}, Theoretical and Applied Climatology 107, 1-2, 265-277.

Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]}
}
\seealso{
\code{\link{croatia.temp}}
}

\examples{
data(croatia.grid7cp)
plot(croatia.grid7cp[,1:2])
}

\keyword{datasets}
