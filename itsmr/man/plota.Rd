\name{plota}
\Rdversion{1.1}
\alias{plota}

\title{
Plot data and/or model ACF and PACF
}

\description{
Plot data and/or model ACF and PACF
}

\usage{
plota(u, v = NULL, h = 40)
}

\arguments{
\item{u,v}{Data and/or ARMA model in either order}
\item{h}{Maximum lag}
}

\value{
None
}

\examples{
plota(Sunspots)
a = yw(Sunspots,2)
plota(Sunspots,a)
}
