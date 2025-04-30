\name{plotc}
\Rdversion{1.1}
\alias{plotc}

\title{
Plot one or two time series
}

\description{
Plot one or two time series
}

\usage{
plotc(y1, y2 = NULL)
}

\arguments{
\item{y1}{Data vector (plotted in blue with knots)}
\item{y2}{Data vector (plotted in red, no knots)}
}

\value{
None
}

\examples{
plotc(uspop)
y = trend(uspop,2)
plotc(uspop,y)
}
