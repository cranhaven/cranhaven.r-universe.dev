\name{Ell}
\alias{Ell}
\title{Plot ellipse}
\description{Plot ellipse}

\usage{Ell(x, y, width, height=width, theta=2*pi, npoints=100, plot=TRUE, ...)}

\arguments{
\item{x}{x coordinate of center}
\item{y}{y coordinate of center}
\item{width}{length of major axis}
\item{height}{length of minor axis}
\item{theta}{rotation}
\item{npoints}{number of points to send to polygon}
\item{plot}{if TRUE, add to current device, if FALSE, returns list of components}
\item{...}{arguments to 'polygon()'}
}

\details{Plots ellipse based on 'polygon()'.}

\value{If plot=FALSE, returns list of components.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
plot(1:8, type="n")
Ell(4, 5, 6)
}

\keyword{aplot}

