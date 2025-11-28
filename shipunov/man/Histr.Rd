\name{Histr}

\alias{Histr}

\title{Histogram with overlaid curve}

\description{Histogram with overlaid normal curve or density, optionally with rug}

\usage{Histr(x, overlay="normal", rug=FALSE, col="gray80", ...)}

\arguments{
\item{x}{numerical vector}
\item{overlay}{type of curve to overlay, accepted values are "normal" and "density"}
\item{rug}{if TRUE, will add rug plot}
\item{col}{curve color}
\item{...}{arguments to 'hist()'}
}

\details{Histr() plots histogram with overlaid normal curve or density, optionally with rug.
Based on analogous function from Stephen Turner's 'Tmisc' package.}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{hist}}, \code{\link{density}}, \code{\link{rnorm}}}

\examples{
x <- rnorm(1000, mean=5, sd=2)
Histr(x)
Histr(x, overlay="density")
Histr(x^2, overlay="density", rug=TRUE, breaks=50, col="lightblue2")
}

\keyword{hplot}
