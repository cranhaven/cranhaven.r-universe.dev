\name{R.logo}
\alias{R.logo}
\title{Imitation (!) of the modern 'R' logo}
\description{Imitation (!) of the modern 'R' logo}

\usage{R.logo(x, y, col.e="#B8BABF", col.l="#1E63B5", cex=12)}

\arguments{
\item{x}{x coordinate of the letter}
\item{y}{y coordinate of the letter}
\item{col.e}{ellipse color}
\item{col.l}{letter color}
\item{cex}{scale, default 12}
}

\details{Imitation (sic!) of the modern (flat) 'R' logo. Font and proportions are not exactly the same, also there is no gradient.}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Ell}}}

\examples{
plot(1, type="n", axes=FALSE, xlab="", ylab="")
R.logo(1.1, 0.9, cex=25)
##
plot(1:20, type="n")
for (i in 1:20) R.logo(i, i, cex=2)
}

\keyword{aplot}
