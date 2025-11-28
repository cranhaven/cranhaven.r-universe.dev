\name{CVs}
\alias{CVs}
\title{Coefficients of variation}
\description{Coefficients of variation}

\usage{CVs(sample, na.rm=TRUE)}

\arguments{
\item{sample}{Numerical vector}
\item{na.rm}{Remove NAs?}
}

\details{
Coefficients of variation: different variants of the standardized range
}

\value{Named numerical vector}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
sapply(trees, CVs)
}

\keyword{univar}
