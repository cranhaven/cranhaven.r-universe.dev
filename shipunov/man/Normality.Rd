\name{Normality}
\alias{Normality}

\title{Check normality}
\description{Check normality through Shapiro-Wilks test}

\usage{Normality(x, p=0.05)}

\arguments{
\item{x}{numerical vector}
\item{p}{level of significance}
}

\details{Normality via Shapiro-Wilks test. Kolmogorov-Smirnov is apparently too weak for small samples. The word of caution: this function only \emph{helps} to decide if the data complains with parametric methods ("normal"). }

\value{Character vector of length one.}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{qqnorm}}, \code{\link{hist}}, \code{\link{rnorm}}}

\examples{
Normality(rnorm(100))
sapply(trees, Normality)
}

\keyword{htest}

