\name{Cladd}
\alias{Cladd}
\title{Adds confidence bands to the simple linear model plots}
\description{Adds confidence bands to the simple linear model plots}

\usage{Cladd(model, data, level=.95, lty=2, ab.lty=0, col="black", ab.col="black")}

\arguments{
\item{model}{Simple linear model name}
\item{data}{Original data}
\item{level}{Confidence level}
\item{lty}{Confidence bands line type}
\item{ab.lty}{Regression line type}
\item{col}{Confidence bands line color}
\item{ab.col}{Regression line color}
}

\details{'Cladd()' adds confidence bands to the simple linear model plots.
Works only for simple lm(y ~ x) objects!
}

% \value{}

%\references{}

\author{Alexey Shipunov}

\seealso{\code{\link{lm}}}

\examples{
hg.lm <- lm(Height ~ Girth, data=trees)
plot(Height ~ Girth, data=trees)
Cladd(hg.lm, data=trees, ab.lty=1)
}

\keyword{aplot}
